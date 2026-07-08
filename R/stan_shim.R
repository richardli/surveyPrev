#' rstan replacement for the INLA beta-binomial fit in clusterModel()
#'
#' These internal helpers reproduce, with rstan, the single
#' `INLA::inla(family = "betabinomial", ...)` fit plus the subsequent
#' `INLA::inla.posterior.sample()` call used inside [clusterModel()].  The goal
#' is a drop-in replacement: the returned objects expose exactly the pieces the
#' downstream aggregation code relies on, namely
#'   * `imod$summary.fitted.values` with columns
#'     mean, sd, `0.025quant`, `0.5quant`, `0.975quant`, and
#'   * `samp`, a list of posterior draws where each element has a `$latent`
#'     matrix whose row names match INLA's latent naming convention
#'     (`sID:k`, `(Intercept):1`, `strataurban:1`, `admin1.nameX:1`,
#'     `admin1.nameX:strataurban:1`, `<covariate>:1`).
#'
#' The model specification matches INLA exactly (see inst/stan/betabinom.stan):
#' beta-binomial likelihood with overdispersion = intracluster correlation,
#' logit link, IID or BYM2 area effects, PC priors on the precision and the
#' BYM2 mixing parameter, and INLA's default fixed-effect priors (flat
#' intercept, precision 0.001 otherwise).
#'
#' @keywords internal
#' @name stan_shim
NULL

# compiled-model cache (compile once per session)
.surveyPrev_stan_cache <- new.env(parent = emptyenv())

#' @keywords internal
get_betabinom_stanmodel <- function() {
  if (!is.null(.surveyPrev_stan_cache$betabinom)) {
    return(.surveyPrev_stan_cache$betabinom)
  }
  f <- system.file("stan", "betabinom.stan", package = "surveyPrev")
  if (identical(f, "") || !file.exists(f)) {
    # dev / load_all fallback
    cand <- file.path("inst", "stan", "betabinom.stan")
    if (file.exists(cand)) f <- cand
  }
  if (identical(f, "") || !file.exists(f)) {
    stop("Could not locate inst/stan/betabinom.stan")
  }
  mod <- rstan::stan_model(f)
  .surveyPrev_stan_cache$betabinom <- mod
  mod
}

#' BYM2 scaling factor (geometric mean of marginal variances of the scaled ICAR)
#' @keywords internal
bym2_scaling_factor <- function(W) {
  W <- as.matrix(W)
  diag(W) <- 0
  W <- (W != 0) * 1
  n <- nrow(W)
  Q <- diag(rowSums(W)) - W
  Q_pert <- Q + diag(n) * max(diag(Q)) * sqrt(.Machine$double.eps)
  Qinv <- solve(Q_pert)
  A <- matrix(1, nrow = n, ncol = 1)                 # sum-to-zero constraint
  Qinv_c <- Qinv - Qinv %*% A %*% solve(t(A) %*% Qinv %*% A) %*% t(A) %*% Qinv
  list(scaling = exp(mean(log(diag(Qinv_c)))), Qinv_c = Qinv_c)
}

#' PC prior log-density grid for the BYM2 mixing parameter phi
#'
#' Reproduces INLA's KLD-based PC prior: with Sigma(phi) = (1-phi) I + phi R*
#' (R* = scaled structured covariance, geometric-mean diagonal 1), the distance
#' is d(phi) = sqrt(2 * KLD(phi)) and phi has an exponential prior on d with
#' rate theta chosen so that P(phi < u) = alpha.
#' @keywords internal
bym2_phi_pc_grid <- function(Qinv_c, scaling, u = 0.5, alpha = 2 / 3, K = 200) {
  Rstar <- Qinv_c / scaling                          # geometric-mean diag == 1
  ev <- eigen(Rstar, symmetric = TRUE, only.values = TRUE)$values
  ev <- sort(ev, decreasing = TRUE)
  e <- ev[seq_len(length(ev) - 1)]                   # drop null-space eigenvalue

  KLD <- function(phi) {
    lam <- 1 - phi + phi * e
    0.5 * sum(phi * (e - 1) - log(lam))
  }
  dfun <- function(phi) sqrt(2 * max(KLD(phi), 0))

  theta <- -log(1 - alpha) / dfun(u)                 # calibrate to P(phi<u)=alpha

  grid <- seq(0.0005, 0.9995, length.out = K)
  dvals <- vapply(grid, dfun, numeric(1))
  # d'(phi) by central finite difference
  h <- 1e-5
  dprime <- vapply(grid, function(p) {
    pl <- min(max(p - h, 1e-8), 1 - 1e-8)
    pr <- min(max(p + h, 1e-8), 1 - 1e-8)
    (dfun(pr) - dfun(pl)) / (pr - pl)
  }, numeric(1))
  logp <- log(theta) - theta * dvals + log(abs(dprime))
  list(grid = grid, logp = logp)
}

#' Drop-in rstan replacement for the INLA beta-binomial fit in clusterModel()
#'
#' @param formula the INLA-style formula built inside clusterModel (the
#'   `f(sID, ...)` term is stripped; the random effect is specified via `model`
#'   and `admin.mat` instead).
#' @param data the assembled `c.dat.tmp` data frame (observed cluster rows plus
#'   appended per-area prediction rows with `value = NA`).
#' @param model "bym2" or "iid".
#' @param admin.mat area adjacency matrix (required for bym2).
#' @param Ntrials name of the trials column (default "n").
#' @param nsamp number of posterior draws to return (matches INLA's 1000).
#' @return list(imod = <list with summary.fitted.values and stanfit>,
#'              samp = <list of posterior draws with INLA-named $latent>).
#' @keywords internal
stan_betabinomial <- function(formula, data, model = c("bym2", "iid"),
                              admin.mat = NULL,
                              overdisp.mean = 0, overdisp.prec = 0.4,
                              pc.u = 1, pc.alpha = 0.01,
                              pc.u.phi = 0.5, pc.alpha.phi = 2 / 3,
                              nsamp = 1000, Ntrials = "n",
                              stan.chains = 4, stan.iter = 1000,
                              stan.warmup = 500, stan.seed = 1,
                              stan.cores = getOption("mc.cores", 1L), ...) {
  model <- match.arg(model)
  use_bym2 <- as.integer(model == "bym2")

  ## ---- fixed-effects design matrix (strip the f() random-effect term) ----
  tt <- stats::terms(formula)
  labs <- attr(tt, "term.labels")
  keep <- labs[!grepl("^f\\(", labs)]
  icpt <- attr(tt, "intercept")
  rhs <- if (length(keep)) paste(keep, collapse = " + ") else "1"
  if (icpt == 0) rhs <- paste(rhs, "- 1")
  fixed_formula <- stats::as.formula(paste("value ~", rhs))

  mf <- stats::model.frame(fixed_formula, data = data, na.action = stats::na.pass)
  mm <- stats::model.matrix(fixed_formula, mf)
  mm[is.na(mm)] <- 0                     # NA only in unused prediction-row cells
  colnms <- colnames(mm)
  P <- ncol(mm)

  ## ---- observed vs prediction rows ----
  yval <- data[["value"]]
  ntr  <- data[[Ntrials]]
  sID  <- data[["sID"]]
  obs  <- which(!is.na(yval) & !is.na(ntr))
  prd  <- which(is.na(yval))
  R <- max(sID, na.rm = TRUE)

  ## ---- BYM2 structure ----
  if (use_bym2 == 1) {
    if (is.null(admin.mat)) stop("admin.mat required for bym2 model")
    W <- as.matrix(admin.mat)
    diag(W) <- 0
    W <- (W != 0) * 1
    ut <- which(upper.tri(W) & W == 1, arr.ind = TRUE)
    node1 <- as.integer(ut[, 1]); node2 <- as.integer(ut[, 2])
    sf <- bym2_scaling_factor(W)
    phi_pc <- bym2_phi_pc_grid(sf$Qinv_c, sf$scaling, u = pc.u.phi,
                               alpha = pc.alpha.phi)
    scaling_factor <- sf$scaling
    phi_grid <- phi_pc$grid; phi_logp <- phi_pc$logp
  } else {
    node1 <- integer(0); node2 <- integer(0)
    scaling_factor <- 1
    phi_grid <- c(0, 1); phi_logp <- c(0, 0)
  }

  # as.array() guards against rstan collapsing length-1 vectors to scalars
  stan_data <- list(
    N = length(obs), P = P,
    X = mm[obs, , drop = FALSE],
    y = as.array(as.integer(yval[obs])),
    Ntrials = as.array(as.integer(ntr[obs])),
    R = R, region = as.array(as.integer(sID[obs])),
    Npred = length(prd),
    Xpred = mm[prd, , drop = FALSE],
    region_pred = as.array(as.integer(sID[prd])),
    beta_prec = as.array(ifelse(colnms == "(Intercept)", 0, 0.001)),
    od_mean = overdisp.mean, od_sd = 1 / sqrt(overdisp.prec),
    lambda = -log(pc.alpha) / pc.u,
    use_bym2 = use_bym2,
    N_edges = length(node1),
    node1 = as.array(node1), node2 = as.array(node2),
    scaling_factor = scaling_factor,
    Kphi = length(phi_grid),
    phi_grid = as.array(phi_grid), phi_logp = as.array(phi_logp)
  )

  mod <- get_betabinom_stanmodel()
  dots <- list(...)
  if (is.null(dots$control)) {
    dots$control <- list(max_treedepth = 12, adapt_delta = 0.95)
  }
  fit <- do.call(rstan::sampling, c(list(object = mod, data = stan_data,
                                         chains = stan.chains, iter = stan.iter,
                                         warmup = stan.warmup, seed = stan.seed,
                                         cores = stan.cores, refresh = 0), dots))

  post <- rstan::extract(fit, pars = c("beta", "re", "fitted"), permuted = TRUE)
  ndraw <- nrow(post$beta)
  sel <- sample.int(ndraw, nsamp, replace = ndraw < nsamp)

  ## ---- posterior draws with INLA latent naming ----
  re_names <- paste0("sID:", seq_len(R))
  fx_names <- paste0(colnms, ":1")
  beta_sel <- post$beta[sel, , drop = FALSE]
  re_sel   <- post$re[sel, , drop = FALSE]
  samp <- lapply(seq_len(nsamp), function(i) {
    latent <- matrix(c(re_sel[i, ], beta_sel[i, ]), ncol = 1)
    rownames(latent) <- c(re_names, fx_names)
    list(latent = latent)
  })

  ## ---- summary.fitted.values (prediction rows, INLA column layout) ----
  fp <- post$fitted                     # ndraw x Npred
  qs <- t(apply(fp, 2, stats::quantile, probs = c(0.025, 0.5, 0.975)))
  sfv_pred <- data.frame(
    mean = colMeans(fp),
    sd = apply(fp, 2, stats::sd),
    `0.025quant` = qs[, 1],
    `0.5quant`  = qs[, 2],
    `0.975quant` = qs[, 3],
    check.names = FALSE, row.names = NULL
  )
  sfv <- data.frame(
    mean = rep(NA_real_, nrow(data)), sd = NA_real_,
    `0.025quant` = NA_real_, `0.5quant` = NA_real_, `0.975quant` = NA_real_,
    check.names = FALSE
  )
  sfv[prd, ] <- sfv_pred

  imod <- list(summary.fitted.values = sfv,
               stanfit = fit,
               waic = list(waic = NA_real_),
               .engine = "rstan")

  list(imod = imod, samp = samp)
}
