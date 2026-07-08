#' rstan replacement for SUMMER::smoothSurvey() as used by fhModel()
#'
#' [stan_smoothSurvey()] reimplements, with rstan, the subset of
#' `SUMMER::smoothSurvey()` exercised by [fhModel()] in this package:
#' the area-level (Fay-Herriot) model with `svy = TRUE`, either
#'   * `response.type = "gaussian"` with `direct.est` supplied (logit-scale
#'     direct estimates and their variances), or
#'   * `response.type = "binary"` with unit-level survey data, where the
#'     Horvitz-Thompson logit estimates are first computed with the survey
#'     package (identically to SUMMER) and then smoothed.
#'
#' In both cases the smoothing model is gaussian with known, fixed
#' observation variances (INLA: precision fixed at 1, `scale = 1/var_i`),
#' an intercept, optional area-level covariates `X`, and an IID (when
#' `Amat` is `NULL`) or BYM2 region effect with PC priors -- fitted here
#' with Stan (inst/stan/fh_gaussian.stan).
#'
#' The returned object mirrors the pieces of the `SUMMERmodel.svy` output
#' that surveyPrev consumes: `$smooth`, `$draws.est` (region, time, then
#' `sample:j` columns of linear-predictor draws), `$HT`, `$direct`, and
#' `$fit` (here the stanfit). Row order follows SUMMER exactly, including
#' the re-sorting (alphabetical by region) that `merge()` introduces when
#' covariates `X` are supplied.
#'
#' Note: SUMMER::smoothSurvey() (<= 1.4.0) has a bug where, for binary
#' responses, `smooth$logit.upper` is filled with the *lower* quantile
#' (SUMMER source: `quantile(tmp, (1 - CI)/2)` for both bounds). This
#' implementation computes the upper quantile correctly, so
#' `logit.upper` will differ from SUMMER's output by design.
#'
#' @keywords internal
get_fh_stanmodel <- function() {
  if (!is.null(.surveyPrev_stan_cache$fh_gaussian)) {
    return(.surveyPrev_stan_cache$fh_gaussian)
  }
  f <- system.file("stan", "fh_gaussian.stan", package = "surveyPrev")
  if (identical(f, "") || !file.exists(f)) {
    cand <- file.path("inst", "stan", "fh_gaussian.stan")
    if (file.exists(cand)) f <- cand
  }
  if (identical(f, "") || !file.exists(f)) {
    stop("Could not locate inst/stan/fh_gaussian.stan")
  }
  mod <- rstan::stan_model(f)
  .surveyPrev_stan_cache$fh_gaussian <- mod
  mod
}

#' @rdname get_fh_stanmodel
#' @keywords internal
stan_smoothSurvey <- function(data = NULL, Amat = NULL, region.list = NULL,
                              X = NULL,
                              responseType = NULL,
                              response.type = c("binary", "gaussian")[1],
                              responseVar, strataVar = "strata",
                              weightVar = "weights", regionVar = "region",
                              clusterVar = "~v001+v002",
                              pc.u = 1, pc.alpha = 0.01,
                              pc.u.phi = 0.5, pc.alpha.phi = 2 / 3,
                              CI = 0.95, direct.est = NULL,
                              direct.est.var = NULL, is.unit.level = FALSE,
                              nsim = 1000, save.draws = FALSE, smooth = TRUE,
                              stan.chains = 4, stan.iter = 1000,
                              stan.warmup = 500, stan.seed = 1,
                              stan.cores = getOption("mc.cores", 1L), ...) {
  if (!is.null(responseType)) response.type <- responseType
  response.type <- tolower(response.type)
  if (is.unit.level) {
    stop("stan_smoothSurvey() only implements the area-level model used by fhModel()")
  }

  ## ---------- assemble the region-level direct estimates (dat) ----------
  if (!is.null(direct.est)) {
    ## gaussian direct-estimate mode (logit-scale estimates supplied)
    d <- direct.est
    region0 <- as.character(d[, regionVar])
    if (is.null(Amat)) {
      regions <- if (!is.null(region.list)) unique(region.list) else unique(region0)
      if (sum(!region0 %in% regions) > 0) {
        stop("'region.list' is specified but there are regions in the data not in the 'region.list'.")
      }
      Amat <- matrix(0, length(regions), length(regions),
                     dimnames = list(regions, regions))
      is.iid.space <- TRUE
    } else {
      is.iid.space <- FALSE
    }
    if (sum(!region0 %in% colnames(Amat)) > 0) {
      stop("Exist regions in the data frame but not in Amat.")
    }
    p.i <- d[, responseVar]
    if (is.null(direct.est.var)) {
      stop("Need to specify column for the variance of direct estimates")
    }
    var.i <- d[, direct.est.var]
    if (response.type == "binary") {
      ht <- log(p.i / (1 - p.i))
      ht.v <- var.i / (p.i^2 * (1 - p.i)^2)
    } else {
      ht <- p.i
      ht.v <- var.i
    }
    dat <- data.frame(HT.est = p.i, HT.var = var.i,
                      HT.logit.est = ht, HT.logit.var = ht.v,
                      HT.logit.prec = 1 / ht.v,
                      region = region0, stringsAsFactors = FALSE)
  } else {
    ## binary survey-data mode: HT estimates via the survey package
    data <- as.data.frame(data)
    data$response0 <- data[, responseVar]
    data$region0 <- as.character(data[, regionVar])
    if (is.null(Amat)) {
      regions <- if (!is.null(region.list)) unique(region.list) else unique(data$region0)
      if (sum(!data$region0 %in% regions) > 0) {
        stop("'region.list' is specified but there are regions in the data not in the 'region.list'.")
      }
      Amat <- matrix(0, length(regions), length(regions),
                     dimnames = list(regions, regions))
      is.iid.space <- TRUE
    } else {
      is.iid.space <- FALSE
    }
    if (sum(!data$region0 %in% colnames(Amat)) > 0) {
      stop("Exist regions in the data frame but not in Amat.")
    }
    data$region0 <- factor(data$region0, levels = colnames(Amat))
    data$weights0 <- data[, weightVar]
    data$strata0 <- if (is.null(strataVar)) 1 else factor(data[, strataVar])

    design <- survey::svydesign(ids = stats::formula(clusterVar),
                                weights = ~weights0, strata = ~strata0,
                                data = data)
    mean <- survey::svyby(formula = ~response0, by = ~region0,
                          design = design, survey::svymean,
                          drop.empty.groups = FALSE)
    p.i <- mean$response0
    var.i <- mean$se^2
    ht <- log(p.i / (1 - p.i))
    ht.v <- var.i / (p.i^2 * (1 - p.i)^2)
    dat <- data.frame(HT.est = p.i, HT.var = var.i,
                      HT.logit.est = ht, HT.logit.var = ht.v,
                      HT.logit.prec = 1 / ht.v,
                      region = as.character(mean$region0),
                      stringsAsFactors = FALSE)
  }

  dat$HT.logit.est[is.infinite(abs(dat$HT.logit.est))] <- NA
  dat$region.struct <- match(dat$region, rownames(Amat))
  dat <- dat[order(dat$region.struct), ]

  ## ---------- covariates: replicate SUMMER's merge (re-sorts rows) ----------
  fixed <- NULL
  if (!is.null(X)) {
    X <- data.frame(X)
    fixed <- colnames(X)[colnames(X) != regionVar]
    colnames(X)[colnames(X) == regionVar] <- "region"
    if (sum(fixed %in% colnames(dat)) > 0) {
      dat <- dat[, !colnames(dat) %in% fixed]
    }
    dat <- merge(dat, X, by = "region", all = TRUE)   # sorts by region name
  }
  R <- nrow(dat)

  ## ---------- fixed-effects design ----------
  rhs <- if (length(fixed)) paste(c("1", fixed), collapse = " + ") else "1"
  mm <- stats::model.matrix(stats::as.formula(paste("~", rhs)),
                            stats::model.frame(stats::as.formula(paste("~", rhs)),
                                               data = dat,
                                               na.action = stats::na.pass))
  mm[is.na(mm)] <- 0
  colnms <- colnames(mm)
  P <- ncol(mm)

  ## ---------- observation rows ----------
  obs <- which(!is.na(dat$HT.logit.est) & !is.na(dat$HT.logit.var) &
                 dat$HT.logit.var > 0)

  ## ---------- spatial structure ----------
  use_bym2 <- as.integer(!is.iid.space)
  if (use_bym2 == 1) {
    W <- as.matrix(Amat)
    diag(W) <- 0
    W <- (W != 0) * 1
    ## reorder W to dat's row order
    W <- W[dat$region, dat$region, drop = FALSE]
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

  stan_data <- list(
    R = R, P = P, Xall = mm,
    region = as.array(seq_len(R)),
    N = length(obs), obs_idx = as.array(obs),
    y = as.array(dat$HT.logit.est[obs]),
    sdv = as.array(sqrt(dat$HT.logit.var[obs])),
    beta_prec = as.array(ifelse(colnms == "(Intercept)", 0, 0.001)),
    lambda = -log(pc.alpha) / pc.u,
    use_bym2 = use_bym2,
    N_edges = length(node1),
    node1 = as.array(node1), node2 = as.array(node2),
    scaling_factor = scaling_factor,
    Kphi = length(phi_grid),
    phi_grid = as.array(phi_grid), phi_logp = as.array(phi_logp)
  )

  mod <- get_fh_stanmodel()
  dots <- list(...)
  dots <- dots[names(dots) %in% c("control", "thin", "init")]
  if (is.null(dots$control)) {
    dots$control <- list(max_treedepth = 12, adapt_delta = 0.95)
  }
  fit <- do.call(rstan::sampling,
                 c(list(object = mod, data = stan_data, chains = stan.chains,
                        iter = stan.iter, warmup = stan.warmup,
                        seed = stan.seed, cores = stan.cores, refresh = 0),
                   dots))

  eta <- rstan::extract(fit, pars = "eta", permuted = TRUE)$eta  # ndraw x R

  ## ---------- $smooth (proj) ----------
  qs <- c((1 - CI) / 2, 0.5, 1 - (1 - CI) / 2)
  proj <- data.frame(region = dat$region,
                     mean = NA_real_, var = NA_real_, median = NA_real_,
                     lower = NA_real_, upper = NA_real_,
                     logit.mean = NA_real_, logit.var = NA_real_,
                     logit.median = NA_real_, logit.lower = NA_real_,
                     logit.upper = NA_real_, stringsAsFactors = FALSE)
  for (i in seq_len(R)) {
    tmp <- eta[, i]
    if (response.type == "binary") {
      tmp2 <- SUMMER::expit(tmp)
      proj[i, c("logit.mean", "logit.var", "logit.median")] <-
        c(mean(tmp), stats::var(tmp), stats::median(tmp))
      proj[i, c("logit.lower", "logit.upper")] <-
        stats::quantile(tmp, c(qs[1], qs[3]))
    } else {
      tmp2 <- tmp
    }
    proj[i, c("mean", "var", "median")] <-
      c(mean(tmp2), stats::var(tmp2), stats::median(tmp2))
    proj[i, c("lower", "upper")] <- stats::quantile(tmp2, c(qs[1], qs[3]))
  }
  if (response.type == "gaussian") {
    proj <- proj[, !colnames(proj) %in% c("logit.mean", "logit.var",
                                          "logit.median", "logit.lower",
                                          "logit.upper")]
  }

  ## ---------- $draws.est ----------
  draws.out <- NULL
  if (save.draws) {
    ndraw <- nrow(eta)
    sel <- sample.int(ndraw, nsim, replace = ndraw < nsim)
    draws.out <- data.frame(region = dat$region, time = NA,
                            stringsAsFactors = FALSE)
    smp <- t(eta[sel, , drop = FALSE])                 # R x nsim
    colnames(smp) <- paste0("sample:", seq_len(nsim))
    draws.out <- cbind(draws.out, smp)
  }

  ## ---------- $HT ----------
  HT <- dat[, colnames(dat) %in% c("region", "HT.est", "HT.var",
                                   "HT.logit.est", "HT.logit.var",
                                   "HT.logit.prec")]
  HT <- cbind(region = HT[, "region"],
              HT[, colnames(HT) != "region", drop = FALSE])
  if (response.type == "gaussian") {
    HT <- HT[, !colnames(HT) %in% c("HT.logit.est", "HT.logit.var",
                                    "HT.logit.prec")]
  }
  direct <- HT
  colnames(direct) <- gsub("HT", "direct", colnames(HT))

  out <- list(direct = direct, smooth = proj, smooth.overall = NULL,
              fit = fit, CI = CI, Amat = Amat,
              response.type = response.type, formula = NULL,
              msg = "Area-level model (rstan backend)", HT = HT)
  if (save.draws) {
    out$draws <- NULL
    out$draws.est <- draws.out
  }
  class(out) <- "SUMMERmodel.svy"
  out
}

#' Engine dispatcher for the smoothSurvey call sites in fhModel()
#' @keywords internal
smoothSurvey_engine <- function(...) {
  if (identical(getOption("surveyPrev.engine", "rstan"), "INLA")) {
    SUMMER::smoothSurvey(...)
  } else {
    stan_smoothSurvey(...)
  }
}
