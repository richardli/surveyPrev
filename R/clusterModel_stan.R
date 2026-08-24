#' Stan replacement for the INLA beta-binomial fit used by clusterModel
#'
#' @keywords internal
stan_betabinomial <- function(formula, data, model = c("bym2", "iid"),
                              admin.mat = NULL,
                              overdisp.mean = 0, overdisp.prec = 0.4,
                              pc.u = 1, pc.alpha = 0.01,
                              pc.u.phi = 0.5, pc.alpha.phi = 2 / 3,
                              bym2.phi.prior = c("inla.table", "spectral"),
                              constr = TRUE,
                              nsamp = 1000, Ntrials = "n",
                              stan.chains = 3, stan.iter = 4000,
                              stan.warmup = 2000, stan.seed = 1,
                              stan.cores = getOption("mc.cores", 1L), ...) {
  model <- match.arg(model)
  bym2.phi.prior <- match.arg(bym2.phi.prior)

  tt <- stats::terms(formula)
  labs <- attr(tt, "term.labels")
  keep <- labs[!grepl("^f\\(", labs)]
  rhs <- if (length(keep)) paste(keep, collapse = " + ") else "1"
  if (attr(tt, "intercept") == 0) rhs <- paste(rhs, "- 1")
  fixed_formula <- stats::as.formula(paste("value ~", rhs))
  mf <- stats::model.frame(fixed_formula, data = data,
                           na.action = stats::na.pass)
  mm <- stats::model.matrix(fixed_formula, mf)
  mm[is.na(mm)] <- 0
  colnms <- colnames(mm)

  yval <- data[["value"]]
  ntr <- data[[Ntrials]]
  sID <- data[["sID"]]
  obs <- which(!is.na(yval) & !is.na(ntr))
  pred <- which(is.na(yval))
  R <- max(sID, na.rm = TRUE)

  if (model == "bym2") {
    if (is.null(admin.mat)) {
      stop("admin.mat is required for model = 'bym2'.", call. = FALSE)
    }
    graph <- .prepare_bym2_graph_stan(
      admin.mat, phi.prior = bym2.phi.prior,
      u = pc.u.phi, alpha = pc.alpha.phi
    )
    space_model <- 1L
  } else {
    graph <- .empty_spatial_data_stan(R)
    space_model <- 0L
  }

  stan_data <- list(
    N = length(obs), P = ncol(mm), X = mm[obs, , drop = FALSE],
    y = as.array(as.integer(yval[obs])),
    Ntrials = as.array(as.integer(ntr[obs])),
    R = R, region = as.array(as.integer(sID[obs])),
    time = as.array(rep.int(1L, length(obs))),
    Npred = length(pred), Xpred = mm[pred, , drop = FALSE],
    region_pred = as.array(as.integer(sID[pred])),
    time_pred = as.array(rep.int(1L, length(pred))),
    beta_prec = as.array(ifelse(colnms == "(Intercept)", 0, 0.001)),
    od_mean = overdisp.mean, od_sd = 1 / sqrt(overdisp.prec),
    lambda = -log(pc.alpha) / pc.u,
    space_model = space_model,
    N_edges = graph$N_edges, node1 = graph$node1, node2 = graph$node2,
    N_components = graph$N_components, component = graph$component,
    component_size = graph$component_size,
    component_reference = graph$component_reference,
    N_singletons = graph$N_singletons, singleton = graph$singleton,
    node_scale = graph$node_scale,
    W_sar = matrix(0, R, R),
    phi_prior_type = graph$phi_prior_type,
    Kphi = graph$Kphi, phi_grid = graph$phi_grid,
    phi_logp = graph$phi_logp, phi_gamma = graph$phi_gamma,
    phi_f = graph$phi_f, phi_lambda = graph$phi_lambda,
    has_time = 0L, T = 1L, time_model = 1L, time_scale = 1,
    st_type = 0L, constr = as.integer(isTRUE(constr))
  )

  sampling_args <- .stan_sampling_args(
    chains = stan.chains, iter = stan.iter, warmup = stan.warmup,
    seed = stan.seed, cores = stan.cores, ...
  )
  fit <- do.call(
    rstan::sampling,
    c(list(object = get_betabinom_stanmodel(), data = stan_data),
      sampling_args)
  )

  post <- rstan::extract(
    fit, pars = c("beta", "re_space", "fitted"), permuted = TRUE
  )
  ndraw <- nrow(post$beta)
  selected <- sample.int(ndraw, nsamp, replace = ndraw < nsamp)
  re_names <- paste0("sID:", seq_len(R))
  fx_names <- paste0(colnms, ":1")
  samp <- lapply(selected, function(i) {
    latent <- matrix(c(post$re_space[i, ], post$beta[i, ]), ncol = 1)
    rownames(latent) <- c(re_names, fx_names)
    list(latent = latent)
  })

  fitted <- post$fitted
  quantiles <- t(apply(fitted, 2, stats::quantile,
                       probs = c(0.025, 0.5, 0.975)))
  predicted_summary <- data.frame(
    mean = colMeans(fitted),
    sd = apply(fitted, 2, stats::sd),
    `0.025quant` = quantiles[, 1],
    `0.5quant` = quantiles[, 2],
    `0.975quant` = quantiles[, 3],
    check.names = FALSE, row.names = NULL
  )
  summary_fitted <- data.frame(
    mean = rep(NA_real_, nrow(data)), sd = NA_real_,
    `0.025quant` = NA_real_, `0.5quant` = NA_real_,
    `0.975quant` = NA_real_, check.names = FALSE
  )
  summary_fitted[pred, ] <- predicted_summary

  imod <- list(
    summary.fitted.values = summary_fitted,
    stanfit = fit,
    waic = list(waic = NA_real_),
    .engine = "rstan",
    .stan_data = stan_data,
    .bym2_phi_prior = if (model == "bym2") bym2.phi.prior else NULL
  )
  list(imod = imod, samp = samp)
}
