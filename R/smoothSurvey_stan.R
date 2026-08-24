.smoothsurvey_spatial_data_stan <- function(Amat, is.iid.space, space.model,
                                            type.st, bym2.phi.prior,
                                            pc.u.phi, pc.alpha.phi) {
  # Decide which graph representation is needed by the spatial main effect
  # and by Type III/IV interactions, even when the main effect itself is IID.
  R <- nrow(Amat)
  needs_besag <- !is.iid.space && (space.model == "bym2" ||
    type.st %in% c(3L, 4L))
  if (needs_besag) {
    graph <- .prepare_bym2_graph_stan(
      Amat,
      phi.prior = if (space.model == "bym2") bym2.phi.prior else "spectral",
      u = pc.u.phi, alpha = pc.alpha.phi
    )
  } else {
    graph <- .empty_spatial_data_stan(R)
  }

  W.sar <- matrix(0, R, R)
  # Stan code: 0 = IID, 1 = BYM2, 2 = row-standardized SAR.
  if (is.iid.space) {
    space.code <- 0L
  } else if (space.model == "bym2") {
    if (graph$N_edges == 0L) {
      stop(
        "A BYM2 model requires at least one adjacency edge. Use an IID model ",
        "when every region is a singleton.", call. = FALSE
      )
    }
    space.code <- 1L
  } else {
    space.code <- 2L
    W <- as.matrix(Amat)
    diag(W) <- 0
    rs <- rowSums(W)
    nonzero <- rs > 0
    W.sar[nonzero, ] <- W[nonzero, , drop = FALSE] / rs[nonzero]
  }

  list(code = space.code, graph = graph, W.sar = W.sar)
}

#' Stan fit used internally by smoothSurvey
#'
#' @keywords internal
.stan_smoothSurvey_fit <- function(dat, formulatext, Amat, is.iid.space,
                                   is.unit.level, response.type, svy,
                                   unmatched.link, time.model, space.model,
                                   type.st, constr, bym2.phi.prior,
                                   pc.u, pc.alpha, pc.u.phi, pc.alpha.phi,
                                   stan.chains = 3, stan.iter = 4000,
                                   stan.warmup = 2000, stan.seed = 1,
                                   stan.cores = getOption("mc.cores", 1L),
                                   stan.control = NULL) {
  # SUMMER has already standardized variable names and indices in dat. Here we
  # translate its fixed formula and model flags into the fixed Stan data schema.
  rhs <- sub("^[^~]*~", "", formulatext)
  fixed_formula <- stats::as.formula(paste("~", rhs))
  mf <- stats::model.frame(fixed_formula, data = dat,
                           na.action = stats::na.pass)
  mm <- stats::model.matrix(fixed_formula, mf)
  mm[is.na(mm)] <- 0
  colnms <- colnames(mm)
  Nrow <- nrow(dat)
  R <- nrow(Amat)
  region <- as.integer(dat$region.struct)

  has.time <- !is.null(dat$time.struct)
  # RW order is encoded as 1/2; scaling makes its marginal variance comparable
  # across different numbers of periods.
  T <- if (has.time) max(dat$time.struct) else 1L
  time <- if (has.time) as.integer(dat$time.struct) else rep.int(1L, Nrow)
  time.code <- if (tolower(time.model) == "rw1") 1L else 2L
  if (has.time && time.code == 1L && T < 2L) {
    stop("time.model = 'rw1' requires at least two time periods.", call. = FALSE)
  }
  if (has.time && time.code == 2L && T < 3L) {
    stop("time.model = 'rw2' requires at least three time periods.", call. = FALSE)
  }
  time.scale <- if (has.time) .rw_scaling_factor_stan(T, time.code) else 1

  spatial <- .smoothsurvey_spatial_data_stan(
    Amat = Amat, is.iid.space = is.iid.space, space.model = space.model,
    type.st = type.st, bym2.phi.prior = bym2.phi.prior,
    pc.u.phi = pc.u.phi, pc.alpha.phi = pc.alpha.phi
  )
  graph <- spatial$graph
  beta.prec <- as.array(ifelse(colnms == "(Intercept)", 0, 0.001))
  # PC prior P(sigma > pc.u) = pc.alpha implies lambda = -log(alpha) / u.
  lambda <- -log(pc.alpha) / pc.u

  # Both Stan likelihoods consume this common latent-effect specification.
  shared <- list(
    R = R,
    beta_prec = beta.prec,
    lambda = lambda,
    space_model = spatial$code,
    N_edges = graph$N_edges, node1 = graph$node1, node2 = graph$node2,
    N_components = graph$N_components, component = graph$component,
    component_size = graph$component_size,
    component_reference = graph$component_reference,
    N_singletons = graph$N_singletons, singleton = graph$singleton,
    node_scale = graph$node_scale, W_sar = spatial$W.sar,
    phi_prior_type = graph$phi_prior_type,
    Kphi = graph$Kphi, phi_grid = graph$phi_grid,
    phi_logp = graph$phi_logp, phi_gamma = graph$phi_gamma,
    phi_f = graph$phi_f, phi_lambda = graph$phi_lambda,
    has_time = as.integer(has.time), T = as.integer(T),
    time_model = time.code, time_scale = time.scale,
    st_type = if (has.time) as.integer(type.st) else 0L,
    constr = as.integer(isTRUE(constr))
  )
  sampling.args <- .stan_sampling_args(
    chains = stan.chains, iter = stan.iter, warmup = stan.warmup,
    seed = stan.seed, cores = stan.cores, control = stan.control
  )

  if (is.unit.level && response.type == "binary") {
    # Aggregated cluster counts use a beta-binomial likelihood.
    yresp <- dat$y
    ntot <- dat$total
    obs <- which(!is.na(yresp) & !is.na(ntot) & ntot > 0)
    stan.data <- c(list(
      N = length(obs), P = ncol(mm), X = mm[obs, , drop = FALSE],
      y = as.array(as.integer(yresp[obs])),
      Ntrials = as.array(as.integer(ntot[obs])),
      region = as.array(region[obs]), time = as.array(time[obs]),
      Npred = Nrow, Xpred = mm,
      region_pred = as.array(region), time_pred = as.array(time),
      od_mean = 0, od_sd = 1 / sqrt(0.4)
    ), shared)
    fitS <- do.call(
      rstan::sampling,
      c(list(object = get_betabinom_stanmodel(), data = stan.data),
        sampling.args)
    )
    post <- rstan::extract(
      fitS,
      pars = c("beta", "re_space", "re_time_unstruct", "re_time_struct",
               "re_st", "eta"),
      permuted = TRUE
    )
    eta.draws <- post$eta
  } else {
    # All Gaussian branches share fh_gaussian.stan. free.prec estimates an
    # observation variance; otherwise sdv contains known sampling errors.
    if (is.unit.level && response.type == "gaussian") {
      yresp <- dat$y
      vknown <- NULL
      link.logit <- 0L
      free.prec <- 1L
    } else if (!svy && response.type == "binary") {
      stop(
        "The non-survey binomial branch is not currently reachable from ",
        "smoothSurvey().", call. = FALSE
      )
    } else if (!svy && response.type == "gaussian") {
      yresp <- dat$y
      vknown <- rep(1, Nrow)
      link.logit <- 0L
      free.prec <- 0L
    } else if (unmatched.link) {
      yresp <- dat$HT.est
      vknown <- dat$HT.var
      link.logit <- 1L
      free.prec <- 0L
    } else {
      yresp <- dat$HT.logit.est
      vknown <- dat$HT.logit.var
      link.logit <- 0L
      free.prec <- 0L
    }

    if (free.prec == 1L) {
      obs <- which(!is.na(yresp))
      sdv <- rep(1, length(obs))
    } else {
      bad <- which(!is.na(yresp) & !is.na(vknown) & vknown <= 0)
      if (length(bad)) {
        stop(
          "Known sampling variances must be strictly positive. Invalid ",
          "model row(s): ", paste(bad, collapse = ", "), ".",
          call. = FALSE
        )
      }
      obs <- which(!is.na(yresp) & !is.na(vknown))
      sdv <- sqrt(vknown[obs])
    }
    stan.data <- c(list(
      Nrow = Nrow, P = ncol(mm), Xall = mm,
      region = as.array(region),
      N = length(obs), obs_idx = as.array(as.integer(obs)),
      y = as.array(yresp[obs]), sdv = as.array(sdv),
      link_logit = link.logit, free_prec = free.prec,
      time = as.array(time)
    ), shared)
    fitS <- do.call(
      rstan::sampling,
      c(list(object = get_fh_stanmodel(), data = stan.data), sampling.args)
    )
    post <- rstan::extract(
      fitS,
      pars = c("beta", "re_space", "re_time_unstruct", "re_time_struct",
               "re_st", "eta"),
      permuted = TRUE
    )
    eta.draws <- post$eta
  }

  ndraw <- nrow(eta.draws)
  # SUMMER post-processing expects INLA-like posterior samples named by latent
  # predictor, spatial, temporal, interaction, and fixed-effect components.
  samples <- lapply(seq_len(ndraw), function(i) {
    values <- c(eta.draws[i, ], post$re_space[i, ])
    names <- c(paste0("Predictor:", seq_len(Nrow)),
               paste0("region.struct:", seq_len(R)))
    if (has.time) {
      values <- c(values, post$re_time_unstruct[i, ],
                  post$re_time_struct[i, ])
      names <- c(names, paste0("time.unstruct:", seq_len(T)),
                 paste0("time.struct:", seq_len(T)))
      if (type.st > 0L) {
        values <- c(values, post$re_st[i, ])
        names <- c(names, paste0("region.int:", seq_len(R * T)))
      }
    }
    values <- c(values, post$beta[i, ])
    names <- c(names, paste0(colnms, ":1"))
    latent <- matrix(values, ncol = 1, dimnames = list(names, NULL))
    list(latent = latent)
  })

  list(
    eta_draws = eta.draws,
    samples = samples,
    stanfit = fitS,
    engine = "rstan",
    .engine = "rstan",
    .stan_data = stan.data,
    .bym2_phi_prior = if (spatial$code == 1L) bym2.phi.prior else NULL
  )
}

.stan_smoothSurvey_backend <- function(
    bym2.phi.prior = c("inla.table", "spectral"),
    stan.chains = 3, stan.iter = 4000, stan.warmup = 2000,
    stan.seed = 1, stan.cores = getOption("mc.cores", 1L),
    stan.control = NULL) {
  bym2.phi.prior <- match.arg(bym2.phi.prior)
  # This adapter implements the small backend contract consumed by SUMMER:
  # fit the model, return linear-predictor draws, and expose latent samples.
  list(
    name = "rstan",
    fit = function(input) {
      # nsim is part of SUMMER's INLA-oriented output interface. Stan returns
      # every retained MCMC draw, so it is not an input to the Stan fitter.
      input$nsim <- NULL
      do.call(
        .stan_smoothSurvey_fit,
        c(input, list(
          bym2.phi.prior = bym2.phi.prior,
          stan.chains = stan.chains, stan.iter = stan.iter,
          stan.warmup = stan.warmup, stan.seed = stan.seed,
          stan.cores = stan.cores, stan.control = stan.control
        ))
      )
    },
    linpred_draws = function(fit, i) matrix(fit$eta_draws[, i]),
    posterior_sample = function(fit, n) fit$samples
  )
}

#' Fit a SUMMER smoothSurvey model with Stan
#'
#' Uses \code{SUMMER::smoothSurvey()} for data preparation and output assembly and
#' supplies the surveyPrev Stan computational backend. This avoids exporting a
#' second function named `smoothSurvey` when both packages are attached.
#'
#' @param ... Arguments passed to \code{SUMMER::smoothSurvey()}. The `formula`
#'   argument is not supported because arbitrary INLA model expressions cannot
#'   be translated safely into the fixed Stan programs. If `nsim` is supplied,
#'   it is overwritten by the number of retained post-warmup Stan draws.
#' @param bym2.phi.prior BYM2 mixing prior. `"inla.table"` evaluates INLA's
#'   tabulated PC prior during data preparation; `"spectral"` evaluates the
#'   analytic spectral form in Stan.
#' @param constr logical; apply the default soft linear constraints to
#'   intrinsic spatial, temporal, and interaction effects.
#' @param stan.chains,stan.iter,stan.warmup,stan.seed,stan.cores,stan.control
#'   Stan sampling controls.
#'
#' @return An object of class `SUMMERmodel.svy` with the raw `stanfit` stored
#'   in `fit$stanfit`.
#' @export
smoothSurveyStan <- function(...,
    bym2.phi.prior = c("inla.table", "spectral"), constr = TRUE,
    stan.chains = 3, stan.iter = 4000, stan.warmup = 2000,
    stan.seed = 1, stan.cores = getOption("mc.cores", 1L),
    stan.control = NULL) {
  # Keep validation here at the public boundary, before SUMMER performs its
  # engine-independent data preparation and output assembly.
  dots <- list(...)
  if (".backend" %in% names(dots)) {
    stop(".backend is reserved for surveyPrev internals.", call. = FALSE)
  }
  if ("engine" %in% names(dots)) {
    stop(
      "smoothSurveyStan() always uses Stan; remove the engine argument. ",
      "Use SUMMER::smoothSurvey() for INLA.", call. = FALSE
    )
  }
  if (!is.null(dots$formula)) {
    stop(
      "The formula argument is available only in SUMMER::smoothSurvey(). ",
      "Use the built-in Stan model arguments or fit the model with INLA.",
      call. = FALSE
    )
  }
  if (!".backend" %in% names(formals(SUMMER::smoothSurvey))) {
    stop(
      "smoothSurveyStan() requires a SUMMER version with the smoothSurvey ",
      "backend interface.", call. = FALSE
    )
  }
  type.st <- if (is.null(dots$type.st)) 1L else dots$type.st
  if (length(type.st) != 1L || is.na(type.st) || !type.st %in% 0:4) {
    stop("type.st must be one of 0, 1, 2, 3, or 4.", call. = FALSE)
  }
  if (!is.null(dots$time.model)) {
    dots$time.model <- match.arg(tolower(dots$time.model), c("rw1", "rw2"))
  }
  if (!is.null(dots$space.model)) {
    dots$space.model <- match.arg(tolower(dots$space.model), c("bym2", "sar"))
  }

  retained.draws <- .stan_retained_draw_count(
    stan.chains, stan.iter, stan.warmup
  )
  if ("nsim" %in% names(dots)) {
    message(
      "smoothSurveyStan(): overwriting supplied nsim = ",
      paste(dots$nsim, collapse = ", "), "; using all ", retained.draws,
      " retained Stan draws (", stan.chains, " chains x (", stan.iter,
      " iterations - ", stan.warmup, " warmup))."
    )
  }
  # SUMMER uses nsim to size saved-draw and stratum-aggregation output. Keep
  # that interface synchronized with the number of draws retained by Stan.
  dots$nsim <- retained.draws

  dots$.backend <- .stan_smoothSurvey_backend(
    bym2.phi.prior = bym2.phi.prior,
    stan.chains = stan.chains, stan.iter = stan.iter,
    stan.warmup = stan.warmup, stan.seed = stan.seed,
    stan.cores = stan.cores, stan.control = stan.control
  )
  dots$constr <- constr
  # SUMMER calls the injected backend after preparing the survey/model data.
  do.call(SUMMER::smoothSurvey, dots)
}

.smoothSurvey_dispatch <- function(...,
    engine = getOption("surveyPrev.engine", "rstan"),
    bym2.phi.prior = c("inla.table", "spectral"), constr = TRUE,
    stan.chains = 3, stan.iter = 4000, stan.warmup = 2000,
    stan.seed = 1, stan.cores = getOption("mc.cores", 1L),
    stan.control = NULL) {
  engine <- match.arg(engine, c("rstan", "INLA"))
  # Internal compatibility bridge used by fhModel(); users call either
  # SUMMER::smoothSurvey() or surveyPrev::smoothSurveyStan() directly.
  if (engine == "INLA") {
    SUMMER::smoothSurvey(..., constr = constr)
  } else {
    smoothSurveyStan(
      ..., bym2.phi.prior = bym2.phi.prior, constr = constr,
      stan.chains = stan.chains, stan.iter = stan.iter,
      stan.warmup = stan.warmup, stan.seed = stan.seed,
      stan.cores = stan.cores, stan.control = stan.control
    )
  }
}
