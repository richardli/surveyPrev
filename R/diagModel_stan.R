.diag_stan <- function(fit, rhat.threshold, ess.ratio.threshold,
                       ebfmi.threshold) {
  # Sampler diagnostics are computed per retained chain; warmup is excluded.
  sampler <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  divergences <- sum(vapply(sampler, function(x) sum(x[, "divergent__"]),
                            numeric(1)))
  total <- sum(vapply(sampler, nrow, integer(1)))

  stan.args <- tryCatch(fit@stan_args, error = function(e) NULL)
  # Compare treedepth against the value actually used by rstan when available.
  max.depth <- 10L
  if (length(stan.args) && !is.null(stan.args[[1]]$control$max_treedepth)) {
    max.depth <- stan.args[[1]]$control$max_treedepth
  }
  depth.hits <- sum(vapply(
    sampler, function(x) sum(x[, "treedepth__"] >= max.depth), numeric(1)
  ))
  ebfmi <- vapply(sampler, function(x) {
    energy <- x[, "energy__"]
    mean(diff(energy)^2) / stats::var(energy)
  }, numeric(1))

  smry <- rstan::summary(fit, probs = c(0.05, 0.5, 0.95))$summary
  # Flag parameter-level convergence/mixing, while reporting aggregate counts.
  rhat <- smry[, "Rhat"]
  neff <- smry[, "n_eff"]
  rhat.bad <- sum(is.finite(rhat) & rhat > rhat.threshold)
  max.rhat <- if (any(is.finite(rhat))) max(rhat, na.rm = TRUE) else NA_real_
  ess.ratio <- neff / max(total, 1)
  ess.bad <- sum(is.finite(ess.ratio) & ess.ratio < ess.ratio.threshold)
  min.ess.ratio <- if (any(is.finite(ess.ratio))) {
    min(ess.ratio, na.rm = TRUE)
  } else {
    NA_real_
  }

  rows <- list(
    .diag_row(
      "divergences", if (divergences == 0) "pass" else "warning",
      divergences, "0",
      if (divergences == 0) "No divergent transitions."
      else "Increase adapt_delta and inspect the model geometry."
    ),
    .diag_row(
      "maximum treedepth", if (depth.hits == 0) "pass" else "warning",
      depth.hits, "0",
      if (depth.hits == 0) "No transitions saturated max_treedepth."
      else "Increase max_treedepth only after checking parameterization."
    ),
    .diag_row(
      "R-hat", if (rhat.bad == 0) "pass" else "warning",
      sprintf("max %.3f; %d above threshold", max.rhat, rhat.bad),
      paste0("<= ", rhat.threshold),
      if (rhat.bad == 0) "All reported R-hat values are acceptable."
      else "Run longer chains and inspect affected parameters."
    ),
    .diag_row(
      "effective sample size",
      if (ess.bad == 0) "pass" else "warning",
      sprintf("min ratio %.3f; %d below threshold", min.ess.ratio, ess.bad),
      paste0(">= ", ess.ratio.threshold),
      if (ess.bad == 0) "Effective sample sizes are adequate."
      else "Increase post-warmup draws and inspect autocorrelation."
    ),
    .diag_row(
      "E-BFMI", if (all(ebfmi >= ebfmi.threshold, na.rm = TRUE)) "pass"
      else "warning",
      paste(sprintf("chain %d: %.3f", seq_along(ebfmi), ebfmi),
            collapse = "; "),
      paste0(">= ", ebfmi.threshold),
      if (all(ebfmi >= ebfmi.threshold, na.rm = TRUE))
        "Energy exploration is acceptable."
      else "Low E-BFMI suggests inefficient exploration of the energy distribution."
    )
  )
  list(summary = do.call(rbind, rows),
       details = list(rhat = rhat, n_eff = neff, ess_ratio = ess.ratio,
                      ebfmi = ebfmi, sampler = sampler))
}
