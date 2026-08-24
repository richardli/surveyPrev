.diag_row <- function(check, status, value, threshold, message) {
  data.frame(
    check = check, status = status, value = as.character(value),
    threshold = as.character(threshold), message = message,
    stringsAsFactors = FALSE
  )
}

.find_model_fit <- function(object) {
  # clusterModel(), fhModel(), and smoothSurveyStan() use different wrapper
  # depths. In particular, fhModel() returns model$fit for INLA and
  # model$fit$stanfit for Stan. Walk only the known model-wrapper fields so
  # that diagnostics also work if another lightweight wrapper is added.
  queue <- list(list(value = object, depth = 0L))
  visited <- 0L
  while (length(queue) && visited < 100L) {
    item <- queue[[1L]]
    queue <- queue[-1L]
    candidate <- item$value
    visited <- visited + 1L

    if (inherits(candidate, "stanfit")) {
      return(list(engine = "rstan", fit = candidate))
    }
    if (inherits(candidate, "inla")) {
      return(list(engine = "INLA", fit = candidate))
    }

    if (is.list(candidate) && item$depth < 8L) {
      wrapper.names <- intersect(
        c("model", "fit", "inla", "stanfit"), names(candidate)
      )
      for (name in wrapper.names) {
        child <- candidate[[name]]
        if (!is.null(child)) {
          queue[[length(queue) + 1L]] <- list(
            value = child, depth = item$depth + 1L
          )
        }
      }
    }
  }
  stop(
    "diagModel() could not find an INLA fit or rstan stanfit in object.",
    call. = FALSE
  )
}

.diag_inla <- function(fit, cpo.failure.threshold, pit.p.threshold) {
  mode.status <- if (!is.null(fit$mode$status)) fit$mode$status else NA
  mode.ok <- is.na(mode.status) || identical(as.integer(mode.status), 0L)

  summaries <- c(list(fit$summary.fixed), unname(fit$summary.random))
  summaries <- Filter(Negate(is.null), summaries)
  finite.ok <- TRUE
  negative.sd <- FALSE
  for (x in summaries) {
    if (!is.matrix(x) && !is.data.frame(x)) next
    columns <- intersect(c("mean", "sd"), colnames(x))
    if (length(columns)) finite.ok <- finite.ok && all(is.finite(as.matrix(x[, columns, drop = FALSE])))
    if ("sd" %in% colnames(x)) negative.sd <- negative.sd || any(x[, "sd"] < 0)
  }

  cpo.failure <- fit$cpo$failure
  n.cpo.failure <- if (is.null(cpo.failure)) NA_integer_ else
    sum(cpo.failure > 0, na.rm = TRUE)
  cpo.values <- fit$cpo$cpo
  cpo.ok <- is.null(cpo.values) || all(is.finite(cpo.values) & cpo.values > 0,
                                      na.rm = TRUE)
  pit <- fit$cpo$pit
  pit <- pit[is.finite(pit)]
  pit.p <- if (length(pit) >= 20L) {
    suppressWarnings(stats::ks.test(pit, "punif")$p.value)
  } else {
    NA_real_
  }

  hyper <- fit$summary.hyperpar
  hyper.ok <- is.null(hyper) || all(is.finite(as.matrix(hyper)))
  rows <- list(
    .diag_row(
      "optimizer status", if (mode.ok) "pass" else "warning",
      if (is.na(mode.status)) "not reported" else mode.status, "0",
      if (mode.ok) "INLA did not report an optimizer failure."
      else "Inspect INLA mode and optimizer output."
    ),
    .diag_row(
      "latent summaries",
      if (finite.ok && !negative.sd) "pass" else "warning",
      if (finite.ok && !negative.sd) "finite" else "invalid values",
      "finite means and non-negative SDs",
      if (finite.ok && !negative.sd) "Fixed and random-effect summaries are finite."
      else "At least one fitted latent summary is non-finite or has invalid SD."
    ),
    .diag_row(
      "hyperparameters", if (hyper.ok) "pass" else "warning",
      if (hyper.ok) "finite" else "non-finite", "finite",
      if (hyper.ok) "Hyperparameter summaries are finite."
      else "Inspect hyperparameter marginals and prior specification."
    ),
    .diag_row(
      "CPO failures",
      if (is.na(n.cpo.failure)) "not available"
      else if (n.cpo.failure <= cpo.failure.threshold && cpo.ok) "pass"
      else "warning",
      if (is.na(n.cpo.failure)) "not computed" else n.cpo.failure,
      paste0("<= ", cpo.failure.threshold),
      if (is.na(n.cpo.failure)) "Refit with control.compute = list(cpo = TRUE) to run this check."
      else if (n.cpo.failure == 0 && cpo.ok) "No CPO failures were reported."
      else "Inspect observations with failed or invalid CPO values."
    ),
    .diag_row(
      "PIT calibration",
      if (is.na(pit.p)) "not available"
      else if (pit.p >= pit.p.threshold) "pass" else "warning",
      if (is.na(pit.p)) "not computed or fewer than 20 values"
      else sprintf("KS p-value %.3g", pit.p),
      paste0("p >= ", pit.p.threshold),
      if (is.na(pit.p)) "PIT requires computed CPO values and enough observations."
      else if (pit.p >= pit.p.threshold) "No strong aggregate PIT non-uniformity was detected."
      else "PIT values indicate possible likelihood or calibration problems."
    )
  )
  list(summary = do.call(rbind, rows),
       details = list(mode.status = mode.status, cpo.failure = cpo.failure,
                      cpo = cpo.values, pit = pit, pit.p.value = pit.p))
}

#' Diagnose a fitted INLA or Stan model
#'
#' Runs sampler diagnostics for Stan (divergences, treedepth, R-hat,
#' effective sample size, and E-BFMI) or fitted-model checks for INLA
#' (optimizer status, finite latent/hyperparameter summaries, CPO failures,
#' and PIT calibration). The function accepts raw fits and fitted objects
#' returned by \code{smoothSurveyStan()}, \code{SUMMER::smoothSurvey()},
#' \code{clusterModel()}, or \code{fhModel()}.
#'
#' @param object fitted INLA/Stan object or a surveyPrev model containing one.
#' @param rhat.threshold maximum acceptable Stan R-hat.
#' @param ess.ratio.threshold minimum Stan effective-sample-size ratio.
#' @param ebfmi.threshold minimum acceptable Stan E-BFMI.
#' @param cpo.failure.threshold maximum acceptable number of INLA CPO failures.
#' @param pit.p.threshold warning threshold for an aggregate PIT uniformity test.
#' @return An object of class \code{diagModel} containing a check table and
#'   engine-specific details.
#' @export
diagModel <- function(object, rhat.threshold = 1.01,
                      ess.ratio.threshold = 0.1,
                      ebfmi.threshold = 0.3,
                      cpo.failure.threshold = 0,
                      pit.p.threshold = 0.01) {
  resolved <- .find_model_fit(object)
  result <- if (resolved$engine == "rstan") {
    .diag_stan(resolved$fit, rhat.threshold, ess.ratio.threshold,
               ebfmi.threshold)
  } else {
    .diag_inla(resolved$fit, cpo.failure.threshold, pit.p.threshold)
  }
  result$engine <- resolved$engine
  result$fit <- resolved$fit
  class(result) <- "diagModel"
  result
}

#' @export
print.diagModel <- function(x, ...) {
  cat("Model diagnostics (", x$engine, ")\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}
