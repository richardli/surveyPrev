## Validate SUMMER::smoothSurvey() (INLA) against the surveyPrev Stan backend.
## on the SUMMER modes NOT exercised by fhModel():
##   J: unit-level binary (beta-binomial), stratified, weight.strata aggregation
##   K: unit-level gaussian nested-error model
##   L: area-level binary with unmatched.link
##   M: smooth = FALSE parity (HT only; also runs without INLA touched)
suppressMessages(library(rstan))
options(mc.cores = 2); rstan_options(auto_write = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
set.seed(77)

A <- 10; areas <- sprintf("r%02d", 1:A)
W <- matrix(0, A, A, dimnames = list(areas, areas))
for (i in 1:(A - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }

## ---------- unit-level data (individual rows with cluster ids) ----------
re <- as.numeric(scale(cos(1:A))) * 0.5
rows <- list(); cl <- 0
for (a in 1:A) for (k in 1:15) {
  cl <- cl + 1
  st <- sample(c("urban", "rural"), 1)
  n <- sample(15:30, 1)
  p <- plogis(-1 + re[a] + ifelse(st == "urban", 0.6, 0))
  cp <- rbeta(1, p * 9, (1 - p) * 9)
  y <- rbinom(1, n, cp)
  rows[[cl]] <- data.frame(clusterid = cl, hh = seq_len(n),
                           region = areas[a], strata.ur = st,
                           yvar = c(rep(1, y), rep(0, n - y)),
                           gauss = rnorm(n, mean = 1 + re[a] +
                                           ifelse(st == "urban", 0.3, 0), sd = 1),
                           weights = runif(1, 0.5, 2))
}
udat <- do.call(rbind, rows)
wstrata <- data.frame(region = areas, urban = runif(A, 0.3, 0.6))
wstrata$rural <- 1 - wstrata$urban

both <- function(label, ...) {
  cat("\n=============", label, "=============\n")
  fi <- SUMMER::smoothSurvey(...)
  fs <- surveyPrev::smoothSurveyStan(...)
  list(i = fi, s = fs)
}
cmp <- function(si, ss, label, cols = c("mean", "var", "lower", "upper")) {
  key <- intersect(c("region", "strata"), colnames(si))
  oi <- do.call(order, si[key]); os <- do.call(order, ss[key])
  si <- si[oi, ]; ss <- ss[os, ]
  d <- vapply(cols, function(cc) max(abs(ss[[cc]] - si[[cc]]), na.rm = TRUE),
              numeric(1))
  cat(sprintf("%-34s %s  %s\n", label,
              paste(sprintf("max|d.%s|=%.4f", cols, d), collapse = "  "),
              ifelse(d[1] < 0.015, "OK", "** CHECK **")))
}

## ---------- J: unit-level binary + weight.strata ----------
rJ <- both("J: unit-level binary, stratified + weight.strata",
           data = udat, Amat = W, response.type = "binary",
           responseVar = "yvar", regionVar = "region",
           clusterVar = "clusterid", strataVar.within = "strata.ur",
           weight.strata = wstrata, is.unit.level = TRUE,
           CI = 0.95, save.draws = TRUE)
cmp(rJ$i$smooth, rJ$s$smooth, "J smooth (per strata)")
cmp(rJ$i$smooth.overall, rJ$s$smooth.overall, "J smooth.overall (aggregated)")

## ---------- K: unit-level gaussian nested-error ----------
rK <- both("K: unit-level gaussian nested",
           data = udat, Amat = W, response.type = "gaussian",
           responseVar = "gauss", regionVar = "region",
           is.unit.level = TRUE, CI = 0.95, save.draws = TRUE)
cmp(rK$i$smooth, rK$s$smooth, "K smooth")

## ---------- L: area-level binary, unmatched link ----------
rL <- both("L: area-level binary, unmatched.link",
           data = udat, Amat = W, response.type = "binary",
           unmatched.link = TRUE,
           responseVar = "yvar", regionVar = "region",
           strataVar = "strata.ur", weightVar = "weights",
           clusterVar = "~clusterid+hh",
           CI = 0.95, save.draws = TRUE)
cmp(rL$i$smooth, rL$s$smooth, "L smooth")

## ---------- M: smooth = FALSE parity ----------
mi <- SUMMER::smoothSurvey(data = udat, Amat = W,
                   response.type = "binary", responseVar = "yvar",
                   regionVar = "region", strataVar = "strata.ur",
                   weightVar = "weights", clusterVar = "~clusterid+hh",
                   CI = 0.95, smooth = FALSE)
ms <- surveyPrev::smoothSurveyStan(data = udat, Amat = W,
                   response.type = "binary", responseVar = "yvar",
                   regionVar = "region", strataVar = "strata.ur",
                   weightVar = "weights", clusterVar = "~clusterid+hh",
                   CI = 0.95, smooth = FALSE)
cat("\n============= M: smooth = FALSE =============\n")
cat(sprintf("M HT identical: %s\n",
            isTRUE(all.equal(mi$HT, ms$HT, tolerance = 1e-12))))

cat("\nALL DONE\n")
