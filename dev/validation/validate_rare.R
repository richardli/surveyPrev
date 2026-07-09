## Rare binary outcome stress test: INLA vs rstan vs exact posterior.
##
## Hypotheses tested:
##  (1) Cluster-level beta-binomial with rare outcomes (prev ~2% and ~0.5%):
##      INLA's approximations may degrade (skewed low-count likelihoods).
##      Arbitrated against the EXACT posterior: for the iid model, given
##      (b0, sigma, rho) the areas factorize, so each area only needs 1-D
##      Gauss-Hermite quadrature and the hyperparameters a 3-D grid.
##  (2) FH smoothed-direct on the same data: the logit-normal approximation
##      of the direct estimates fails at low counts for BOTH engines equally
##      (engine parity, model failure) -- zero-case areas drop out entirely.
##  (3) Cluster model should track the truth far better than FH here.
suppressMessages(library(rstan))
suppressMessages(library(dplyr))
options(mc.cores = 2); rstan_options(auto_write = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))

## ---------------- simulation ----------------
A <- 8; areas <- sprintf("area%02d", 1:A)
W <- matrix(0, A, A, dimnames = list(areas, areas))
for (i in 1:(A - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }

sim_rare <- function(p0, seed) {
  set.seed(seed)
  b0 <- qlogis(p0)
  u <- as.numeric(scale(sin(1:A))) * 0.4
  p_area <- plogis(b0 + u)                      # truth
  rho <- 0.10; s <- (1 - rho) / rho
  rows <- list(); cl <- 0
  for (a in 1:A) for (k in 1:20) {
    cl <- cl + 1
    n <- sample(15:35, 1)
    cp <- rbeta(1, p_area[a] * s, (1 - p_area[a]) * s)
    y <- rbinom(1, n, cp)
    rows[[cl]] <- data.frame(cluster = cl,
                             value = c(rep(1, y), rep(0, n - y)),
                             strata = sample(c("urban", "rural"), 1),
                             householdID = paste0(cl, "_", seq_len(n)),
                             weight = 1, area = a)
  }
  df <- do.call(rbind, rows)
  list(data = df[, 1:5],
       cluster.info = list(data = data.frame(
         cluster = 1:cl, admin1.name = areas[df$area[match(1:cl, df$cluster)]],
         admin2.name.full = paste0(areas[df$area[match(1:cl, df$cluster)]], "_d"),
         LONGNUM = runif(cl, 30, 33), LATNUM = runif(cl, -12, -9))),
       admin.info = list(data = data.frame(admin1.name = areas,
                           population = rep(1e5, A), urban = rep(0.3, A)),
                         mat = W),
       truth = p_area,
       agg = df %>% group_by(cluster) %>%
         summarise(area = first(area), y = sum(value), n = n(), .groups = "drop"))
}

## ---------------- exact posterior (iid model) ----------------
gauss_hermite <- function(K) {
  i <- 1:(K - 1); a <- sqrt(i / 2)
  J <- matrix(0, K, K); J[cbind(i, i + 1)] <- a; J[cbind(i + 1, i)] <- a
  e <- eigen(J, symmetric = TRUE)
  list(nodes = e$values, weights = sqrt(pi) * e$vectors[1, ]^2)
}
logsumexp <- function(x) { m <- max(x); m + log(sum(exp(x - m))) }

exact_rare <- function(agg, pc.u = 1, pc.alpha = 0.01,
                       od.mean = 0, od.prec = 0.4,
                       nb0 = 35, ns = 30, nr = 30, K = 30) {
  lambda <- -log(pc.alpha) / pc.u
  gh <- gauss_hermite(K); z <- gh$nodes; wl <- log(gh$weights / sqrt(pi))
  phat <- max(sum(agg$y) / sum(agg$n), 1e-4)
  b0g <- seq(qlogis(phat) - 2.5, qlogis(phat) + 2.0, length.out = nb0)
  sg  <- exp(seq(log(0.02), log(1.6), length.out = ns))
  thg <- seq(-5, 1, length.out = nr)               # logit(rho)
  ## p values for all (b0, sigma, node): dim nb0 x ns x K
  ## linear predictor b0 + sigma * sqrt(2) * z  (inner outer is the product
  ## sigma*z, the outer one must be a SUM)
  Pmat <- array(plogis(outer(b0g, outer(sg, sqrt(2) * z), "+")), c(nb0, ns, K))
  pvec <- as.vector(Pmat)                           # length nb0*ns*K
  Areas <- sort(unique(agg$area)); R <- length(Areas)
  G <- length(pvec)
  lp_grid <- array(NA_real_, c(nb0, ns, nr))
  Emean <- matrix(0, R, 1); E2 <- matrix(0, R, 1)
  ## store conditional area moments per grid point on the fly
  cond_m <- array(NA_real_, c(R, nb0, ns, nr))
  cond_m2 <- array(NA_real_, c(R, nb0, ns, nr))
  for (r in seq_len(nr)) {
    rho <- plogis(thg[r]); s <- (1 - rho) / rho
    a1 <- pvec * s; b1 <- (1 - pvec) * s
    LLa <- matrix(0, R, G)                          # area loglik at each p
    for (c in seq_len(nrow(agg))) {
      yc <- agg$y[c]; nc <- agg$n[c]; ar <- match(agg$area[c], Areas)
      LLa[ar, ] <- LLa[ar, ] + lchoose(nc, yc) +
        lbeta(yc + a1, nc - yc + b1) - lbeta(a1, b1)
    }
    dim(LLa) <- c(R, nb0, ns, K)
    for (i in seq_len(nb0)) for (j in seq_len(ns)) {
      lva <- LLa[, i, j, ] + rep(wl, each = R)      # R x K
      lm_a <- apply(lva, 1, logsumexp)              # log m_a
      lp_grid[i, j, r] <- sum(lm_a) +
        dexp(sg[j], lambda, log = TRUE) + log(sg[j]) +   # jacobian for log grid
        dnorm(thg[r], od.mean, 1 / sqrt(od.prec), log = TRUE)
      v <- exp(lva - lm_a)                          # R x K conditional weights
      pk <- Pmat[i, j, ]                            # K
      cond_m[, i, j, r]  <- v %*% pk
      cond_m2[, i, j, r] <- v %*% pk^2
    }
  }
  Wt <- exp(lp_grid - max(lp_grid)); Wt <- Wt / sum(Wt)
  post_mean <- post_sd <- numeric(R)
  for (a in seq_len(R)) {
    m1 <- sum(Wt * cond_m[a, , , ]); m2 <- sum(Wt * cond_m2[a, , , ])
    post_mean[a] <- m1; post_sd[a] <- sqrt(max(m2 - m1^2, 0))
  }
  data.frame(area = Areas, mean = post_mean, sd = post_sd)
}

## ---------------- runners ----------------
run_cluster <- function(sim, engine, model = "iid") {
  options(surveyPrev.engine = engine)
  fit <- clusterModel(data = sim$data, cluster.info = sim$cluster.info,
                      admin.info = sim$admin.info, admin = 1, model = model,
                      stratification = FALSE, aggregation = FALSE, CI = 0.95)
  r <- fit$res.admin1[order(fit$res.admin1$admin1.name), ]
  r
}
run_fh <- function(sim, engine) {
  options(surveyPrev.engine = engine)
  fit <- fhModel(data = sim$data, cluster.info = sim$cluster.info,
                 admin.info = sim$admin.info, admin = 1, model = "iid",
                 aggregation = FALSE, CI = 0.95)
  fit$res.admin1[order(fit$res.admin1$admin1.name), ]
}

scenario <- function(p0, seed, label) {
  cat("\n##############", label, sprintf("(true base prev %.3f)", p0), "##############\n")
  sim <- sim_rare(p0, seed)
  ycount <- tapply(sim$agg$y, sim$agg$area, sum)
  cat("cases per area:", paste(ycount, collapse = " "), "\n")

  ex <- exact_rare(sim$agg)
  ci <- run_cluster(sim, "INLA")
  cs <- run_cluster(sim, "rstan")

  tab <- data.frame(area = areas, truth = round(sim$truth, 4),
                    cases = as.integer(ycount),
                    exact = round(ex$mean, 4),
                    inla = round(ci$mean, 4), stan = round(cs$mean, 4),
                    exact.sd = round(ex$sd, 4),
                    inla.sd = round(ci$sd, 4), stan.sd = round(cs$sd, 4))
  print(tab, row.names = FALSE)
  cat(sprintf("cluster model, max|err vs exact|: INLA mean=%.4f sd=%.4f | stan mean=%.4f sd=%.4f\n",
              max(abs(ci$mean - ex$mean)), max(abs(ci$sd - ex$sd)),
              max(abs(cs$mean - ex$mean)), max(abs(cs$sd - ex$sd))))
  cat(sprintf("cluster model, RELATIVE max err (mean): INLA=%.1f%% | stan=%.1f%%\n",
              100 * max(abs(ci$mean - ex$mean) / ex$mean),
              100 * max(abs(cs$mean - ex$mean) / ex$mean)))

  ## FH comparison
  fi <- tryCatch(run_fh(sim, "INLA"), error = function(e) e)
  fs <- tryCatch(run_fh(sim, "rstan"), error = function(e) e)
  if (inherits(fi, "error") || inherits(fs, "error")) {
    cat("FH failed:", if (inherits(fi, "error")) conditionMessage(fi) else conditionMessage(fs), "\n")
  } else {
    cat(sprintf("FH engine parity:  max|inla-stan| mean=%.4f sd=%.4f\n",
                max(abs(fi$mean - fs$mean), na.rm = TRUE),
                max(abs(fi$sd - fs$sd), na.rm = TRUE)))
    mae <- function(x) mean(abs(x - sim$truth), na.rm = TRUE)
    cat(sprintf("MAE vs truth:  cluster/stan=%.4f  cluster/INLA=%.4f  FH/stan=%.4f  FH/INLA=%.4f\n",
                mae(cs$mean), mae(ci$mean), mae(fs$mean), mae(fi$mean)))
    cat(sprintf("FH dropped areas (no cases -> logit undefined): %d of %d\n",
                sum(ycount == 0), A))
  }
  invisible(list(sim = sim, exact = ex, ci = ci, cs = cs))
}

r2  <- scenario(0.020, seed = 101, label = "PREV ~2%")
r05 <- scenario(0.005, seed = 202, label = "PREV ~0.5%")

## ---------------- bym2 spot check at the rare level ----------------
cat("\n############## PREV ~0.5%, bym2 (no exact; stan validated above) ##############\n")
ci2 <- run_cluster(r05$sim, "INLA", model = "bym2")
cs2 <- run_cluster(r05$sim, "rstan", model = "bym2")
cat(sprintf("bym2: max|inla-stan| mean=%.4f sd=%.4f | rel mean diff=%.1f%%\n",
            max(abs(ci2$mean - cs2$mean)), max(abs(ci2$sd - cs2$sd)),
            100 * max(abs(ci2$mean - cs2$mean) / cs2$mean)))

cat("\nALL DONE\n")
