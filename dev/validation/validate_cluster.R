## Validate rstan clusterModel against INLA on simulated inputs (iid + bym2,
## admin1, unstratified). Run from the package root with devtools installed.
suppressMessages({
  library(rstan)
})
options(mc.cores = 2)
rstan_options(auto_write = TRUE)

pkg <- "."
suppressMessages(devtools::load_all(pkg, quiet = TRUE))

set.seed(42)

## ---- build a small admin1-level example ----
A <- 8
areas <- paste0("area", 1:A)
# chain adjacency 1-2-3-...-8
W <- matrix(0, A, A, dimnames = list(areas, areas))
for (i in 1:(A - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }

intercept <- -0.5
true_re <- as.numeric(scale(sin(1:A)) * 0.6)      # arbitrary area effects
p_area <- plogis(intercept + true_re)
rho <- 0.10                                       # overdispersion (ICC)
s <- (1 - rho) / rho

clusters_per_area <- 25
rows <- list()
cl <- 0
for (a in 1:A) {
  for (k in 1:clusters_per_area) {
    cl <- cl + 1
    n <- sample(10:40, 1)
    cp <- rbeta(1, p_area[a] * s, (1 - p_area[a]) * s)
    succ <- rbinom(1, n, cp)
    # expand to individual rows (clusterModel aggregates by cluster)
    rows[[cl]] <- data.frame(
      cluster = cl,
      value = c(rep(1, succ), rep(0, n - succ)),
      strata = sample(c("urban", "rural"), 1),
      householdID = paste0(cl, "_", seq_len(n)),
      weight = 1,
      area = a
    )
  }
}
dat_full <- do.call(rbind, rows)

data <- dat_full[, c("cluster", "value", "strata", "householdID", "weight")]

cluster.info <- list(data = data.frame(
  cluster = 1:cl,
  admin1.name = areas[dat_full$area[match(1:cl, dat_full$cluster)]],
  admin2.name.full = paste0(areas[dat_full$area[match(1:cl, dat_full$cluster)]], "_d1"),
  LONGNUM = runif(cl, 30, 33),
  LATNUM = runif(cl, -12, -9)
))

admin.info <- list(
  data = data.frame(
    admin1.name = areas,
    population = round(runif(A, 5e4, 2e5)),
    urban = runif(A, 0.2, 0.6)
  ),
  mat = W
)

compare <- function(model) {
  cat("\n==================  model =", model, " ==================\n")
  options(surveyPrev.engine = "INLA")
  fi <- clusterModel(data = data, cluster.info = cluster.info,
                     admin.info = admin.info, admin = 1, model = model,
                     stratification = FALSE, aggregation = FALSE, CI = 0.95)
  options(surveyPrev.engine = "rstan")
  fs <- clusterModel(data = data, cluster.info = cluster.info,
                     admin.info = admin.info, admin = 1, model = model,
                     stratification = FALSE, aggregation = FALSE, CI = 0.95)

  ri <- fi$res.admin1[order(fi$res.admin1$admin1.name), ]
  rs <- fs$res.admin1[order(fs$res.admin1$admin1.name), ]
  out <- data.frame(
    area = ri$admin1.name,
    inla.mean = round(ri$mean, 4), stan.mean = round(rs$mean, 4),
    d.mean = round(rs$mean - ri$mean, 4),
    inla.sd = round(ri$sd, 4), stan.sd = round(rs$sd, 4),
    inla.lo = round(ri$lower, 4), stan.lo = round(rs$lower, 4),
    inla.up = round(ri$upper, 4), stan.up = round(rs$upper, 4)
  )
  print(out, row.names = FALSE)
  cat(sprintf("max|d.mean| = %.4f   max|d.sd| = %.4f\n",
              max(abs(out$d.mean)), max(abs(out$stan.sd - out$inla.sd))))
  invisible(list(inla = fi, stan = fs))
}

res_iid  <- compare("iid")
res_bym2 <- compare("bym2")
cat("\nDONE\n")
