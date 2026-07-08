## Validate stratified admin1 rstan vs INLA (tests strata naming + draws path).
suppressMessages(library(rstan))
options(mc.cores = 2); rstan_options(auto_write = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
set.seed(7)

A <- 6; areas <- paste0("area", 1:A)
W <- matrix(0, A, A, dimnames = list(areas, areas))
for (i in 1:(A - 1)) { W[i, i + 1] <- 1; W[i + 1, i] <- 1 }

intercept <- -0.3; urban_eff <- 0.5
re <- as.numeric(scale(cos(1:A)) * 0.5)
rho <- 0.08; s <- (1 - rho) / rho
rows <- list(); cl <- 0
for (a in 1:A) for (k in 1:30) {
  cl <- cl + 1
  st <- sample(c("urban", "rural"), 1, prob = c(0.4, 0.6))
  n <- sample(10:35, 1)
  p <- plogis(intercept + re[a] + ifelse(st == "urban", urban_eff, 0))
  cp <- rbeta(1, p * s, (1 - p) * s); succ <- rbinom(1, n, cp)
  rows[[cl]] <- data.frame(cluster = cl,
                           value = c(rep(1, succ), rep(0, n - succ)),
                           strata = st, householdID = paste0(cl, "_", seq_len(n)),
                           weight = 1, area = a)
}
df <- do.call(rbind, rows)
data <- df[, c("cluster", "value", "strata", "householdID", "weight")]
amap <- areas[df$area[match(1:cl, df$cluster)]]
cluster.info <- list(data = data.frame(cluster = 1:cl, admin1.name = amap,
                       admin2.name.full = paste0(amap, "_d1"),
                       LONGNUM = runif(cl, 30, 33), LATNUM = runif(cl, -12, -9)))
admin.info <- list(data = data.frame(admin1.name = areas,
                     population = round(runif(A, 5e4, 2e5)), urban = runif(A, 0.25, 0.6)),
                   mat = W)

run <- function(eng) {
  options(surveyPrev.engine = eng)
  clusterModel(data = data, cluster.info = cluster.info, admin.info = admin.info,
               admin = 1, model = "bym2", stratification = TRUE,
               aggregation = FALSE, CI = 0.95)
}
fi <- run("INLA"); fs <- run("rstan")
pick <- function(r) { x <- r$res.admin1[r$res.admin1$type == "full", ]; x[order(x$admin1.name), ] }
ri <- pick(fi); rs <- pick(fs)
out <- data.frame(area = ri$admin1.name,
  inla.mean = round(ri$mean, 4), stan.mean = round(rs$mean, 4),
  d.mean = round(rs$mean - ri$mean, 4),
  inla.sd = round(ri$sd, 4), stan.sd = round(rs$sd, 4),
  inla.lo = round(ri$lower, 4), stan.lo = round(rs$lower, 4),
  inla.up = round(ri$upper, 4), stan.up = round(rs$upper, 4))
cat("\n===== stratified admin1 (type=full) =====\n")
print(out, row.names = FALSE)
cat(sprintf("max|d.mean| = %.4f   max|d.sd| = %.4f\n",
            max(abs(out$d.mean)), max(abs(out$stan.sd - out$inla.sd))))
cat("DONE\n")
