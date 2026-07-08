## Lock-down validation: all remaining clusterModel branches, rstan vs INLA.
## Geography: 4 admin1 x 3 admin2 = 12 admin2 areas.
suppressMessages(library(rstan))
suppressMessages(library(dplyr))
options(mc.cores = 2); rstan_options(auto_write = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
set.seed(11)

## ---------- geography ----------
A1 <- 4; A2per <- 3
adm1 <- paste0("reg", LETTERS[1:A1])
adm2_tab <- expand.grid(admin2.name = paste0("d", 1:A2per), admin1.name = adm1,
                        stringsAsFactors = FALSE)[, 2:1]
adm2_tab <- adm2_tab[order(adm2_tab$admin1.name, adm2_tab$admin2.name), ]
adm2_tab$admin2.name.full <- paste0(adm2_tab$admin1.name, "_", adm2_tab$admin2.name)
R2 <- nrow(adm2_tab)                                   # 12

# chain adjacency over the sorted admin2 list (connected graph)
W2 <- matrix(0, R2, R2, dimnames = list(adm2_tab$admin2.name.full,
                                        adm2_tab$admin2.name.full))
for (i in 1:(R2 - 1)) { W2[i, i + 1] <- 1; W2[i + 1, i] <- 1 }
W1 <- matrix(0, A1, A1, dimnames = list(adm1, adm1))
for (i in 1:(A1 - 1)) { W1[i, i + 1] <- 1; W1[i + 1, i] <- 1 }

## ---------- truth ----------
alpha1 <- c(-0.8, -0.2, 0.3, 0.6)[match(adm2_tab$admin1.name, adm1)]  # admin1 intercepts
re2 <- as.numeric(scale(sin(1:R2)) * 0.4)
urban_eff <- 0.5
rho <- 0.08; s <- (1 - rho) / rho

## ---------- cluster-level data ----------
cl_per_adm2 <- 18
rows <- list(); cl <- 0
xc_cl <- numeric(0)                                    # unit covariate per cluster
for (a in 1:R2) for (k in 1:cl_per_adm2) {
  cl <- cl + 1
  st <- sample(c("urban", "rural"), 1, prob = c(0.35, 0.65))
  xc_cl[cl] <- rnorm(1, 0, 0.5)
  n <- sample(10:30, 1)
  eta <- alpha1[a] + re2[a] + ifelse(st == "urban", urban_eff, 0) + 0.3 * xc_cl[cl]
  p <- plogis(eta)
  cp <- rbeta(1, p * s, (1 - p) * s); succ <- rbinom(1, n, cp)
  rows[[cl]] <- data.frame(cluster = cl,
                           value = c(rep(1, succ), rep(0, n - succ)),
                           strata = st, householdID = paste0(cl, "_", seq_len(n)),
                           weight = 1, a2 = a)
}
df <- do.call(rbind, rows)
data <- df[, c("cluster", "value", "strata", "householdID", "weight")]
a2_of_cl <- df$a2[match(1:cl, df$cluster)]

cluster.info <- list(data = data.frame(
  cluster = 1:cl,
  admin1.name = adm2_tab$admin1.name[a2_of_cl],
  admin2.name.full = adm2_tab$admin2.name.full[a2_of_cl],
  LONGNUM = runif(cl, 30, 33), LATNUM = runif(cl, -12, -9)))

pop2 <- round(runif(R2, 3e4, 1e5))
admin.info2 <- list(
  data = data.frame(adm2_tab,
                    population = pop2,
                    population.admin1 = ave(pop2, adm2_tab$admin1.name, FUN = sum),
                    urban = runif(R2, 0.2, 0.6)),
  mat = W2)
admin.info1 <- list(
  data = data.frame(admin1.name = adm1,
                    population = round(runif(A1, 1e5, 3e5)),
                    urban = runif(A1, 0.25, 0.6)),
  mat = W1)

## area covariate (admin1)
X1 <- data.frame(admin1.name = adm1, xarea = as.numeric(scale(1:A1)))

## unit + pixel covariates
X.unit <- data.frame(cluster = 1:cl, xc = xc_cl)
pix <- do.call(rbind, lapply(1:R2, function(a) {
  data.frame(admin2.name.full = adm2_tab$admin2.name.full[a],
             admin1.name = adm2_tab$admin1.name[a],
             Population = runif(6, 500, 5000),
             xc = rnorm(6, 0, 0.5))
}))

## ---------- comparison helper ----------
both <- function(label, ...) {
  cat("\n=============", label, "=============\n")
  options(surveyPrev.engine = "INLA")
  fi <- clusterModel(...)
  options(surveyPrev.engine = "rstan")
  fs <- clusterModel(...)
  list(inla = fi, stan = fs)
}
cmp <- function(ri, rs, key, label) {
  if ("type" %in% colnames(ri)) { ri <- ri[ri$type == "full", ]; rs <- rs[rs$type == "full", ] }
  ri <- ri[order(ri[[key]]), ]; rs <- rs[order(rs[[key]]), ]
  dm <- max(abs(rs$mean - ri$mean)); ds <- max(abs(rs$sd - ri$sd))
  dl <- max(abs(rs$lower - ri$lower)); du <- max(abs(rs$upper - ri$upper))
  cat(sprintf("%-38s max|d.mean|=%.4f  max|d.sd|=%.4f  max|d.lo|=%.4f  max|d.up|=%.4f  %s\n",
              label, dm, ds, dl, du, ifelse(dm < 0.01, "OK", "** CHECK **")))
  invisible(c(dm, ds))
}

## ---------- A: admin2, bym2, unstratified, aggregation ----------
rA <- both("A: admin2 bym2 unstrat + aggregation",
           data = data, cluster.info = cluster.info, admin.info = admin.info2,
           admin = 2, model = "bym2", stratification = FALSE, aggregation = TRUE)
cmp(rA$inla$res.admin2, rA$stan$res.admin2, "admin2.name.full", "A res.admin2")
cmp(rA$inla$agg.admin1, rA$stan$agg.admin1, "admin1.name", "A agg.admin1")
cat(sprintf("A agg.natl: inla mean=%.4f stan mean=%.4f | inla sd=%.4f stan sd=%.4f\n",
            rA$inla$agg.natl$mean, rA$stan$agg.natl$mean,
            rA$inla$agg.natl$sd, rA$stan$agg.natl$sd))

## ---------- B: admin2, bym2, stratified, nested ----------
rB <- both("B: admin2 bym2 strat nested",
           data = data, cluster.info = cluster.info, admin.info = admin.info2,
           admin = 2, model = "bym2", stratification = TRUE, aggregation = FALSE,
           nested = TRUE)
cmp(rB$inla$res.admin2, rB$stan$res.admin2, "admin2.name.full", "B res.admin2 (full)")

## ---------- C: admin2, bym2, stratified, nested, interact ----------
rC <- both("C: admin2 bym2 strat nested interact",
           data = data, cluster.info = cluster.info, admin.info = admin.info2,
           admin = 2, model = "bym2", stratification = TRUE, aggregation = FALSE,
           nested = TRUE, interact = TRUE)
cmp(rC$inla$res.admin2, rC$stan$res.admin2, "admin2.name.full", "C res.admin2 (full)")

## ---------- D: admin1, iid, unstratified, area covariate X ----------
rD <- both("D: admin1 iid unstrat + area covariate",
           data = data, cluster.info = cluster.info, admin.info = admin.info1,
           admin = 1, model = "iid", stratification = FALSE, aggregation = FALSE,
           X = X1)
cmp(rD$inla$res.admin1, rD$stan$res.admin1, "admin1.name", "D res.admin1")

## ---------- E: admin2, bym2, unstratified, X.unit + X.pixel ----------
rE <- both("E: admin2 bym2 unstrat + X.unit/X.pixel",
           data = data, cluster.info = cluster.info, admin.info = admin.info2,
           admin = 2, model = "bym2", stratification = FALSE, aggregation = FALSE,
           X.unit = X.unit, X.pixel = pix)
cmp(rE$inla$res.admin2, rE$stan$res.admin2, "admin2.name.full", "E res.admin2")
cmp(rE$inla$res.admin1, rE$stan$res.admin1, "admin1.name", "E res.admin1 (agg)")
cat(sprintf("E res.admin0: inla mean=%.4f stan mean=%.4f | inla sd=%.4f stan sd=%.4f\n",
            rE$inla$res.admin0$mean, rE$stan$res.admin0$mean,
            rE$inla$res.admin0$sd, rE$stan$res.admin0$sd))

cat("\nALL DONE\n")
