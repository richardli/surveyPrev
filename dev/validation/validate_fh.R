## Validate the rstan smoothed-direct (Fay-Herriot) port against
## SUMMER::smoothSurvey/INLA, on simulated inputs.
## Scenarios:
##   F: fhModel admin1 bym2, aggregation=TRUE          (binary svy mode)
##   G: fhModel admin1 iid + area covariate X          (binary + merge/covariate)
##   H: smoothSurvey gaussian direct.est mode, bym2    (isolated engine test)
##   I: fhModel admin2 bym2 nested=TRUE                (factor covariate admin1.name)
##
## Note on H: with few regions the INLA and rstan answers differ slightly
## (logit-scale means by up to ~0.02, with INLA showing systematically less
## shrinkage). Brute-force numerical integration (validate_fh_exact.R) shows
## the exact posterior matches the rstan result to within MC error; the gap
## is INLA's hyperparameter-integration approximation error in the
## small-data regime, not a porting bug.
suppressMessages(library(rstan))
suppressMessages(library(dplyr))
options(mc.cores = 2); rstan_options(auto_write = TRUE)
suppressMessages(devtools::load_all(".", quiet = TRUE))
set.seed(23)

## ---------- geography: 4 admin1 x 3 admin2 ----------
A1 <- 4; adm1 <- paste0("reg", LETTERS[1:A1])
adm2_tab <- expand.grid(admin2.name = paste0("d", 1:3), admin1.name = adm1,
                        stringsAsFactors = FALSE)[, 2:1]
adm2_tab <- adm2_tab[order(adm2_tab$admin1.name, adm2_tab$admin2.name), ]
adm2_tab$admin2.name.full <- paste0(adm2_tab$admin1.name, "_", adm2_tab$admin2.name)
R2 <- nrow(adm2_tab)
W2 <- matrix(0, R2, R2, dimnames = list(adm2_tab$admin2.name.full,
                                        adm2_tab$admin2.name.full))
for (i in 1:(R2 - 1)) { W2[i, i + 1] <- 1; W2[i + 1, i] <- 1 }
W1 <- matrix(0, A1, A1, dimnames = list(adm1, adm1))
for (i in 1:(A1 - 1)) { W1[i, i + 1] <- 1; W1[i + 1, i] <- 1 }

## ---------- simulated survey ----------
alpha1 <- c(-0.8, -0.2, 0.3, 0.6)[match(adm2_tab$admin1.name, adm1)]
re2 <- as.numeric(scale(sin(1:R2)) * 0.4)
rows <- list(); cl <- 0
for (a in 1:R2) for (k in 1:18) {
  cl <- cl + 1
  st <- sample(c("urban", "rural"), 1, prob = c(0.35, 0.65))
  n <- sample(15:35, 1)
  p <- plogis(alpha1[a] + re2[a] + ifelse(st == "urban", 0.5, 0))
  cp <- rbeta(1, p * 11.5, (1 - p) * 11.5)
  succ <- rbinom(1, n, cp)
  rows[[cl]] <- data.frame(cluster = cl,
                           value = c(rep(1, succ), rep(0, n - succ)),
                           strata = st, householdID = paste0(cl, "_", seq_len(n)),
                           weight = runif(1, 0.5, 2), a2 = a)
}
df <- do.call(rbind, rows)
data <- df[, c("cluster", "value", "strata", "householdID", "weight")]
a2_of_cl <- df$a2[match(1:cl, df$cluster)]
cluster.info <- list(data = data.frame(
  cluster = 1:cl,
  admin1.name = adm2_tab$admin1.name[a2_of_cl],
  admin2.name.full = adm2_tab$admin2.name.full[a2_of_cl],
  LONGNUM = runif(cl, 30, 33), LATNUM = runif(cl, -12, -9)))
admin.info1 <- list(data = data.frame(admin1.name = adm1,
                      population = round(runif(A1, 1e5, 3e5)),
                      urban = runif(A1, 0.25, 0.6)),
                    mat = W1)
pop2 <- round(runif(R2, 3e4, 1e5))
admin.info2 <- list(data = data.frame(adm2_tab, population = pop2,
                      population.admin1 = ave(pop2, adm2_tab$admin1.name, FUN = sum),
                      urban = runif(R2, 0.2, 0.6)),
                    mat = W2)
X1 <- data.frame(admin1.name = adm1, xarea = as.numeric(scale(1:A1)))

## ---------- helpers ----------
both_fh <- function(label, ...) {
  cat("\n=============", label, "=============\n")
  options(surveyPrev.engine = "INLA");  fi <- fhModel(...)
  options(surveyPrev.engine = "rstan"); fs <- fhModel(...)
  list(inla = fi, stan = fs)
}
cmp <- function(ri, rs, key, label) {
  ri <- ri[order(ri[[key]]), ]; rs <- rs[order(rs[[key]]), ]
  dm <- max(abs(rs$mean - ri$mean), na.rm = TRUE)
  ds <- max(abs(rs$sd - ri$sd), na.rm = TRUE)
  dl <- max(abs(rs$lower - ri$lower), na.rm = TRUE)
  du <- max(abs(rs$upper - ri$upper), na.rm = TRUE)
  cat(sprintf("%-30s max|d.mean|=%.4f  max|d.sd|=%.4f  max|d.lo|=%.4f  max|d.up|=%.4f  %s\n",
              label, dm, ds, dl, du, ifelse(dm < 0.015, "OK", "** CHECK **")))
}

## ---------- F: admin1 bym2, aggregation ----------
rF <- both_fh("F: fhModel admin1 bym2 + agg",
              data = data, cluster.info = cluster.info,
              admin.info = admin.info1, admin = 1, model = "bym2",
              aggregation = TRUE, CI = 0.95)
cmp(rF$inla$res.admin1, rF$stan$res.admin1, "admin1.name", "F res.admin1")
cat(sprintf("F agg.natl: inla mean=%.4f stan mean=%.4f | inla sd=%.4f stan sd=%.4f\n",
            rF$inla$agg.natl$mean, rF$stan$agg.natl$mean,
            rF$inla$agg.natl$sd, rF$stan$agg.natl$sd))

## ---------- G: admin1 iid + covariate ----------
rG <- both_fh("G: fhModel admin1 iid + X",
              data = data, cluster.info = cluster.info,
              admin.info = admin.info1, admin = 1, model = "iid",
              X = X1, aggregation = FALSE, CI = 0.95)
cmp(rG$inla$res.admin1, rG$stan$res.admin1, "admin1.name", "G res.admin1")

## ---------- H: isolated gaussian direct.est mode ----------
cat("\n============= H: smoothSurvey gaussian direct.est bym2 =============\n")
set.seed(5)
dl <- data.frame(region = adm1,
                 HT.logit.est = alpha1[match(adm1, adm2_tab$admin1.name)] + rnorm(A1, 0, 0.15),
                 HT.logit.var = runif(A1, 0.02, 0.08))
dl$HT.logit.est[2] <- NA; dl$HT.logit.var[2] <- NA   # one missing region
hi <- SUMMER::smoothSurvey(data = NULL, direct.est = dl, Amat = W1,
                           regionVar = "region", responseVar = "HT.logit.est",
                           direct.est.var = "HT.logit.var",
                           response.type = "gaussian", CI = 0.95,
                           smooth = TRUE, save.draws = TRUE)
hs <- surveyPrev::smoothSurveyStan(data = NULL, direct.est = dl, Amat = W1,
                        regionVar = "region", responseVar = "HT.logit.est",
                        direct.est.var = "HT.logit.var",
                        response.type = "gaussian", CI = 0.95,
                        smooth = TRUE, save.draws = TRUE)
si <- hi$smooth[order(hi$smooth$region), ]
ss <- hs$smooth[order(hs$smooth$region), ]
cat(sprintf("H smooth (logit scale):        max|d.mean|=%.4f  max|d.var|=%.4f  max|d.lo|=%.4f  max|d.up|=%.4f\n",
            max(abs(ss$mean - si$mean)), max(abs(ss$var - si$var)),
            max(abs(ss$lower - si$lower)), max(abs(ss$upper - si$upper))))
di <- expit(t(hi$draws.est[, -c(1, 2)])); ds <- expit(t(hs$draws.est[, -c(1, 2)]))
cat(sprintf("H draws.est expit col-means:   max|d|=%.4f\n",
            max(abs(colMeans(ds) - colMeans(di)))))

## ---------- I: admin2 bym2 nested ----------
rI <- both_fh("I: fhModel admin2 bym2 nested",
              data = data, cluster.info = cluster.info,
              admin.info = admin.info2, admin = 2, model = "bym2",
              nested = TRUE, aggregation = FALSE, CI = 0.95)
key2 <- "admin2.name.full"
cmp(rI$inla$res.admin2, rI$stan$res.admin2, key2, "I res.admin2")

cat("\nALL DONE\n")
