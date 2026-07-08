## Exact posterior for the iid FH gaussian test via 1-D integration over sigma.
## Model: y_i ~ N(b0 + u_i, v_i) (i observed), u ~ N(0, sigma^2 I_R),
##        b0 ~ flat (N(0, 1e10)), sigma ~ Exp(lambda), lambda = -log(0.01)/1.
set.seed(5)
R <- 12; regs <- sprintf("r%02d", 1:R)
y.all <- 0.3 * scale(sin(1:R))[, 1] + rnorm(R, 0, 0.15)
v.all <- runif(R, 0.02, 0.08)
y.all[2] <- NA; v.all[2] <- NA
obs <- which(!is.na(y.all))
n <- length(obs)
lambda <- -log(0.01) / 1

## design: theta = (b0, u_1..u_R); y = A theta + e
A <- cbind(1, diag(R)[obs, , drop = FALSE])
B <- cbind(1, diag(R))                      # eta = B theta
Dinv <- diag(1 / v.all[obs])

post_given_sigma <- function(sigma) {
  P <- diag(c(1e-10, rep(1 / sigma^2, R)))
  Q <- t(A) %*% Dinv %*% A + P
  V <- solve(Q)
  m <- V %*% t(A) %*% Dinv %*% y.all[obs]
  ## log marginal likelihood pi(y | sigma) (up to const):
  ## -.5 log|Q| - .5 (y'D^-1 y - m'Qm) + .5 log|P| (proper part only)
  ll <- -0.5 * determinant(Q, logarithm = TRUE)$modulus +
        0.5 * (t(m) %*% Q %*% m) - 0.5 * sum(y.all[obs]^2 / v.all[obs]) +
        0.5 * R * log(1 / sigma^2)
  list(ll = as.numeric(ll), mean = as.numeric(B %*% m),
       var = diag(B %*% V %*% t(B)))
}

sig.grid <- seq(0.005, 2.5, length.out = 400)
res <- lapply(sig.grid, post_given_sigma)
logw <- sapply(res, `[[`, "ll") + dexp(sig.grid, lambda, log = TRUE)
w <- exp(logw - max(logw)); w <- w / sum(w)
Emean <- Reduce(`+`, Map(function(r, wi) wi * r$mean, res, w))
Evar  <- Reduce(`+`, Map(function(r, wi) wi * (r$var + r$mean^2), res, w)) - Emean^2

cat("Exact posterior (numerical integration):\n")
exact <- data.frame(region = regs, mean = round(Emean, 3), sd = round(sqrt(Evar), 3))
print(exact, row.names = FALSE)
cat("\nposterior mean of sigma:", round(sum(w * sig.grid), 4), "\n")
cat("\nReference values from previous run:\n")
cat("INLA (both strategies) means: 0.056 -0.038 -0.063 -0.088 -0.067 -0.070 0.029 0.101 0.016 -0.101 -0.110 -0.118\n")
cat("Stan (8000 draws)      means: 0.042 -0.035 -0.059 -0.077 -0.060 -0.065 0.020 0.080 0.008 -0.091 -0.096 -0.102\n")
cat("DONE\n")
