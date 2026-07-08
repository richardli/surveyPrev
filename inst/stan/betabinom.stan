// Beta-binomial area/cluster model: rstan replacement for the INLA fit used in
// surveyPrev::clusterModel(). Supports IID or BYM2 area random effects, an
// arbitrary fixed-effects design matrix (intercept / strata / nested admin1
// intercepts / interactions / covariates), PC priors on the precision and the
// BYM2 mixing parameter, and the INLA beta-binomial overdispersion (ICC).
//
// Parameterisation matches INLA exactly:
//   p_i      = inv_logit(eta_i),  eta_i = X beta + re[region_i]
//   rho      = inv_logit(logit_rho)            (overdispersion = ICC)
//   a_i      = p_i (1 - rho) / rho
//   b_i      = (1 - p_i)(1 - rho) / rho
//   y_i ~ BetaBinomial(Ntrials_i, a_i, b_i)
//
// IID:  re = sigma * v
// BYM2: re = sigma * ( sqrt(1 - phi) * v + sqrt(phi / scaling_factor) * u )
//       u = scaled ICAR structured effect, v = iid.

functions {
  // Linear interpolation of a log-density supplied on an equally spaced,
  // ascending grid. Used to pass INLA's KLD-based PC prior for the BYM2
  // mixing parameter phi into Stan (unnormalised log-density is sufficient).
  real grid_lpdf(real x, vector gx, vector glp) {
    int K = num_elements(gx);
    real lo = gx[1];
    real hi = gx[K];
    real step = (hi - lo) / (K - 1);
    real pos = (x - lo) / step + 1;     // fractional index, 1-based
    if (pos <= 1) return glp[1];
    if (pos >= K) return glp[K];
    int j = 1;
    while (j + 1 <= K && pos >= j + 1) j += 1;
    real w = pos - j;                   // in [0, 1)
    return (1 - w) * glp[j] + w * glp[j + 1];
  }
}

data {
  // --- likelihood rows (observed cluster aggregates) ---
  int<lower=0> N;                       // number of observed rows
  int<lower=1> P;                       // number of fixed-effect columns
  matrix[N, P] X;                       // fixed-effects design (observed rows)
  array[N] int<lower=0> y;              // successes
  array[N] int<lower=0> Ntrials;        // trials
  int<lower=1> R;                       // number of areas/regions
  array[N] int<lower=1, upper=R> region;

  // --- prediction rows (one per area; for summary.fitted.values) ---
  int<lower=0> Npred;
  matrix[Npred, P] Xpred;
  array[Npred] int<lower=1, upper=R> region_pred;

  // --- fixed-effect priors (INLA: intercept flat, others prec 0.001) ---
  vector<lower=0>[P] beta_prec;         // 0 => improper flat prior

  // --- overdispersion prior: logit(rho) ~ Normal(od_mean, od_sd) ---
  real od_mean;
  real<lower=0> od_sd;

  // --- precision PC prior: sigma ~ Exponential(lambda) ---
  real<lower=0> lambda;

  // --- model selector ---
  int<lower=0, upper=1> use_bym2;

  // --- BYM2 structure (ignored when use_bym2 == 0) ---
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=R> node1;
  array[N_edges] int<lower=1, upper=R> node2;
  real<lower=0> scaling_factor;

  // --- BYM2 phi PC prior grid (ignored when use_bym2 == 0) ---
  int<lower=2> Kphi;
  vector[Kphi] phi_grid;
  vector[Kphi] phi_logp;
}

parameters {
  vector[P] beta;
  real logit_rho;
  real<lower=0> sigma;                  // total sd of the area effect
  vector[R] v;                          // iid (or unstructured part of bym2)
  vector[use_bym2 ? R : 0] u;           // structured ICAR part (bym2 only)
  array[use_bym2 ? 1 : 0] real<lower=0, upper=1> phi;
}

transformed parameters {
  vector[R] re;
  if (use_bym2 == 1) {
    re = sigma * (sqrt(1 - phi[1]) * v + sqrt(phi[1] / scaling_factor) * u);
  } else {
    re = sigma * v;
  }
}

model {
  // fixed-effect priors (flat where beta_prec == 0)
  for (p in 1:P) {
    if (beta_prec[p] > 0) {
      target += normal_lpdf(beta[p] | 0, inv_sqrt(beta_prec[p]));
    }
  }

  // overdispersion + precision priors
  logit_rho ~ normal(od_mean, od_sd);
  target += exponential_lpdf(sigma | lambda);

  // random-effect structure
  v ~ normal(0, 1);
  if (use_bym2 == 1) {
    // scaled ICAR (improper); soft sum-to-zero constraint
    target += -0.5 * dot_self(u[node1] - u[node2]);
    sum(u) ~ normal(0, 0.001 * R);
    target += grid_lpdf(phi[1] | phi_grid, phi_logp);
  }

  // beta-binomial likelihood (INLA parameterisation)
  {
    real rho = inv_logit(logit_rho);
    real s = (1 - rho) / rho;
    vector[N] eta = X * beta + re[region];
    for (n in 1:N) {
      real pp = inv_logit(eta[n]);
      y[n] ~ beta_binomial(Ntrials[n], pp * s, (1 - pp) * s);
    }
  }
}

generated quantities {
  // fitted probability for each prediction row (-> summary.fitted.values)
  vector[Npred] fitted;
  {
    vector[Npred] etap = Xpred * beta + re[region_pred];
    for (m in 1:Npred) fitted[m] = inv_logit(etap[m]);
  }
}
