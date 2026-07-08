// Gaussian Fay-Herriot smoothing model: rstan replacement for the INLA fit
// inside SUMMER::smoothSurvey() as used by surveyPrev::fhModel().
//
// Model (matches INLA exactly):
//   y_i    ~ Normal(eta_i, sd_i)        observation sd fixed and known
//                                       (INLA: gaussian family with precision
//                                        fixed at 1 and scale = 1/var_i)
//   eta    = X beta + re[region]
//   re     = iid or BYM2 with PC priors, identical to betabinom.stan.
//
// One eta per data row (a row per region, in the caller's row order);
// rows with missing y contribute nothing to the likelihood but their
// linear predictor is still drawn (INLA's NA-response prediction).

functions {
  real grid_lpdf(real x, vector gx, vector glp) {
    int K = num_elements(gx);
    real lo = gx[1];
    real hi = gx[K];
    real step = (hi - lo) / (K - 1);
    real pos = (x - lo) / step + 1;
    if (pos <= 1) return glp[1];
    if (pos >= K) return glp[K];
    int j = 1;
    while (j + 1 <= K && pos >= j + 1) j += 1;
    real w = pos - j;
    return (1 - w) * glp[j] + w * glp[j + 1];
  }
}

data {
  int<lower=1> R;                       // number of data rows (= regions)
  int<lower=1> P;                       // fixed-effect columns
  matrix[R, P] Xall;                    // design matrix, one row per region
  array[R] int<lower=1, upper=R> region;// region index (structured effect id)

  int<lower=0> N;                       // rows with observed y
  array[N] int<lower=1, upper=R> obs_idx;
  vector[N] y;                          // logit-scale direct estimates
  vector<lower=0>[N] sdv;               // known sd = sqrt(HT.logit.var)

  vector<lower=0>[P] beta_prec;         // 0 => flat prior (intercept)
  real<lower=0> lambda;                 // PC prec prior: sigma ~ Exp(lambda)

  int<lower=0, upper=1> use_bym2;
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=R> node1;
  array[N_edges] int<lower=1, upper=R> node2;
  real<lower=0> scaling_factor;
  int<lower=2> Kphi;
  vector[Kphi] phi_grid;
  vector[Kphi] phi_logp;
}

parameters {
  vector[P] beta;
  real<lower=0> sigma;
  vector[R] v;
  vector[use_bym2 ? R : 0] u;
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
  for (p in 1:P) {
    if (beta_prec[p] > 0) {
      target += normal_lpdf(beta[p] | 0, inv_sqrt(beta_prec[p]));
    }
  }
  target += exponential_lpdf(sigma | lambda);
  v ~ normal(0, 1);
  if (use_bym2 == 1) {
    target += -0.5 * dot_self(u[node1] - u[node2]);
    sum(u) ~ normal(0, 0.001 * R);
    target += grid_lpdf(phi[1] | phi_grid, phi_logp);
  }
  {
    vector[R] eta = Xall * beta + re[region];
    y ~ normal(eta[obs_idx], sdv);
  }
}

generated quantities {
  vector[R] eta = Xall * beta + re[region];
}
