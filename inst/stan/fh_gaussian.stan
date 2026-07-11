// Gaussian smoothing model: rstan replacement for the INLA gaussian-family
// fits inside smoothSurvey(). Generalised to cover all gaussian branches:
//
//   * area-level Fay-Herriot, matched link (INLA: precision fixed at 1,
//     scale = 1/var_i):        y_i ~ N(eta_i, sd_i), sd_i known
//   * area-level unmatched link (control.family link="logit"):
//                              y_i ~ N(inv_logit(eta_i), sd_i), sd_i known
//   * unit-level nested error model (free observation variance with INLA's
//     default loggamma(1, 5e-5) prior on the precision)
//
//   eta = X beta + re[region],  re = iid or BYM2 with PC priors
//   (identical to betabinom.stan).
//
// Rows (data) and effects (regions) are decoupled: Nrow data rows index into
// R region effects; rows with missing response contribute nothing to the
// likelihood but their linear predictor is still produced (INLA's
// NA-response prediction).

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
  int<lower=1> Nrow;                    // data rows
  int<lower=1> R;                       // region effects (nrow(Amat))
  int<lower=1> P;                       // fixed-effect columns
  matrix[Nrow, P] Xall;                 // design matrix, one row per data row
  array[Nrow] int<lower=1, upper=R> region;

  int<lower=0> N;                       // rows entering the likelihood
  array[N] int<lower=1, upper=Nrow> obs_idx;
  vector[N] y;
  vector<lower=0>[N] sdv;               // known sd (ignored if free_prec)

  int<lower=0, upper=1> link_logit;     // mean = inv_logit(eta) instead of eta
  int<lower=0, upper=1> free_prec;      // estimate common obs precision
                                        // (INLA default loggamma(1, 5e-5))

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
  array[free_prec ? 1 : 0] real<lower=0> tau_obs;
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
  if (free_prec == 1) {
    target += gamma_lpdf(tau_obs[1] | 1, 5e-5);   // INLA default family prec
  }
  {
    vector[Nrow] eta = Xall * beta + re[region];
    vector[N] mu = link_logit == 1 ? inv_logit(eta[obs_idx]) : eta[obs_idx];
    if (free_prec == 1) {
      y ~ normal(mu, inv_sqrt(tau_obs[1]));
    } else {
      y ~ normal(mu, sdv);
    }
  }
}

generated quantities {
  vector[Nrow] eta = Xall * beta + re[region];
}
