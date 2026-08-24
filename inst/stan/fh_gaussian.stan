functions {
  // Linear interpolation of a log prior tabulated on the phi scale in R.
  real interpolate_lpdf(real x, vector gx, vector glp) {
    int K = num_elements(gx);
    int lo = 1;
    int hi = K;
    if (x <= gx[1]) return glp[1];
    if (x >= gx[K]) return glp[K];
    while (hi - lo > 1) {
      int mid = (lo + hi) / 2;
      if (x >= gx[mid]) lo = mid;
      else hi = mid;
    }
    return glp[lo] + (x - gx[lo]) / (gx[hi] - gx[lo]) *
      (glp[hi] - glp[lo]);
  }

  // Analytic BYM2 PC prior: pi(phi) = lambda exp(-lambda d(phi)) |d'(phi)|.
  // gamma and f summarize the scaled graph spectrum used to compute d(phi).
  real pc_bym2_phi_lpdf(real phi, vector gamma, real f, real lambda) {
    int n = num_elements(gamma);
    real base = n * f - sum(gamma);
    real d2 = phi * base;
    real numerator = base;
    for (i in 1:n) {
      d2 -= log1p(phi * gamma[i]) - phi * gamma[i];
      numerator += phi * square(gamma[i]) / (1 + phi * gamma[i]);
    }
    if (d2 <= 0 || numerator <= 0) return negative_infinity();
    return log(lambda) - lambda * sqrt(d2) + log(numerator) -
      log(2) - 0.5 * log(d2);
  }
}

data {
  // All prediction rows and the subset with an observed Gaussian response.
  int<lower=1> Nrow;
  int<lower=1> R;
  int<lower=1> P;
  matrix[Nrow, P] Xall;
  array[Nrow] int<lower=1, upper=R> region;

  int<lower=0> N;
  array[N] int<lower=1, upper=Nrow> obs_idx;
  vector[N] y;
  vector<lower=0>[N] sdv;
  int<lower=0, upper=1> link_logit;
  int<lower=0, upper=1> free_prec;

  // beta_prec controls fixed priors; lambda is the PC prior rate for SDs.
  vector<lower=0>[P] beta_prec;
  real<lower=0> lambda;

  // Spatial encoding and graph representation shared with betabinom.stan.
  int<lower=0, upper=2> space_model; // 0 IID, 1 BYM2, 2 SAR
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=R> node1;
  array[N_edges] int<lower=1, upper=R> node2;
  int<lower=1> N_components;
  array[R] int<lower=1, upper=N_components> component;
  array[N_components] int<lower=1> component_size;
  array[R] int<lower=0, upper=1> component_reference;
  int<lower=0> N_singletons;
  array[N_singletons] int<lower=1, upper=R> singleton;
  vector<lower=0>[R] node_scale;
  matrix[R, R] W_sar;

  // BYM2 phi prior: 1 interpolates an INLA table; 2 uses the graph spectrum.
  int<lower=1, upper=2> phi_prior_type; // 1 INLA table, 2 spectral
  int<lower=2> Kphi;
  vector[Kphi] phi_grid;
  vector[Kphi] phi_logp;
  vector[R] phi_gamma;
  real phi_f;
  real<lower=0> phi_lambda;

  // Temporal main effects and Knorr-Held interaction type.
  int<lower=0, upper=1> has_time;
  int<lower=1> T;
  array[Nrow] int<lower=1, upper=T> time;
  int<lower=1, upper=2> time_model;
  real<lower=0> time_scale;
  int<lower=0, upper=4> st_type; // 1 IIDxIID, 2 IIDxRW, 3 BesagxIID, 4 BesagxRW
  int<lower=0, upper=1> constr;
}

parameters {
  // v_space is IID; u_space is the structured ICAR part used only by BYM2.
  vector[P] beta;
  real<lower=0> sigma_space;
  vector[R] v_space;
  vector[space_model == 1 ? R : 0] u_space;
  array[space_model == 1 ? 1 : 0] real<lower=0, upper=1> phi;
  array[space_model == 2 ? 1 : 0] real rho_raw;

  // Zero-length declarations switch unused temporal/interaction terms off.
  array[has_time ? 1 : 0] real<lower=0> sigma_time_unstruct;
  array[has_time ? 1 : 0] real<lower=0> sigma_time_struct;
  vector[has_time ? T : 0] z_time_unstruct;
  vector[has_time ? T : 0] u_time;
  // The constrained field carries RW2 curvature. The separate flat slope
  // retains smoothDirect()'s unpenalized linear-trend null direction without
  // multiplying that direction by sigma_time_struct.
  array[has_time && time_model == 2 && constr == 1 ? 1 : 0] real time_slope;

  array[st_type > 0 ? 1 : 0] real<lower=0> sigma_st;
  vector[st_type > 0 ? R * T : 0] u_st;
  // Type IV RW2 slope contrasts left free by the smoothDirect constraints.
  vector[st_type == 4 && time_model == 2 && constr == 1 ? R : 0] st_slope;
  // Unit-level Gaussian models estimate tau_obs; FH models supply known sdv.
  array[free_prec ? 1 : 0] real<lower=0> tau_obs;
}

transformed parameters {
  vector[R] re_space;
  vector[T] re_time_unstruct = rep_vector(0, T);
  vector[T] re_time_struct = rep_vector(0, T);
  vector[R * T] re_st = rep_vector(0, R * T);

  if (space_model == 1) {
    // BYM2: b = sigma [sqrt(1-phi) v + sqrt(phi) u*].
    re_space = sigma_space * (sqrt(1 - phi[1]) * v_space +
      sqrt(phi[1]) * (u_space ./ sqrt(node_scale)));
  } else if (space_model == 2) {
    // SAR: b = sigma (I - rho W)^(-1) v, with -1 < rho < 1.
    real rho = -1 + 2 * inv_logit(rho_raw[1]);
    matrix[R, R] A = diag_matrix(rep_vector(1.0, R)) - rho * W_sar;
    re_space = sigma_space * mdivide_left(A, v_space);
  } else {
    re_space = sigma_space * v_space;
  }

  if (has_time == 1) {
    // Time main effect = IID noise + scaled RW1/RW2 effect.
    re_time_unstruct = sigma_time_unstruct[1] * z_time_unstruct;
    re_time_struct = sigma_time_struct[1] * u_time / sqrt(time_scale);
    if (time_model == 2 && constr == 1) {
      for (t in 1:T)
        re_time_struct[t] += time_slope[1] * (t - 0.5 * (T + 1));
    }
  }
  if (st_type > 0) {
    // Interactions are stored time-major: k = (t - 1) R + r.
    for (t in 1:T) {
      for (r in 1:R) {
        int k = (t - 1) * R + r;
        real scale = 1;
        if (st_type == 2) scale = time_scale;
        if (st_type == 3) scale = node_scale[r];
        if (st_type == 4) scale = node_scale[r] * time_scale;
        re_st[k] = sigma_st[1] * u_st[k] / sqrt(scale);
        if (st_type == 4 && time_model == 2 && constr == 1)
          re_st[k] += st_slope[r] * (t - 0.5 * (T + 1));
      }
    }
  }
}

model {
  // A zero beta precision denotes the flat intercept prior used by INLA.
  for (p in 1:P) {
    if (beta_prec[p] > 0)
      target += normal_lpdf(beta[p] | 0, inv_sqrt(beta_prec[p]));
  }
  sigma_space ~ exponential(lambda);
  v_space ~ normal(0, 1);

  if (space_model == 1) {
    // ICAR kernel: log pi(u) = -1/2 sum_(r~s) (u_r - u_s)^2.
    target += -0.5 * dot_self(u_space[node1] - u_space[node2]);
    if (N_singletons > 0) u_space[singleton] ~ normal(0, 1);
    if (constr == 1) {
      // One soft sum-to-zero constraint identifies each connected component.
      for (cc in 1:N_components) {
        if (component_size[cc] > 1) {
          real component_sum = 0;
          for (r in 1:R)
            if (component[r] == cc) component_sum += u_space[r];
          target += normal_lpdf(component_sum | 0,
            0.001 * component_size[cc]);
        }
      }
    }
    if (phi_prior_type == 1)
      target += interpolate_lpdf(phi[1] | phi_grid, phi_logp);
    else
      target += pc_bym2_phi_lpdf(phi[1] | phi_gamma, phi_f, phi_lambda);
  } else if (space_model == 2) {
    rho_raw[1] ~ normal(0, inv_sqrt(10.0));
  }

  if (has_time == 1) {
    sigma_time_unstruct[1] ~ exponential(lambda);
    sigma_time_struct[1] ~ exponential(lambda);
    z_time_unstruct ~ normal(0, 1);
    if (time_model == 1) {
      // RW1 penalizes first differences; RW2 penalizes second differences.
      for (t in 2:T) target += normal_lpdf(u_time[t] - u_time[t - 1] | 0, 1);
    } else {
      for (t in 3:T)
        target += normal_lpdf(u_time[t] - 2 * u_time[t - 1] + u_time[t - 2] | 0, 1);
    }
    if (constr == 1) {
      target += normal_lpdf(sum(u_time) | 0, 0.001 * T);
      if (time_model == 2) {
        vector[T] tt;
        for (t in 1:T) tt[t] = t - 0.5 * (T + 1);
        target += normal_lpdf(dot_product(tt, u_time) | 0,
          0.001 * dot_self(tt));
      }
    }
  }

  if (st_type > 0) {
    sigma_st[1] ~ exponential(lambda);
    if (st_type == 1) {
      // Type I: IID space x IID time.
      u_st ~ normal(0, 1);
    } else if (st_type == 3) {
      // Type III: Besag space x IID time (one ICAR field per time).
      for (t in 1:T) {
        int offset = (t - 1) * R;
        for (e in 1:N_edges)
          target += normal_lpdf(u_st[offset + node1[e]] -
            u_st[offset + node2[e]] | 0, 1);
        for (s in 1:N_singletons)
          target += normal_lpdf(u_st[offset + singleton[s]] | 0, 1);
        if (constr == 1) {
          for (cc in 1:N_components) {
            if (component_size[cc] > 1) {
              real component_sum = 0;
              for (r in 1:R)
                if (component[r] == cc) component_sum += u_st[offset + r];
              target += normal_lpdf(component_sum | 0,
                0.001 * component_size[cc]);
            }
          }
        }
      }
    } else if (st_type == 2) {
      // Type II: IID space x RW time (one RW trajectory per region).
      for (r in 1:R) {
        if (time_model == 1) {
          for (t in 2:T)
            target += normal_lpdf(u_st[(t - 1) * R + r] -
              u_st[(t - 2) * R + r] | 0, 1);
        } else {
          for (t in 3:T)
            target += normal_lpdf(u_st[(t - 1) * R + r] -
              2 * u_st[(t - 2) * R + r] + u_st[(t - 3) * R + r] | 0, 1);
        }
        if (constr == 1) {
          real temporal_sum = 0;
          real temporal_slope = 0;
          for (t in 1:T) {
            temporal_sum += u_st[(t - 1) * R + r];
            temporal_slope += (t - 0.5 * (T + 1)) * u_st[(t - 1) * R + r];
          }
          target += normal_lpdf(temporal_sum | 0, 0.001 * T);
          if (time_model == 2)
            target += normal_lpdf(temporal_slope | 0, 0.001 * T * T);
        }
      }
    } else {
      // Type IV: Besag space x RW time. Penalize spatial contrasts of the
      // temporal first/second differences, i.e. (Delta^q_t u_r - Delta^q_t u_s).
      if (time_model == 1) {
        for (t in 2:T) {
          for (e in 1:N_edges) {
            real d1 = u_st[(t - 1) * R + node1[e]] - u_st[(t - 2) * R + node1[e]];
            real d2 = u_st[(t - 1) * R + node2[e]] - u_st[(t - 2) * R + node2[e]];
            target += normal_lpdf(d1 - d2 | 0, 1);
          }
          for (s in 1:N_singletons)
            target += normal_lpdf(u_st[(t - 1) * R + singleton[s]] -
              u_st[(t - 2) * R + singleton[s]] | 0, 1);
        }
      } else {
        for (t in 3:T) {
          for (e in 1:N_edges) {
            real d1 = u_st[(t - 1) * R + node1[e]] - 2 * u_st[(t - 2) * R + node1[e]] + u_st[(t - 3) * R + node1[e]];
            real d2 = u_st[(t - 1) * R + node2[e]] - 2 * u_st[(t - 2) * R + node2[e]] + u_st[(t - 3) * R + node2[e]];
            target += normal_lpdf(d1 - d2 | 0, 1);
          }
          for (s in 1:N_singletons)
            target += normal_lpdf(u_st[(t - 1) * R + singleton[s]] -
              2 * u_st[(t - 2) * R + singleton[s]] +
              u_st[(t - 3) * R + singleton[s]] | 0, 1);
        }
      }
      if (constr == 1) {
        for (t in 1:T) {
          int offset = (t - 1) * R;
          for (cc in 1:N_components) {
            if (component_size[cc] > 1) {
              real component_sum = 0;
              for (r in 1:R)
                if (component[r] == cc) component_sum += u_st[offset + r];
              target += normal_lpdf(component_sum | 0,
                0.001 * component_size[cc]);
            }
          }
        }
        // Skip one temporal constraint per non-singleton spatial component;
        // its redundant constraint is supplied by the componentwise spatial
        // sums above. Singleton regions are not skipped.
        for (r in 1:R) {
          real temporal_sum = 0;
          real temporal_slope = 0;
          for (t in 1:T) {
            temporal_sum += u_st[(t - 1) * R + r];
            temporal_slope += (t - 0.5 * (T + 1)) * u_st[(t - 1) * R + r];
          }
          if (component_reference[r] == 0) {
            target += normal_lpdf(temporal_sum | 0, 0.001 * T);
            if (time_model == 2)
              target += normal_lpdf(temporal_slope | 0, 0.001 * T * T);
          }
        }
        // The explicit slopes have one componentwise sum constraint, leaving
        // exactly R - C slope contrasts free when there are C non-singleton
        // components, as in smoothDirect().
        if (time_model == 2) {
          for (cc in 1:N_components) {
            if (component_size[cc] > 1) {
              real slope_sum = 0;
              for (r in 1:R)
                if (component[r] == cc) slope_sum += st_slope[r];
              target += normal_lpdf(slope_sum | 0,
                0.001 * component_size[cc]);
            }
          }
        }
      }
    }
  }

  if (free_prec == 1) tau_obs[1] ~ gamma(1, 5e-5);
  {
    // FH: y_i ~ Normal(g^-1(eta_i), sdv_i); unit Gaussian models replace
    // sdv_i with the estimated common standard deviation tau_obs^(-1/2).
    vector[Nrow] eta;
    for (i in 1:Nrow) {
      eta[i] = dot_product(Xall[i], beta) + re_space[region[i]];
      if (has_time == 1) {
        eta[i] += re_time_unstruct[time[i]] + re_time_struct[time[i]];
        if (st_type > 0) eta[i] += re_st[(time[i] - 1) * R + region[i]];
      }
    }
    if (free_prec == 1)
      y ~ normal(link_logit ? inv_logit(eta[obs_idx]) : eta[obs_idx],
                 inv_sqrt(tau_obs[1]));
    else
      y ~ normal(link_logit ? inv_logit(eta[obs_idx]) : eta[obs_idx], sdv);
  }
}

generated quantities {
  // Return eta for observed and unobserved rows; R performs transformations.
  vector[Nrow] eta;
  for (i in 1:Nrow) {
    eta[i] = dot_product(Xall[i], beta) + re_space[region[i]];
    if (has_time == 1) {
      eta[i] += re_time_unstruct[time[i]] + re_time_struct[time[i]];
      if (st_type > 0) eta[i] += re_st[(time[i] - 1) * R + region[i]];
    }
  }
}
