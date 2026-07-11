# INLA → rstan port of surveyPrev: changes, design, and validation

Branch: `rstan-experiment` (off `main`, merged with upstream `main` as of
2026-07-07). Goal: replace all INLA calls with rstan while keeping the user
interface, the internal data structures (saved posterior draws, result
tables), and the model specifications identical. Both engines remain
available and switchable at runtime.

---

## 1. Where INLA actually lived (scope discovery)

| Location | INLA usage | Action |
|---|---|---|
| `R/clusterModel.R` | Direct: one `INLA::inla(family="betabinomial")` fit + `INLA::inla.posterior.sample(n=1000)` | Ported |
| `R/smthdirectEST.R` (`fhModel`) | Indirect via `SUMMER::smoothSurvey(smooth=TRUE)`, 4 call sites (gaussian `direct.est` mode and binary survey mode, admin1/admin2) | Ported |
| `R/directEST.R`, `R/directEST_varfix.R` | `smoothSurvey(..., smooth=FALSE)` — design-based HT estimates only, INLA never runs | **No port needed** |
| `R/U5MR_helpers.R` | `clusterModel_u5mr` loops over `clusterModel`; `directEST_u5mr` uses `SUMMER::getDirect` (design-based) | Covered for free |
| `R/DV_*.R` (11 files) | False positives: the string "in-law" in domestic-violence indicator definitions | Nothing to do |

Both `smoothSurvey` modes used by `fhModel` reduce to the **same model**:
a gaussian Fay–Herriot on logit-scale direct estimates with known, fixed
variances. So the whole package needs exactly **two Stan models**.

## 2. Architecture and design choices

### 2.1 Drop-in shim, not a rewrite

The single most important decision: rather than rewrite the ~1,400 lines of
post-processing in `clusterModel.R`, the rstan backend mimics the INLA API
surface that the downstream code consumes:

* `stan_betabinomial()` returns `(imod, samp)` where
  * `samp[[j]]$latent` is a named one-column matrix whose row names follow
    INLA's latent naming convention exactly — `sID:k`, `(Intercept):1`,
    `strataurban:1`, `stratarural:1`, `admin1.nameX:1`,
    `admin1.nameX:stratarural:1`, `<covariate>:1` — produced by building the
    fixed-effects design with `stats::model.matrix()` (INLA names its fixed
    effects after model-matrix columns) and appending `":1"`;
  * `imod$summary.fitted.values` has INLA's column layout
    (`mean, sd, 0.025quant, 0.5quant, 0.975quant`) computed from
    generated-quantity draws for the appended per-area prediction rows;
  * `imod$stanfit` carries the raw `stanfit`.
* `surveyPrev::smoothSurvey()` is a **full clone of
  `SUMMER::smoothSurvey()`** with one added argument,
  `engine = c("rstan", "INLA")`. All data preparation, HT (direct)
  estimation, covariate handling and output assembly are shared verbatim
  between the engines (identical `SUMMERmodel.svy` output: `$smooth`,
  `$smooth.overall`, `$draws.est` with `region`/`time`/`sample:j` columns
  of linear-predictor draws, `$HT`, `$direct`, `$draws`, `$fit`). Only
  three points are engine-specific:
  1. the model fit (`.stan_smoothSurvey_fit()` maps the smoothSurvey
     branches onto the two package Stan models);
  2. linear-predictor draws (`.linpred_draws()`: `inla.rmarginal` vs the
     `eta` draw matrix);
  3. posterior samples (`.posterior_sample_engine()`, which emits
     `inla.posterior.sample()`-shaped `$latent` — Predictor rows first,
     then `region.struct:k` and `<fixed>:1` — so even SUMMER's
     `weight.strata` aggregation block runs unchanged for both engines).

  rstan coverage: area-level FH (binary/gaussian, matched and unmatched
  link, `direct.est` input), unit-level binary beta-binomial (incl.
  within-area strata and `weight.strata` aggregation), unit-level
  gaussian nested-error models. Temporal models (`timeVar`), SAR, and
  custom `formula` additions raise an informative error under rstan.
  `smooth = FALSE` no longer requires INLA to be installed at all. The
  function is designed to be portable upstream to SUMMER.

Result: **zero changes** to the aggregation/stratification/pixel code, and
the saved posterior-draw matrices (`admin1_post`, `urban_post`, …) are
structurally identical between engines.

### 2.2 Engine switch

`options(surveyPrev.engine = "rstan" | "INLA")`, default `"rstan"`.
One `if` at the fit call site in `clusterModel()`; everywhere else the
engine flows through `smoothSurvey(engine = ...)`, whose default is the
same option. This is what makes every validation below an A/B test on
identical inputs.

### 2.3 The `inla=` output slot

Kept under its original name but now holds the shim's `imod` (with the
stanfit inside). Chosen over (a) a separate `stan=` slot (changes list
structure) and (b) a full INLA-object mimic (needless adapter code —
downstream only reads `summary.fitted.values`).

### 2.4 Exact model parity with INLA

Verified against INLA's own documentation (`documentation/likelihood/
betabinomial.pdf`) and the SUMMER/INLA sources:

| Component | Specification (identical in both engines) |
|---|---|
| Beta-binomial likelihood | ICC parameterisation: `a = p(1−ρ)/ρ`, `b = (1−p)(1−ρ)/ρ`; logit link |
| Overdispersion prior | on `logit(ρ) ~ N(overdisp.mean, prec = overdisp.prec)` |
| FH likelihood | `y_i ~ N(η_i, v_i)` with `v_i` known (INLA: gaussian family, precision fixed at 1, `scale = 1/v_i`) |
| PC precision prior | `σ ~ Exp(λ)`, `λ = −log(pc.alpha)/pc.u` |
| BYM2 | `re = σ(√(1−φ)·v + √(φ/scaling)·u)`; `u` = scaled ICAR; scaling factor = geometric mean of marginal variances of the constrained ICAR (computed from the perturbed precision, sum-to-zero-projected inverse) |
| BYM2 φ prior | INLA's KLD-based PC prior `π(φ)`: `d(φ)=√(2·KLD)`, exponential on `d` calibrated so `P(φ<u)=α`; passed to Stan as a 200-point log-density grid with linear interpolation |
| Fixed effects | intercept flat; all others `N(0, prec = 0.001)` (INLA defaults) |
| NA covariates | contribute 0 to the linear predictor (INLA convention), via `mm[is.na(mm)] <- 0` |
| Posterior draws | 1,000 saved draws in both engines (`nsamp`/`nsim = 1000`) |

### 2.5 Stan implementation details

* Non-centered parameterisation of the random effects; ICAR via the
  pairwise-difference trick with a soft sum-to-zero constraint
  (`sum(u) ~ N(0, 0.001·R)`).
* NUTS defaults: 4 chains × 1000 iter (500 warmup),
  `control = list(max_treedepth = 12, adapt_delta = 0.95)` — chosen after
  treedepth/ESS warnings at the defaults; user-overridable through `...`.
* Compiled models cached per session in a package environment
  (`.surveyPrev_stan_cache`); `.stan` sources live in `inst/stan/` and are
  compiled on first use (`rstan::stan_model`). `inst/stan/*.rds`
  (`auto_write` cache) is git-ignored.
* rstan collapses length-1 vectors to scalars — all vector-valued
  `stan_data` entries are wrapped in `as.array()`.

### 2.6 Intentional deviation from SUMMER

`SUMMER::smoothSurvey` (≤ 1.4.0) fills `smooth$logit.upper` with the
**lower** quantile for binary responses (source: `quantile(tmp, (1−CI)/2)`
for both bounds). `surveyPrev::smoothSurvey()` computes the upper quantile
correctly. Every other quirk is replicated faithfully, including the row
re-sort (alphabetical by region) that `merge(dat, X, by="region")`
introduces when covariates are supplied, and the exact survey-package HT
computation (`svydesign(ids=~cluster+householdID, strata=~strata.full)` →
`svyby(svymean)` → logit transform with delta-method variance).

## 3. Files changed

| File | Change |
|---|---|
| `inst/stan/betabinom.stan` | new — beta-binomial cluster model (iid/BYM2) |
| `inst/stan/fh_gaussian.stan` | new — gaussian FH smoothing model (iid/BYM2) |
| `R/stan_shim.R` | new — `stan_betabinomial()`, BYM2 scaling factor, φ PC-prior grid, model cache |
| `R/smoothSurvey.R` | new — full `SUMMER::smoothSurvey()` clone with `engine` argument (supersedes the interim `stan_smoothsurvey.R`) |
| `R/clusterModel.R` | fit call site swapped behind the engine option |
| `R/smthdirectEST.R`, `R/directEST.R`, `R/directEST_varfix.R` | all `smoothSurvey` call sites resolve to the package-local function (SUMMER import dropped) |
| `DESCRIPTION` | `rstan` added to Imports |
| `dev/validation/*.R` | 7 validation scripts (below); `dev/` is build-ignored |

Commits: `6be296f` (backend), `4982aff` (clusterModel wiring), `434f125`
(cluster validation), `468e0f4` (merge of main), `cddb0c7` (FH port),
`a60a637` (FH validation + exact arbitration), `ecc61ba` (rare-outcome test).

## 4. Validation performed

All comparisons run both engines on **identical simulated inputs** (same
data, priors, model options). Tolerances are judged against Monte Carlo
error for 1,000 saved draws. Scripts in `dev/validation/`.

### 4.1 clusterModel lock-down (`validate_cluster.R`, `validate_strat.R`, `validate_full.R`)

Geography: 8 areas (admin1 tests) / 4 admin1 × 3 admin2 (full matrix).
Post-merge rerun numbers (probability scale):

| Scenario | max\|Δmean\| | max\|Δsd\| |
|---|---|---|
| iid, admin1, unstratified | 0.0015 | 0.0022 |
| bym2, admin1, unstratified | 0.0015 | 0.0012 |
| bym2, admin1, stratified | 0.0025 | 0.0022 |
| A: admin2 bym2 + aggregation (res.admin2 / agg.admin1 / agg.natl) | 0.0014 / 0.0022 / 0.0005 | ≤0.0018 |
| B: admin2 bym2 stratified nested | 0.0100 | 0.0020 |
| C: admin2 bym2 stratified nested **interact** | 0.0076 | 0.0025 |
| D: admin1 iid + area covariate `X` | 0.0010 | 0.0005 |
| E: admin2 bym2 + `X.unit`/`X.pixel` (agg.admin2/admin1/natl) | 0.0045 / 0.0019 / 0.0007 | ≤0.0031 |

These jointly exercise every latent-naming convention the downstream
extraction depends on (`sID:k`, `(Intercept):1`, `strataurban:1`,
`admin1.nameX:1`, `admin1.nameX:stratarural:1`, `<cov>:1`) and every
aggregation path (fitted-values path, draws path, pixel-population path).

### 4.2 Smoothed-direct / fhModel (`validate_fh.R`)

| Scenario | max\|Δmean\| (probability scale) |
|---|---|
| F: fhModel admin1 bym2 + national aggregation | 0.0004 (agg.natl 0.5493 vs 0.5492) |
| G: fhModel admin1 iid + area covariate | 0.0005 |
| I: fhModel admin2 bym2 nested (factor covariate `admin1.name`) | 0.0058 |
| H: isolated gaussian `direct.est` mode, small data | logit-scale gaps up to ~0.02 → arbitrated below |

### 4.3 Exact-posterior arbitrations

Two independent gold standards were computed by brute force, so neither
engine is judged by the other:

**(a) FH gaussian, iid, 12 areas, 1 missing (`validate_fh_exact.R`).**
Since the model is gaussian given σ, the posterior is a 1-D numerical
integral over σ. Result: **Stan matches exact to ≤0.005 (its MC error);
INLA is off by up to 0.017**, systematically under-shrinking. Mechanism
confirmed by comparing the σ posteriors directly:

| σ posterior | mean | q25 | median | q75 | P(σ<0.05) |
|---|---|---|---|---|---|
| exact | 0.1223 | 0.0582 | 0.1123 | 0.1665 | 0.209 |
| Stan | 0.1178 | 0.0558 | 0.1084 | 0.1664 | 0.225 |
| INLA | 0.1272 | 0.0676 | 0.1172 | 0.1728 | **0.168** |

INLA underweights the near-zero-variance region of the skewed hyperparameter
posterior (log-precision-space Laplace/grid machinery), hence less
shrinkage. Insensitive to `strategy="laplace"` / `int.strategy="grid"`
(identical to 3 decimals) — it is not a tuning issue. This is a genuine
small-data INLA approximation error; **the rstan engine is the more
accurate one in this regime**.

**(b) Rare-outcome beta-binomial, iid, 8 areas (`validate_rare.R`).**
Given (β₀, σ, ρ) the areas factorize, so the exact posterior needs only
1-D Gauss–Hermite quadrature per area over a 35×30×30 hyperparameter grid.
Prevalences ~2% (4–19 cases/area) and ~0.5% (0–9 cases/area, three
zero-case areas):

| Regime | INLA max err vs exact (rel.) | Stan max err vs exact (rel.) |
|---|---|---|
| prev ~2% | 0.0006 (3.5%) | 0.0001 (0.5%) |
| prev ~0.5% | 0.0003 (5.0%), slight downward bias in means and sds | 0.0001 (1.6%) |

INLA's cluster model **holds up well** for rare outcomes — cluster-level
aggregation (beta-binomial with n≈15–35 trials) keeps each likelihood
contribution far better behaved than the unit-level Bernoulli case where
INLA's rare-binary problems are documented. Stan is 3–5× closer to exact.
BYM2 spot check at 0.5%: engines within 0.0004 (6.2% relative).

FH on the same rare data: engine parity as predicted (max
\|INLA−Stan\| ≤ 0.0009 — same misspecified model either way), but model
failure at 0.5%: zero-case areas have undefined logits and drop out of the
likelihood entirely; MAE vs truth 0.0035 vs the cluster model's 0.0013
(~2.7× worse). Practical guidance: for rare indicators the choice that
matters is `clusterModel` vs `fhModel`, not the inference engine.

### 4.4 Regression safety

The full lock-down matrix was rerun after merging upstream `main`
(10 commits, including the `agg.admin2/agg.admin1/agg.natl` output renames
in the pixel branch) — all scenarios still pass against the merged code.

## 5. Known limitations / future work

* `imod$waic` is `NA` under the rstan engine (INLA's WAIC is not consumed
  anywhere in the package; could be added via `loo` if wanted).
* `smoothSurvey(engine = "rstan")` covers all modes surveyPrev uses plus
  unit-level and unmatched-link models — but not the temporal / SAR /
  custom-formula modes (informative error; use `engine = "INLA"`).
* Stan models compile on first use (~40 s once per session). Packaging with
  `rstantools` (pre-compiled `src/`) is the natural next step if the branch
  graduates from experiment to release.
* Validation is single-dataset per scenario; a replication study
  (seeds × prevalence × number of areas) would make the accuracy claims
  paper-grade. The rare-outcome script is structured to support this.
* Sampler settings (treedepth 12, adapt_delta 0.95) favour robustness over
  speed; no wall-clock benchmark INLA vs rstan has been run yet.
* Upstream: SUMMER's binary-mode `logit.upper` bug is worth reporting.
