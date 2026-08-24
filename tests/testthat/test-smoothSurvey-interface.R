.direct_fixture <- function(variance = c(0.1, 0.2, 0.15)) {
  regions <- c("a", "b", "c")
  list(
    data = data.frame(region = regions, estimate = c(-0.2, 0.1, 0.3),
                      variance = variance),
    W = matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3,
               dimnames = list(regions, regions))
  )
}

test_that("Stan interfaces use the documented sampling defaults", {
  expect_identical(formals(smoothSurveyStan)$stan.chains, 3)
  expect_identical(formals(smoothSurveyStan)$stan.iter, 4000)
  expect_identical(formals(smoothSurveyStan)$stan.warmup, 2000)
  expect_identical(formals(clusterModel)$stan.chains, 3)
  expect_identical(formals(clusterModel)$stan.iter, 4000)
  expect_identical(formals(clusterModel)$stan.warmup, 2000)

  args <- surveyPrev:::.stan_sampling_args()
  expect_identical(args$chains, 3)
  expect_identical(args$iter, 4000)
  expect_identical(args$warmup, 2000)
})

test_that("Stan retained draw count is determined by sampler controls", {
  expect_identical(
    surveyPrev:::.stan_retained_draw_count(3, 2000, 1000),
    3000L
  )
  expect_identical(
    surveyPrev:::.stan_retained_draw_count(2, 1500, 500),
    2000L
  )
  expect_error(
    surveyPrev:::.stan_retained_draw_count(3, 1000, 1000),
    "stan.warmup < stan.iter"
  )
})

test_that("smoothSurveyStan reports and overwrites a supplied nsim", {
  fixture <- .direct_fixture()
  expect_message(
    smoothSurveyStan(
      data = NULL, direct.est = fixture$data, Amat = fixture$W,
      regionVar = "region", responseVar = "estimate",
      direct.est.var = "variance", response.type = "gaussian",
      smooth = FALSE, nsim = 17,
      stan.chains = 2, stan.iter = 20, stan.warmup = 5
    ),
    "ignoring user-supplied nsim = 17; using all 30 retained Stan draws"
  )
})

test_that("zero and negative sampling variances stop instead of filtering", {
  engines <- "rstan"
  if (requireNamespace("INLA", quietly = TRUE)) engines <- c(engines, "INLA")
  for (engine in engines) {
    for (bad in c(0, -0.1)) {
      fixture <- .direct_fixture(c(0.1, bad, 0.2))
      fit_fun <- if (engine == "rstan") smoothSurveyStan else SUMMER::smoothSurvey
      expect_error(
        fit_fun(
          data = NULL, direct.est = fixture$data, Amat = fixture$W,
          regionVar = "region", responseVar = "estimate",
          direct.est.var = "variance", response.type = "gaussian",
          smooth = FALSE
        ),
        "strictly positive.*b"
      )
    }
  }
})

test_that("custom formula is explicitly INLA-only", {
  expect_error(
    smoothSurveyStan(formula = "f(region.struct, model='iid')"),
    "only in SUMMER::smoothSurvey"
  )
})

test_that("smoothSurveyStan delegates preparation to SUMMER", {
  fixture <- .direct_fixture()
  fit <- smoothSurveyStan(
    data = NULL, direct.est = fixture$data, Amat = fixture$W,
    regionVar = "region", responseVar = "estimate",
    direct.est.var = "variance", response.type = "gaussian",
    smooth = FALSE
  )

  expect_s3_class(fit, "SUMMERmodel.svy")
  expect_identical(fit$engine, "rstan")
  expect_null(fit$fit)
  expect_equal(fit$direct$direct.est, fixture$data$estimate)
})

test_that("type.st zero does not generate an interaction", {
  skip_if_not_installed("INLA")
  fixture <- .direct_fixture()
  fit <- SUMMER::smoothSurvey(
    data = NULL, direct.est = transform(fixture$data, time = 1L),
    Amat = fixture$W, regionVar = "region", timeVar = "time",
    responseVar = "estimate", direct.est.var = "variance",
    response.type = "gaussian", type.st = 0,
    smooth = FALSE
  )
  expect_false(grepl("region.int", paste(deparse(fit$formula), collapse = " ")))
})

test_that("all interaction formula types are generated", {
  skip_if_not_installed("INLA")
  regions <- c("a", "b", "c")
  W <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0), 3, 3,
              dimnames = list(regions, regions))
  direct <- expand.grid(region = regions, time = 1:3)
  direct$estimate <- seq_len(nrow(direct)) / 10
  direct$variance <- 0.1
  for (type in 1:4) {
    fit <- SUMMER::smoothSurvey(
      data = NULL, direct.est = direct, Amat = W,
      regionVar = "region", timeVar = "time", responseVar = "estimate",
      direct.est.var = "variance", response.type = "gaussian",
      type.st = type, smooth = FALSE
    )
    formula.text <- paste(deparse(fit$formula), collapse = " ")
    if (type == 4) {
      expect_true(grepl("time.area", formula.text))
      expect_true(grepl("generic0", formula.text))
    } else {
      expect_true(grepl("region.int", formula.text))
    }
  }
})
