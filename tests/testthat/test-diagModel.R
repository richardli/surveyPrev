test_that("diagModel rejects objects without a fitted model", {
  expect_error(diagModel(list()), "could not find")
})

test_that("INLA diagnostics handle unavailable optional checks", {
  fake <- list(
    mode = list(status = 0L),
    summary.fixed = data.frame(mean = 0, sd = 1),
    summary.random = list(area = data.frame(mean = c(0, 1), sd = c(1, 1))),
    summary.hyperpar = data.frame(mean = 1, sd = 0.2)
  )
  class(fake) <- "inla"
  result <- diagModel(fake)
  expect_s3_class(result, "diagModel")
  expect_equal(result$engine, "INLA")
  expect_true(all(c("check", "status", "message") %in% names(result$summary)))
})

test_that("diagModel finds the INLA fit inside an fhModel result", {
  fake <- list(
    mode = list(status = 0L),
    summary.fixed = data.frame(mean = 0, sd = 1),
    summary.random = list(),
    summary.hyperpar = data.frame(mean = 1, sd = 0.2)
  )
  class(fake) <- "inla"
  fh.result <- list(model = list(fit = fake))

  result <- diagModel(fh.result)
  expect_s3_class(result, "diagModel")
  expect_equal(result$engine, "INLA")
  expect_identical(result$fit, fake)
})
