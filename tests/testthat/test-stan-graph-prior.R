test_that("disconnected BYM2 graphs are scaled by component", {
  W <- matrix(0, 7, 7)
  W[cbind(1:4, 2:5)] <- 1
  W <- W + t(W)
  W[6, 7] <- W[7, 6] <- 1

  graph <- surveyPrev:::.prepare_bym2_graph_stan(W, "spectral")
  expect_equal(graph$N_components, 2L)
  expect_equal(graph$rankdef, 2L)
  expect_equal(as.vector(graph$component_size), c(5L, 2L))
  expect_equal(as.vector(graph$component_reference),
               c(1L, 0L, 0L, 0L, 0L, 1L, 0L))
  expect_equal(graph$N_singletons, 0L)

  for (cc in unique(graph$component)) {
    idx <- which(graph$component == cc)
    expect_equal(exp(mean(log(graph$marginal_variances[idx]))), 1,
                 tolerance = 1e-8)
  }
})

test_that("row-standardized undirected adjacency is accepted", {
  binary <- matrix(0, 5, 5)
  binary[1, 2:5] <- 1
  binary[2:5, 1] <- 1
  row.standardized <- binary / rowSums(binary)

  expect_false(isTRUE(all.equal(row.standardized, t(row.standardized))))
  validated <- surveyPrev:::.validate_adjacency_stan(row.standardized)
  expect_equal(validated, binary)
  expect_silent(
    surveyPrev:::.prepare_bym2_graph_stan(row.standardized, "spectral")
  )

  # This is the graph-data path used by fhModel() through smoothSurveyStan().
  spatial <- surveyPrev:::.smoothsurvey_spatial_data_stan(
    Amat = row.standardized, is.iid.space = FALSE, space.model = "bym2",
    type.st = 0L, bym2.phi.prior = "spectral",
    pc.u.phi = 0.5, pc.alpha.phi = 2 / 3
  )
  expect_equal(spatial$graph$N_edges, 4L)
  expect_equal(spatial$code, 1L)
})

test_that("genuinely directed neighbour links are rejected", {
  directed <- matrix(0, 3, 3)
  directed[1, 2] <- 1
  expect_error(
    surveyPrev:::.validate_adjacency_stan(directed),
    "symmetric neighbour links"
  )
})

test_that("Besag graph data follow the Type III and IV labels", {
  W <- matrix(0, 4, 4)
  W[cbind(1:3, 2:4)] <- 1
  W <- W + t(W)

  type2 <- surveyPrev:::.smoothsurvey_spatial_data_stan(
    Amat = W, is.iid.space = FALSE, space.model = "sar", type.st = 2L,
    bym2.phi.prior = "spectral", pc.u.phi = 0.5, pc.alpha.phi = 2 / 3
  )
  type3 <- surveyPrev:::.smoothsurvey_spatial_data_stan(
    Amat = W, is.iid.space = FALSE, space.model = "sar", type.st = 3L,
    bym2.phi.prior = "spectral", pc.u.phi = 0.5, pc.alpha.phi = 2 / 3
  )

  expect_equal(type2$graph$N_edges, 0L)
  expect_equal(type3$graph$N_edges, 3L)
})

test_that("singleton graph components receive proper unit scaling", {
  W <- matrix(0, 4, 4)
  W[1, 2] <- W[2, 1] <- 1
  graph <- surveyPrev:::.prepare_bym2_graph_stan(W, "spectral")

  expect_equal(graph$rankdef, 1L)
  expect_equal(as.vector(graph$singleton), c(3L, 4L))
  expect_equal(as.vector(graph$component_reference), c(1L, 0L, 0L, 0L))
  expect_equal(as.vector(graph$node_scale[3:4]), c(1, 1))
  expect_equal(diag(graph$Q_scaled)[3:4], c(1, 1))
})

test_that("INLA table prior is finite and uses the requested resolution", {
  skip_if_not_installed("INLA")
  W <- matrix(0, 5, 5)
  W[cbind(1:4, 2:5)] <- 1
  W <- W + t(W)
  graph <- surveyPrev:::.prepare_bym2_graph_stan(
    W, "inla.table", table.length = 501
  )
  expect_equal(graph$phi_prior_type, 1L)
  expect_length(graph$phi_grid, 501)
  expect_true(all(diff(graph$phi_grid) > 0))
  expect_true(all(is.finite(graph$phi_logp)))

  W.disconnected <- matrix(0, 6, 6)
  W.disconnected[1, 2] <- W.disconnected[2, 1] <- 1
  W.disconnected[2, 3] <- W.disconnected[3, 2] <- 1
  W.disconnected[4, 5] <- W.disconnected[5, 4] <- 1
  disconnected <- surveyPrev:::.prepare_bym2_graph_stan(
    W.disconnected, "inla.table", table.length = 101
  )
  expect_equal(disconnected$rankdef, 2L)
  expect_equal(as.vector(disconnected$singleton), 6L)
  expect_true(all(is.finite(disconnected$phi_logp)))
})

test_that("both Stan programs pass semantic translation", {
  skip_if_not_installed("rstan")
  for (filename in c("fh_gaussian.stan", "betabinom.stan")) {
    path <- system.file("stan", filename, package = "surveyPrev")
    if (!nzchar(path)) {
      path <- testthat::test_path("..", "..", "inst", "stan", filename)
    }
    translated <- rstan::stanc(
      file = path,
      allow_undefined = FALSE
    )
    expect_true(translated$status)
  }
})
