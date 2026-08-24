# Shared Stan backend utilities -------------------------------------------------

# Compiled models are cached once per R session.
.surveyPrev_stan_cache <- new.env(parent = emptyenv())

.require_rstan <- function() {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    stop(
      "The optional package 'rstan' is required for the Stan backend. ",
      "Install it with install.packages('rstan').",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.get_stanmodel <- function(name) {
  # Resolve an installed Stan source (or the development copy), compile it
  # once, and reuse the stanmodel for subsequent fits in this R session.
  .require_rstan()
  cached <- .surveyPrev_stan_cache[[name]]
  if (!is.null(cached)) return(cached)

  filename <- paste0(name, ".stan")
  path <- system.file("stan", filename, package = "surveyPrev")
  if (!nzchar(path) || !file.exists(path)) {
    candidate <- file.path("inst", "stan", filename)
    if (file.exists(candidate)) path <- candidate
  }
  if (!nzchar(path) || !file.exists(path)) {
    stop("Could not locate inst/stan/", filename, ".", call. = FALSE)
  }

  model <- rstan::stan_model(path)
  .surveyPrev_stan_cache[[name]] <- model
  model
}

# Small accessors keep callers independent of Stan source filenames.
get_betabinom_stanmodel <- function() .get_stanmodel("betabinom")
get_fh_stanmodel <- function() .get_stanmodel("fh_gaussian")

.validate_adjacency_stan <- function(W) {
  # Stan uses a binary, undirected edge list. Numerical row-standardized
  # weights are therefore reduced to their nonzero neighbour structure.
  W <- as.matrix(W)
  if (length(dim(W)) != 2L || nrow(W) != ncol(W) || nrow(W) < 1L) {
    stop("Amat must be a non-empty square adjacency matrix.", call. = FALSE)
  }
  if (anyNA(W) || any(!is.finite(W))) {
    stop("Amat cannot contain missing or non-finite values.", call. = FALSE)
  }
  diag(W) <- 0
  # spdep::nb2mat() commonly returns row-standardized weights (style = "W").
  # Those numerical weights are asymmetric whenever neighbouring regions have
  # different degrees, even though their undirected nonzero edge structure is
  # symmetric. BYM2/Besag uses that edge structure, not the standardized
  # weights, so validate symmetry after converting nonzero entries to edges.
  W <- (W != 0) * 1
  if (!isTRUE(all.equal(W, t(W), tolerance = 0))) {
    stop(
      "Amat must have symmetric neighbour links for a BYM2/Besag model: ",
      "if Amat[i, j] is nonzero, Amat[j, i] must also be nonzero.",
      call. = FALSE
    )
  }
  storage.mode(W) <- "double"
  W
}

.graph_components_stan <- function(W) {
  # Breadth-first traversal labels connected components for componentwise
  # ICAR scaling and sum-to-zero constraints.
  n <- nrow(W)
  component <- integer(n)
  current <- 0L
  for (start in seq_len(n)) {
    if (component[start] != 0L) next
    current <- current + 1L
    queue <- start
    component[start] <- current
    while (length(queue)) {
      node <- queue[1L]
      queue <- queue[-1L]
      neighbours <- which(W[node, ] != 0 & component == 0L)
      if (length(neighbours)) {
        component[neighbours] <- current
        queue <- c(queue, neighbours)
      }
    }
  }
  sizes <- tabulate(component, nbins = current)
  list(id = component, size = sizes, n = current)
}

.rw_scaling_factor_stan <- function(n, order = 1L) {
  # Scale RW1/RW2 effects by the geometric mean marginal variance of Q^+,
  # giving sigma the same generalized-variance interpretation across T.
  if (order == 1L && n < 2L) return(1)
  if (order == 2L && n < 3L) return(1)
  D <- diff(diag(n), differences = order)
  Q <- crossprod(D)
  ee <- eigen(Q, symmetric = TRUE)
  keep <- ee$values > max(ee$values) * 1e-10
  Qplus <- ee$vectors[, keep, drop = FALSE] %*%
    (t(ee$vectors[, keep, drop = FALSE]) / ee$values[keep])
  exp(mean(log(pmax(diag(Qplus), .Machine$double.xmin))))
}

# Prepare a disconnected graph exactly as required by a BYM2 decomposition.
# Every non-singleton connected component gets its own ICAR scaling and
# constraint. A singleton is represented by a proper N(0, 1) structured term,
# matching INLA's adjust.for.con.comp behaviour.
.prepare_bym2_graph_stan <- function(W, phi.prior = c("inla.table", "spectral"),
                                     u = 0.5, alpha = 2 / 3,
                                     table.length = 10000L) {
  phi.prior <- match.arg(phi.prior)
  W <- .validate_adjacency_stan(W)
  R <- nrow(W)
  graph <- .graph_components_stan(W)
  # Q = D - W is singular within each non-singleton component.
  Q <- diag(rowSums(W)) - W
  Q.scaled <- matrix(0, R, R)
  marginal.var <- numeric(R)
  node.scale <- rep(1, R)

  for (cc in seq_len(graph$n)) {
    idx <- which(graph$id == cc)
    if (length(idx) == 1L) {
      Q.scaled[idx, idx] <- 1
      marginal.var[idx] <- 1
      next
    }
    # Use Q^+ to scale each component to generalized marginal variance one.
    Qc <- Q[idx, idx, drop = FALSE]
    ee <- eigen(Qc, symmetric = TRUE)
    keep <- ee$values > max(ee$values) * 1e-10
    Qplus <- ee$vectors[, keep, drop = FALSE] %*%
      (t(ee$vectors[, keep, drop = FALSE]) / ee$values[keep])
    scale <- exp(mean(log(pmax(diag(Qplus), .Machine$double.xmin))))
    node.scale[idx] <- scale
    marginal.var[idx] <- diag(Qplus) / scale
    Q.scaled[idx, idx] <- scale * Qc
  }

  # Store every undirected edge once; Stan evaluates squared edge differences.
  edges <- which(upper.tri(W) & W != 0, arr.ind = TRUE)
  node1 <- if (nrow(edges)) as.integer(edges[, 1L]) else integer()
  node2 <- if (nrow(edges)) as.integer(edges[, 2L]) else integer()
  singleton <- which(graph$size[graph$id] == 1L)
  component.reference <- integer(R)
  for (cc in seq_len(graph$n)) {
    idx <- which(graph$id == cc)
    if (length(idx) > 1L) component.reference[idx[1L]] <- 1L
  }
  rankdef <- sum(graph$size > 1L)

  # The eigenvalue summaries parameterize the analytic PC prior for phi.
  eigenvalues <- sort(eigen(Q.scaled, symmetric = TRUE,
                            only.values = TRUE)$values, decreasing = TRUE)
  eigenvalues[abs(eigenvalues) < max(abs(eigenvalues), 1) * 1e-10] <- 0
  positive <- eigenvalues[seq_len(R - rankdef)]
  gamma <- c(1 / positive, rep(0, rankdef)) - 1
  f <- mean(marginal.var) - 1

  # Spectral constants are supplied even for the table prior so the Stan data
  # have one stable shape across prior choices.
  spectral_distance <- function(phi) {
    sqrt(max(R * phi * f - sum(log1p(phi * gamma)), 0))
  }
  distance.u <- spectral_distance(u)
  lambda.phi <- if (is.finite(distance.u) && distance.u > 0) {
    -log1p(-alpha) / distance.u
  } else {
    1
  }

  if (phi.prior == "inla.table") {
    if (rankdef == 0L) {
      stop(
        "The BYM2 phi prior is undefined when the graph contains only ",
        "singletons. Use an IID spatial model instead.", call. = FALSE
      )
    }
    if (!requireNamespace("INLA", quietly = TRUE)) {
      stop(
        "bym2.phi.prior = 'inla.table' requires INLA during Stan data preparation. ",
        "Install INLA or use bym2.phi.prior = 'spectral'.",
        call. = FALSE
      )
    }
    # Tabulate INLA's log-density once in R; Stan linearly interpolates it.
    prior.fun <- getFromNamespace("inla.pc.bym.phi", "INLA")(
      eigenvalues = eigenvalues,
      marginal.variances = marginal.var,
      rankdef = rankdef,
      u = u,
      alpha = alpha
    )
    theta <- seq(-10, 10, length.out = as.integer(table.length))
    phi.grid <- stats::plogis(theta)
    phi.logp <- prior.fun(phi.grid)
    phi.prior.type <- 1L
  } else {
    phi.grid <- c(0, 1)
    phi.logp <- c(0, 0)
    phi.prior.type <- 2L
  }

  list(
    W = W,
    N_edges = length(node1),
    node1 = as.array(node1),
    node2 = as.array(node2),
    N_components = graph$n,
    component = as.array(as.integer(graph$id)),
    component_size = as.array(as.integer(graph$size)),
    component_reference = as.array(component.reference),
    N_singletons = length(singleton),
    singleton = as.array(as.integer(singleton)),
    node_scale = as.array(node.scale),
    phi_prior_type = phi.prior.type,
    Kphi = length(phi.grid),
    phi_grid = as.array(phi.grid),
    phi_logp = as.array(phi.logp),
    phi_gamma = as.array(gamma),
    phi_f = f,
    phi_lambda = lambda.phi,
    rankdef = rankdef,
    marginal_variances = marginal.var,
    Q_scaled = Q.scaled
  )
}

.empty_spatial_data_stan <- function(R) {
  # Supply dimensionally valid placeholders when no Besag field is fitted.
  # This keeps one stable Stan data schema for IID and SAR models.
  list(
    N_edges = 0L,
    node1 = as.array(integer()),
    node2 = as.array(integer()),
    N_components = R,
    component = as.array(seq_len(R)),
    component_size = as.array(rep.int(1L, R)),
    component_reference = as.array(rep.int(0L, R)),
    N_singletons = R,
    singleton = as.array(seq_len(R)),
    node_scale = as.array(rep(1, R)),
    phi_prior_type = 2L,
    Kphi = 2L,
    phi_grid = as.array(c(0, 1)),
    phi_logp = as.array(c(0, 0)),
    phi_gamma = as.array(rep(0, R)),
    phi_f = 0,
    phi_lambda = 1
  )
}

.stan_sampling_args <- function(chains = 3, iter = 4000, warmup = 2000,
                                seed = 1, cores = getOption("mc.cores", 1L),
                                control = NULL, ...) {
  # Centralize defaults so every public Stan interface samples consistently.
  if (is.null(control)) {
    control <- list(max_treedepth = 12, adapt_delta = 0.95)
  }
  c(list(chains = chains, iter = iter, warmup = warmup, seed = seed,
         cores = cores, refresh = 0, control = control), list(...))
}

.stan_retained_draw_count <- function(chains, iter, warmup) {
  # SUMMER's nsim must equal chains * (iter - warmup) for the Stan backend.
  values <- c(chains = chains, iter = iter, warmup = warmup)
  if (length(chains) != 1L || length(iter) != 1L || length(warmup) != 1L ||
      anyNA(values) || any(!is.finite(values)) ||
      any(values != floor(values))) {
    stop(
      "stan.chains, stan.iter, and stan.warmup must be finite whole numbers.",
      call. = FALSE
    )
  }
  if (chains < 1L || iter < 1L || warmup < 0L || warmup >= iter) {
    stop(
      "Stan sampling requires stan.chains >= 1, stan.iter >= 1, and ",
      "0 <= stan.warmup < stan.iter.",
      call. = FALSE
    )
  }
  n <- chains * (iter - warmup)
  if (n > .Machine$integer.max) {
    stop("The requested number of retained Stan draws is too large.",
         call. = FALSE)
  }
  as.integer(n)
}
