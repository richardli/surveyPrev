#' Smoothed direct estimates with a selectable inference engine
#'
#' A full re-implementation of \code{SUMMER::smoothSurvey()} with an
#' additional \code{engine} argument selecting between INLA (SUMMER's
#' original backend, code paths preserved verbatim) and rstan. All data
#' preparation, direct (HT) estimation, covariate handling and output
#' assembly are shared between the engines; only the model fit and the
#' posterior extraction differ. Inputs and outputs match
#' \code{SUMMER::smoothSurvey()} so this function can eventually be ported
#' upstream to SUMMER.
#'
#' Supported with \code{engine = "rstan"}:
#' \itemize{
#'   \item area-level (Fay-Herriot) models, binary and gaussian response,
#'         matched or unmatched link, with or without \code{direct.est};
#'   \item unit-level binary (beta-binomial) models, including within-area
#'         stratification and \code{weight.strata} aggregation;
#'   \item unit-level gaussian nested-error models.
#' }
#' Not (yet) supported with \code{engine = "rstan"} (an informative error is
#' thrown): temporal models (\code{timeVar}), \code{space.model = "sar"},
#' and user-supplied \code{formula} additions.
#'
#' Known intentional deviation from SUMMER (both engines): for binary
#' responses, SUMMER (<= 1.4.0) fills \code{smooth$logit.upper} with the
#' lower quantile; this implementation computes the upper quantile.
#'
#' @param engine \code{"rstan"} or \code{"INLA"}. Defaults to
#'   \code{getOption("surveyPrev.engine", "rstan")}.
#' @param data,geo,Amat,region.list,X,X.unit,responseType,response.type,unmatched.link,responseVar,strataVar,weightVar,regionVar,clusterVar,pc.u,pc.alpha,pc.u.phi,pc.alpha.phi,CI,formula,timeVar,time.model,space.model,include.time.unstruct,type.st,direct.est,direct.est.var,is.unit.level,is.agg,strataVar.within,totalVar,weight.strata,nsim,save.draws,smooth,... identical to \code{SUMMER::smoothSurvey()}.
#'
#' @return An object of class \code{SUMMERmodel.svy}, as documented in
#'   \code{SUMMER::smoothSurvey()}. Under \code{engine = "rstan"} the
#'   \code{$fit} element is a list carrying the \code{stanfit} and the
#'   posterior draws instead of an \code{inla} object.
#'
#' @importFrom survey svydesign svyby svymean
#' @importFrom stats as.formula model.frame model.matrix na.pass quantile median var sd qlogis
#' @export
smoothSurvey <- function(data, geo = NULL, Amat = NULL, region.list = NULL,
    X = NULL, X.unit = NULL, responseType = NULL,
    response.type = c("binary", "gaussian")[1], unmatched.link = FALSE,
    responseVar, strataVar = "strata", weightVar = "weights",
    regionVar = "region", clusterVar = "~v001+v002", pc.u = 1,
    pc.alpha = 0.01, pc.u.phi = 0.5, pc.alpha.phi = 2/3, CI = 0.95,
    formula = NULL, timeVar = NULL, time.model = c("rw1", "rw2")[1],
    space.model = c("bym2", "sar")[1], include.time.unstruct = FALSE,
    type.st = 1, direct.est = NULL, direct.est.var = NULL,
    is.unit.level = FALSE, is.agg = FALSE, strataVar.within = NULL,
    totalVar = NULL, weight.strata = NULL, nsim = 1000,
    save.draws = FALSE, smooth = TRUE,
    engine = getOption("surveyPrev.engine", "rstan"), ...)
{
    engine <- match.arg(engine, c("rstan", "INLA"))
    expit <- SUMMER::expit
    logit <- SUMMER::logit
    if (!is.null(responseType)) response.type <- responseType

    ## ------- rstan-unsupported modes: fail early and clearly -------
    if (engine == "rstan" && smooth) {
        if (!is.null(timeVar))
            stop("engine = 'rstan' does not support temporal models (timeVar) yet. Use engine = 'INLA'.")
        if (!is.null(formula))
            stop("engine = 'rstan' does not support user-supplied formula additions. Use engine = 'INLA'.")
        if (space.model[1] == "sar" && !is.null(Amat))
            stop("engine = 'rstan' does not support space.model = 'sar' yet. Use engine = 'INLA'.")
    }

    ## =====================================================================
    ## Shared data preparation -- verbatim from SUMMER::smoothSurvey()
    ## =====================================================================
    svy <- TRUE
    if (is.null(responseVar)) {
        stop("Response variable not specified")
    }
    if (is.null(response.type)) {
        stop("response.type not specified")
    }
    if (is.null(Amat) && smooth) {
        message("No spatial adjacency matrix is specified. IID area random effect is used.")
    }
    is.iid.space <- FALSE
    response.type <- tolower(response.type)
    if (response.type == "gaussian")
        is.agg <- FALSE
    if (response.type == "binary" && !is.null(X.unit)) {
        X.unit <- NULL
        warning("Unit-level covariates not implemented for binary response variable. Set X.unit = NULL.")
    }
    if (is.unit.level == FALSE && !is.null(X.unit)) {
        X.unit <- NULL
        warning("Area-level model is fitted. Set X.unit = NULL.")
    }
    if (is.agg && !is.null(X.unit)) {
        X.unit <- NULL
        warning("Unit-level covariates cannot be used when input data is aggregated to cluster level (is.agg = TRUE). Set X.unit = NULL.")
    }
    if (response.type == "gaussian" && is.unit.level) {
        is.nested <- TRUE
        clusterVar <- NULL
    }
    else {
        is.nested <- FALSE
    }
    if (is.nested && !is.null(X.unit) && !is.null(timeVar)) {
        stop("Unit-level nested error model with covariates is not implemented with temporal components yet.")
    }
    if (is.nested && !is.null(X.unit) && !is.null(strataVar.within)) {
        stop("Unit-level nested error model with covariates is not implemented with stratification components yet.")
    }
    if (is.unit.level) {
        if (response.type == "binary") {
            message("Fitting unit-level model.")
            msg <- "Unit-level model"
        }
        else {
            message("Fitting unit-level nested error model.")
            msg <- "Unit-level nested error model"
        }
        if (is.null(strataVar.within)) {
            message("Within region stratification variable (strataVar.within) not defined. Unstratified model is fitted.")
            strataVar.within <- "strata0"
            data$strata0 <- 1
        }
        if (is.null(clusterVar) && !is.nested)
            stop("Cluster variable (clusterVar) not defined.")
        if (is.null(data))
            stop("Survey dataset not defined.")
        data$response0 <- data[, responseVar]
        data$region0 <- as.character(data[, regionVar])
        if (is.null(Amat)) {
            if (!is.null(region.list)) {
                regions <- unique(region.list)
                if (sum(!data$region0 %in% regions) > 0) {
                  stop("'region.list' is specified but there are regions in the data not in the 'region.list'.")
                }
            }
            else {
                regions <- unique(data$region0)
            }
            Amat <- matrix(0, length(regions), length(regions))
            colnames(Amat) <- rownames(Amat) <- regions
            is.iid.space <- TRUE
        }
        if (sum(!data$region0 %in% colnames(Amat)) > 0) {
            stop("Exist regions in the data frame but not in Amat.")
        }
        data$region0 <- factor(data$region0, levels = colnames(Amat))
        data$strata0 <- factor(data[, strataVar.within])
        if (!is.null(clusterVar))
            data$cluster0 <- data[, clusterVar]
        if (!is.null(X.unit)) {
            if (sum(X.unit %in% colnames(data)) != length(X.unit)) {
                stop("Exist columns specified X.unit but not in the data.")
            }
            if (!is.null(X) && sum(X.unit %in% colnames(X)) !=
                length(X.unit)) {
                stop("Exist columns specified X.unit but not in X.")
            }
            if (is.null(X)) {
                data.sub <- data[, c(X.unit, "region0")]
                ## qualified: surveyPrev imports terra::aggregate, which would
                ## otherwise shadow stats::aggregate here
                X <- stats::aggregate(. ~ region0, data = data.sub,
                  FUN = function(x) {
                    mean(x, na.rm = TRUE)
                  })
            }
        }
        if (!is.agg && !is.nested) {
            vars <- c("cluster0", "region0", "strata0")
            if (!is.null(timeVar)) {
                data$time0 <- data[, timeVar]
                vars <- c(vars, "time0")
            }
            counts <- SUMMER::getCounts(data[, c(vars, "response0")],
                variables = "response0", by = vars, drop = TRUE)
        }
        else if (is.agg) {
            if (!is.null(timeVar)) {
                data$time0 <- data[, timeVar]
            }
            if (is.null(totalVar) && response.type == "binary") {
                stop("Which column correspond to cluster total is not specified")
            }
            data$total <- data[, totalVar]
            counts <- data
        }
        else {
            counts <- data
        }
        if (!is.null(weight.strata)) {
            if (sum(!data$strata0 %in% colnames(weight.strata)) >
                0)
                stop("Exist within-area strata (strataVar.within) not in the weight.strata data frame.")
            stratalist <- unique(data$strata0)
        }
        else {
            stratalist <- NULL
        }
    }
    else if (!is.null(direct.est)) {
        msg <- "Area-level model using direct estimates as input"
        message("Using direct estimates as input instead of survey data.")
        data <- direct.est
        data$region0 <- as.character(direct.est[, regionVar])
        if (is.null(Amat)) {
            if (!is.null(region.list)) {
                regions <- unique(region.list)
                if (sum(!data$region0 %in% regions) > 0) {
                  stop("'region.list' is specified but there are regions in the data not in the 'region.list'.")
                }
            }
            else {
                regions <- unique(data$region0)
            }
            Amat <- matrix(0, length(regions), length(regions))
            colnames(Amat) <- rownames(Amat) <- regions
            is.iid.space <- TRUE
        }
        if (sum(!data$region0 %in% colnames(Amat)) > 0) {
            stop("Exist regions in the data frame but not in Amat.")
        }
        data$region0 <- factor(data$region0, levels = colnames(Amat))
        data$response0 <- direct.est[, responseVar]
        if (is.null(direct.est.var)) {
            stop("Need to specify column for the variance of direct estimates")
        }
        data$var0 <- direct.est[, direct.est.var]
    }
    else {
        msg <- "Area-level model using survey data as input"
        if (!is.data.frame(data)) {
            stop("Input data needs to be a data frame")
        }
        if (is.null(strataVar)) {
            message("Strata not defined. Ignoring stratification")
        }
        if (is.null(clusterVar)) {
            message("cluster not specified. Ignoring clustering")
            clusterVar <- "~1"
        }
        data$response0 <- data[, responseVar]
        data$region0 <- as.character(data[, regionVar])
        if (is.null(Amat)) {
            if (!is.null(region.list)) {
                regions <- unique(region.list)
                if (sum(!data$region0 %in% regions) > 0) {
                  stop("'region.list' is specified but there are regions in the data not in the 'region.list'.")
                }
            }
            else {
                regions <- unique(data$region0)
            }
            Amat <- matrix(0, length(regions), length(regions))
            colnames(Amat) <- rownames(Amat) <- regions
            is.iid.space <- TRUE
        }
        if (sum(!data$region0 %in% colnames(Amat)) > 0)
            stop("Exist regions in the data frame but not in Amat.")
        data$region0 <- factor(data$region0, levels = colnames(Amat))
        if (svy) {
            data$weights0 <- data[, weightVar]
            if (is.null(strataVar)) {
                data$strata0 <- 1
            }
            else {
                data$strata0 <- factor(data[, strataVar])
            }
        }
    }
    if (!is.null(timeVar))
        data$time0 <- data[, timeVar]
    if (is.null(rownames(Amat))) {
        stop("Row names of Amat needs to be specified to region names.")
    }
    if (is.null(colnames(Amat))) {
        stop("Column names of Amat needs to be specified to region names.")
    }
    if (sum(rownames(Amat) != colnames(Amat)) > 0) {
        stop("Row and column names of Amat needs to be the same.")
    }
    if (!is.null(X)) {
        if (regionVar %in% colnames(X)) {
            region.col <- which(colnames(X) == regionVar)
        }
        else {
            region.col <- 1
        }
        if (sum(X[, region.col] %in% colnames(Amat) == FALSE) >
            0 || sum(colnames(Amat) %in% X[, region.col] == FALSE) >
            0) {
            stop(paste0(colnames(X)[region.col], "is used as region identifier in the input covariate X. It does not match the region names in the data."))
        }
    }
    if (response.type == "binary") {
        FUN <- expit
    }
    else if (response.type == "gaussian") {
        FUN <- function(x) {
            x
        }
    }
    if (engine == "INLA") {
        if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
            stop("You need to install the packages 'INLA' to use engine = 'INLA'. Please run in your R terminal:\n  install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)")
        }
        if (isTRUE(requireNamespace("INLA", quietly = TRUE))) {
            if (!is.element("INLA", (.packages()))) {
                attachNamespace("INLA")
            }
        }
    }
    hyperpc1 <- list(prec = list(prior = "pc.prec", param = c(pc.u,
        pc.alpha)))
    hyperpc2 <- list(prec = list(prior = "pc.prec", param = c(pc.u,
        pc.alpha)), phi = list(prior = "pc", param = c(pc.u.phi,
        pc.alpha.phi)))
    if (sum(!data$region0 %in% colnames(Amat)) > 0) {
        stop("Exist regions in data but not in the Amat.")
    }
    if (!is.unit.level) {
        if (svy && is.null(direct.est)) {
            design <- survey::svydesign(ids = stats::formula(clusterVar),
                weights = ~weights0, strata = ~strata0, data = data,
                ...)
            if (!is.null(timeVar)) {
                mean <- survey::svyby(formula = ~response0, by = ~region0 +
                  time0, design = design, survey::svymean, drop.empty.groups = FALSE)
                time.i <- as.numeric(as.character(mean$time0))
            }
            else {
                mean <- survey::svyby(formula = ~response0, by = ~region0,
                  design = design, survey::svymean, drop.empty.groups = FALSE)
            }
            name.i <- mean$region0
            p.i <- mean$response0
            var.i <- mean$se^2
            if (response.type == "binary") {
                ht <- log(p.i/(1 - p.i))
                ht.v <- var.i/(p.i^2 * (1 - p.i)^2)
                ht.prec <- 1/ht.v
            }
            else if (response.type == "gaussian") {
                ht <- p.i
                ht.v <- var.i
                ht.prec <- 1/ht.v
            }
            else {
                stop("response.type argument only supports binary or gaussian at the time.")
            }
            n <- y <- NA
        }
        else if (is.null(direct.est)) {
            if (!is.null(timeVar)) {
                mean <- stats::aggregate(response0 ~ region0 +
                  time0, data = data, FUN = function(x) {
                  c(mean(x), length(x), sum(x))
                }, drop = FALSE)
                name.i <- mean$region0
                time.i <- as.numeric(as.character(mean$time0))
                mean <- data.frame(mean[, -c(1:2)])
            }
            else {
                mean <- stats::aggregate(response0 ~ region0,
                  data = data, FUN = function(x) {
                    c(mean(x), length(x), sum(x))
                  }, drop = FALSE)
                name.i <- mean$region0
                mean <- data.frame(mean[, -1])
            }
            colnames(mean) <- c("mean", "n", "y")
            p.i <- mean$mean
            var.i <- p.i * (1 - p.i)/mean$n
            if (response.type == "binary") {
                ht <- log(p.i/(1 - p.i))
                ht.v <- var.i/(p.i^2 * (1 - p.i)^2)
                ht.prec <- 1/ht.v
            }
            else if (response.type == "gaussian") {
                ht <- p.i
                ht.v <- var.i
                ht.prec <- 1/ht.v
            }
            else {
                stop("response.type argument only supports binary or gaussian at the time.")
            }
            n <- mean$n
            y <- mean$y
        }
        else {
            p.i <- data$response0
            var.i <- data$var0
            if (response.type == "binary") {
                ht <- log(p.i/(1 - p.i))
                ht.v <- var.i/(p.i^2 * (1 - p.i)^2)
                ht.prec <- 1/ht.v
            }
            else if (response.type == "gaussian" && !unmatched.link) {
                ht <- p.i
                ht.v <- var.i
                ht.prec <- 1/ht.v
            }
            else if (response.type == "gaussian" && unmatched.link) {
                p.i <- expit(data$response0)
                var.i <- data$var0 * (p.i^2 * (1 - p.i)^2)
                ht <- log(p.i/(1 - p.i))
                ht.v <- var.i/(p.i^2 * (1 - p.i)^2)
                ht.prec <- 1/ht.v
            }
            else {
                stop("response.type argument only supports binary or gaussian at the time.")
            }
            n <- NA
            y <- NA
            time.i <- as.numeric(as.character(data$time0))
            name.i <- as.character(data$region0)
        }
        dat <- data.frame(HT.est = p.i, HT.var = var.i, HT.logit.est = ht,
            HT.logit.var = ht.v, HT.logit.prec = ht.prec, n = n,
            y = y)
    }
    else {
        dat <- counts
        dat$y <- dat$response0
        name.i <- dat$region0
        if (!is.null(timeVar))
            time.i <- as.numeric(as.character(dat$time0))
    }
    if (!is.null(timeVar)) {
        dat$time <- time.i
        dat$time.struct <- dat$time.unstruct <- time.int <- dat$time -
            min(dat$time) + 1
    }
    regnames <- as.character(name.i)
    dat$region <- as.character(name.i)
    dat$region.unstruct <- dat$region.struct <- dat$region.int <- match(dat$region,
        rownames(Amat))
    if (!is.unit.level)
        dat$HT.logit.est[is.infinite(abs(dat$HT.logit.est))] <- NA
    if (!is.null(timeVar)) {
        dat <- dat[order(dat[, "time.struct"], dat[, "region.struct"]),
            ]
    }
    else {
        dat <- dat[order(dat[, "region.struct"]), ]
    }
    if (!svy && response.type == "binary" && !is.unit.level) {
        formulatext <- "y ~ 1"
    }
    else if (!is.unit.level) {
        if (unmatched.link) {
            formulatext <- "HT.est ~ 1"
        }
        else {
            formulatext <- "HT.logit.est ~ 1"
        }
    }
    else if (length(unique(data$strata0)) == 1) {
        formulatext <- "y ~ 1"
    }
    else {
        formulatext <- "y ~ strata0 - 1"
    }
    fixed <- NULL
    if (!is.null(X) && is.null(X.unit)) {
        X <- data.frame(X)
        fixed <- colnames(X)[colnames(X) != regionVar]
        colnames(X)[colnames(X) == regionVar] <- "region"
        if (sum(fixed %in% colnames(dat)) > 0) {
            message("The following covariates exist in the input data frame. They are replaced with region-level covariates provided in X: ",
                fixed[fixed %in% colnames(dat)])
            dat <- dat[, !colnames(dat) %in% fixed]
        }
        dat <- merge(dat, X, by = "region", all = TRUE)
        formulatext <- paste(formulatext, " + ", paste(fixed,
            collapse = " + "))
    }
    else if (is.nested && !is.null(X.unit)) {
        formulatext <- paste(formulatext, " + ", paste(X.unit,
            collapse = " + "))
        X <- data.frame(X)
        fixed <- colnames(X)[colnames(X) != regionVar]
        colnames(X)[colnames(X) == regionVar] <- "region"
        X <- X[, colnames(X) %in% c("region", X.unit)]
        for (j in colnames(dat)) {
            if (!j %in% c("region", X.unit)) {
                X[, j] <- dat[match(X$region, dat$region), j]
            }
        }
        X$y <- NA
        dat <- rbind(X, dat)
        out.index <- 1:dim(X)[1]
    }
    else if (is.nested) {
        X <- data.frame(region = colnames(Amat))
        for (j in colnames(dat)) {
            if (!j %in% c("region")) {
                X[, j] <- dat[match(X$region, dat$region), j]
            }
        }
        X$y <- NA
        dat <- rbind(X, dat)
        out.index <- 1:dim(X)[1]
    }
    if (is.null(formula)) {
        if (is.iid.space) {
            formula <- paste(formulatext, "f(region.struct, model = 'iid', hyper = hyperpc1)",
                sep = "+")
        }
        else if (space.model == "bym2") {
            formula <- paste(formulatext, "f(region.struct, graph=Amat, model='bym2', hyper = hyperpc2, scale.model = TRUE)",
                sep = "+")
        }
        else {
            hyperSAR <- list(prec = list(prior = "pc.prec", param = c(pc.u,
                pc.alpha)))
            argsSAR <- list(W = Amat, rho.min = 0, rho.max = 1)
            formula <- paste(formulatext, "f(region.struct, model='slm', args.slm = argsSAR, hyper = hyperSAR)",
                sep = "+")
        }
        if (!is.null(timeVar)) {
            formula <- paste(formula, "f(time.unstruct, model = 'iid', hyper = hyperpc1) + f(time.struct, model=tolower(time.model), hyper = hyperpc1, scale.model = TRUE)",
                sep = "+")
            if (type.st == 1) {
                formula <- paste(formula, "f(region.int, model = 'iid', hyper = hyperpc1, group = time.int, control.group = list(model ='iid', hyper = hyperpc1))",
                  sep = "+")
            }
            else if (type.st == 2) {
                formula <- paste(formula, "f(region.int, model = 'besag', graph = Amat, scale.model = TRUE, param=hyperpc1, group = time.int, control.group = list(model ='iid'))",
                  sep = "+")
            }
            else if (type.st == 3) {
                formula <- paste(formula, "f(region.int, model = 'iid', hyper = hyperpc1, group = time.int, control.group = list(model =tolower(time.model), scale.model = TRUE))",
                  sep = "+")
            }
            else {
                formula <- paste(formula, "f(region.int, model = 'besag', graph = Amat, scale.model = TRUE, hyper = hyperpc1, group = time.int, control.group = list(model =tolower(time.model), hyper = hyperpc1, scale.model = TRUE))",
                  sep = "+")
            }
        }
        formula <- as.formula(formula)
    }
    else {
        formula <- as.formula(paste(formulatext, formula, sep = "+"))
    }

    ## =====================================================================
    ## Model fit -- the only engine-specific block
    ## =====================================================================
    if (smooth) {
        if (engine == "INLA") {
            if (is.unit.level && response.type == "binary") {
                fit <- INLA::inla(formula, family = "betabinomial",
                    Ntrials = dat$total, control.compute = list(dic = T,
                      mlik = T, cpo = T, config = TRUE, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE),
                    lincomb = NULL, quantiles = c((1 - CI)/2, 0.5,
                      1 - (1 - CI)/2))
            }
            else if (is.unit.level && response.type == "gaussian") {
                fit <- INLA::inla(formula, family = "gaussian", control.compute = list(dic = T,
                    mlik = T, cpo = T, config = TRUE, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE),
                    lincomb = NULL, quantiles = c((1 - CI)/2, 0.5,
                      1 - (1 - CI)/2))
            }
            else if (!svy && response.type == "binary") {
                fit <- INLA::inla(formula, family = "binomial", Ntrials = n,
                    control.compute = list(dic = T, mlik = T, cpo = T,
                      config = save.draws, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE,
                      link = 1), lincomb = NULL, quantiles = c((1 -
                      CI)/2, 0.5, 1 - (1 - CI)/2))
            }
            else if (!svy && response.type == "gaussian") {
                fit <- INLA::inla(formula, family = "gaussian", control.compute = list(dic = T,
                    mlik = T, cpo = T, config = save.draws, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE),
                    control.family = list(hyper = list(prec = list(initial = log(1),
                      fixed = TRUE))), lincomb = NULL, quantiles = c((1 -
                      CI)/2, 0.5, 1 - (1 - CI)/2))
            }
            else if (unmatched.link && response.type == "binary") {
                fit <- INLA::inla(formula, family = "gaussian", control.compute = list(dic = T,
                    mlik = T, cpo = T, config = save.draws, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE),
                    control.family = list(link = "logit", hyper = list(prec = list(initial = log(1),
                      fixed = TRUE))), scale = 1/dat$HT.var, lincomb = NULL,
                    quantiles = c((1 - CI)/2, 0.5, 1 - (1 - CI)/2))
            }
            else if (unmatched.link) {
                fit <- INLA::inla(formula, family = "gaussian", control.compute = list(dic = T,
                    mlik = T, cpo = T, config = save.draws, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE),
                    control.family = list(link = "logit", hyper = list(prec = list(initial = log(1),
                      fixed = TRUE))), scale = 1/dat$HT.var, lincomb = NULL,
                    quantiles = c((1 - CI)/2, 0.5, 1 - (1 - CI)/2))
            }
            else {
                fit <- INLA::inla(formula, family = "gaussian", control.compute = list(dic = T,
                    mlik = T, cpo = T, config = save.draws, return.marginals.predictor = TRUE),
                    data = dat, control.predictor = list(compute = TRUE),
                    control.family = list(hyper = list(prec = list(initial = log(1),
                      fixed = TRUE))), scale = dat$HT.logit.prec,
                    lincomb = NULL, quantiles = c((1 - CI)/2, 0.5,
                      1 - (1 - CI)/2))
            }
        }
        else {
            fit <- .stan_smoothSurvey_fit(dat = dat, formulatext = formulatext,
                Amat = Amat, is.iid.space = is.iid.space,
                is.unit.level = is.unit.level, response.type = response.type,
                svy = svy, unmatched.link = unmatched.link,
                pc.u = pc.u, pc.alpha = pc.alpha, pc.u.phi = pc.u.phi,
                pc.alpha.phi = pc.alpha.phi, nsim = nsim)
        }
    }
    else {
        fit <- NULL
    }

    ## =====================================================================
    ## Shared output assembly -- verbatim from SUMMER::smoothSurvey(),
    ## with the engine-specific posterior access factored into
    ## .linpred_draws() and .posterior_sample_engine().
    ## =====================================================================
    n <- max(dat$region.struct)
    temp <- NA
    if (!is.unit.level) {
        if (!is.null(timeVar)) {
            n <- n * max(dat$time.struct)
            temp <- dat$time[1:n]
        }
        proj <- data.frame(region = dat$region[1:n], time = temp,
            mean = NA, var = NA, median = NA, lower = NA, upper = NA,
            logit.mean = NA, logit.var = NA, logit.median = NA,
            logit.lower = NA, logit.upper = NA)
    }
    else {
        if (!is.null(timeVar)) {
            temp <- 1:max(dat$time.struct)
        }
        proj <- expand.grid(region = colnames(Amat), time = temp,
            strata = unique(dat$strata0))
        proj <- cbind(proj, data.frame(mean = NA, var = NA, median = NA,
            lower = NA, upper = NA, logit.mean = NA, logit.var = NA,
            logit.median = NA, logit.lower = NA, logit.upper = NA))
    }
    if (smooth) {
        for (i in 1:dim(proj)[1]) {
            if (!is.unit.level) {
                tmp <- .linpred_draws(fit, i, engine)
            }
            else {
                if (is.nested && !is.null(X.unit)) {
                  which <- which(dat[out.index, ]$region == proj$region[i] &
                    dat[out.index, ]$strata0 == proj$strata[i])[1]
                  which <- out.index[which]
                }
                else if (is.null(timeVar)) {
                  which <- which(dat$region == proj$region[i] &
                    dat$strata0 == proj$strata[i])[1]
                }
                else {
                  which <- which(dat$region == proj$region[i] &
                    dat$strata0 == proj$strata[i] & dat$time ==
                    proj$time[i])[1]
                }
                if (is.na(which))
                  next
                tmp <- .linpred_draws(fit, which, engine)
            }
            if (!svy && response.type == "binary") {
                tmp2 <- tmp
                proj[i, "logit.mean"] <- mean(tmp2)
                proj[i, "logit.var"] <- var(tmp2)
                proj[i, "logit.lower"] <- quantile(tmp2, (1 -
                  CI)/2)
                proj[i, "logit.upper"] <- quantile(tmp2, 1 -
                  (1 - CI)/2)
                proj[i, "logit.median"] <- median(tmp2)
                proj[i, "mean"] <- fit$summary.fitted.values[i,
                  "mean"]
                proj[i, "var"] <- fit$summary.fitted.values[i,
                  "sd"]^2
                proj[i, "lower"] <- fit$summary.fitted.values[i,
                  3]
                proj[i, "upper"] <- fit$summary.fitted.values[i,
                  5]
                proj[i, "median"] <- fit$summary.fitted.values[i,
                  "0.5quant"]
            }
            else {
                tmp2 <- apply(tmp, 2, FUN)
                proj[i, "mean"] <- mean(tmp2)
                proj[i, "var"] <- var(tmp2)
                proj[i, "lower"] <- quantile(tmp2, (1 - CI)/2)
                proj[i, "upper"] <- quantile(tmp2, 1 - (1 - CI)/2)
                proj[i, "median"] <- median(tmp2)
                if (response.type == "binary") {
                  proj[i, "logit.mean"] <- mean(tmp)
                  proj[i, "logit.var"] <- var(tmp)
                  proj[i, "logit.lower"] <- quantile(tmp, (1 -
                    CI)/2)
                  ## NOTE: SUMMER (<= 1.4.0) uses the lower quantile here;
                  ## fixed to the upper quantile in this implementation.
                  proj[i, "logit.upper"] <- quantile(tmp, 1 - (1 -
                    CI)/2)
                  proj[i, "logit.median"] <- median(tmp)
                }
            }
        }
        if ("strata" %in% colnames(proj)) {
            draws.out <- proj[, c("region", "time", "strata")]
        }
        else {
            draws.out <- proj[, c("region", "time")]
        }
        for (j in 1:nsim) draws.out[, paste0("sample:", j)] <- NA
        sampAll <- NULL
        if (save.draws) {
            sampAll <- .posterior_sample_engine(fit, nsim, engine)
            for (i in 1:dim(draws.out)[1]) {
                for (j in 1:nsim) {
                  draws.out[i, paste0("sample:", j)] <- sampAll[[j]]$latent[i,
                    1]
                  if (!include.time.unstruct && !is.null(timeVar)) {
                    draws.out[i, paste0("sample:", j)] <- draws.out[i,
                      paste0("sample:", j)] - sampAll[[j]]$latent[paste0("time.unstruct:",
                      draws.out$time[i, 1])]
                  }
                }
            }
        }
        if (is.unit.level && !is.null(weight.strata) && length(unique(data$strata0)) >
            1) {
            proj.agg <- expand.grid(region = colnames(Amat),
                time = temp)
            proj.agg <- cbind(proj.agg, data.frame(mean = NA,
                var = NA, median = NA, lower = NA, upper = NA,
                logit.mean = NA, logit.var = NA, logit.median = NA,
                logit.lower = NA, logit.upper = NA))
            if (is.null(sampAll))
                sampAll <- .posterior_sample_engine(fit, nsim, engine)
            for (i in 1:dim(proj.agg)[1]) {
                if (is.null(timeVar)) {
                  which <- which(weight.strata[, regionVar] ==
                    proj.agg$region[i])
                }
                else {
                  which <- which(weight.strata[, regionVar] ==
                    proj.agg$region[i] & weight.strata[, timeVar] ==
                    proj.agg$time[i])
                }
                draws <- rep(0, nsim)
                for (j in 1:length(sampAll)) {
                  r <- sampAll[[j]]$latent[paste0("region.struct:",
                    match(proj.agg$region[i], colnames(Amat))),
                    ]
                  if (!is.null(timeVar))
                    r <- r + sampAll[[j]]$latent[paste0("time.struct:",
                      proj.agg$time[i]), ]
                  if (!is.null(timeVar) && include.time.unstruct)
                    r <- r + sampAll[[j]]$latent[paste0("time.unstruct:",
                      proj.agg$time[i]), ]
                  if (!is.null(X)) {
                    for (xx in fixed) {
                      slope = sampAll[[j]]$latent[paste0(xx,
                        ":1"), ]
                      r <- r + slope * dat[which(dat$region ==
                        proj.agg$region[i])[1], xx]
                    }
                  }
                  for (s in stratalist) {
                    intercept = sampAll[[j]]$latent[paste0("strata0",
                      s, ":1"), ]
                    if (response.type == "binary") {
                      draws[j] <- draws[j] + expit(r + intercept) *
                        weight.strata[which, s]
                    }
                    else if (response.type == "gaussian") {
                      draws[j] <- draws[j] + (r + intercept) *
                        weight.strata[which, s]
                    }
                  }
                }
                proj.agg[i, "mean"] <- mean(draws)
                proj.agg[i, "var"] <- var(draws)
                proj.agg[i, "lower"] <- quantile(draws, (1 -
                  CI)/2)
                proj.agg[i, "upper"] <- quantile(draws, 1 - (1 -
                  CI)/2)
                proj.agg[i, "median"] <- median(draws)
                if (response.type == "binary") {
                  proj.agg[i, "logit.mean"] <- mean(logit(draws))
                  proj.agg[i, "logit.var"] <- var(logit(draws))
                  proj.agg[i, "logit.lower"] <- quantile(logit(draws),
                    (1 - CI)/2)
                  proj.agg[i, "logit.upper"] <- quantile(logit(draws),
                    1 - (1 - CI)/2)
                  proj.agg[i, "logit.median"] <- median(logit(draws))
                }
            }
        }
        else {
            proj.agg <- NULL
        }
    }
    else {
        proj.agg <- NULL
    }
    if (!is.unit.level) {
        HT <- dat[, !colnames(dat) %in% c("region.struct", "region.unstruct",
            "region.int", "time.struct", "time.unstruct", "time.int")]
        HT <- cbind(region = HT[, "region"], HT[, colnames(HT) !=
            "region"])
        if ("time" %in% names(HT)) {
            HT <- cbind(region = HT[, "region"], time = HT[,
                "time"], HT[, !(colnames(HT) %in% c("region",
                "time"))])
        }
        for (i in 1:dim(HT)[2]) HT[is.nan(HT[, i]), i] <- NA
        if (sum(!is.na(HT$n)) == 0)
            HT <- HT[, colnames(HT) != "n"]
        if (sum(!is.na(HT$y)) == 0)
            HT <- HT[, colnames(HT) != "y"]
        if (sum(!is.na(proj$time)) == 0)
            proj <- proj[, colnames(proj) != "time"]
        if (response.type == "gaussian") {
            HT <- HT[, !colnames(HT) %in% c("HT.logit.est", "HT.logit.var",
                "HT.logit.prec")]
            proj <- proj[, !colnames(proj) %in% c("logit.mean",
                "logit.var", "logit.median", "logit.lower", "logit.upper")]
        }
    }
    else {
        HT <- NULL
    }
    if (!is.null(HT)) {
        direct <- HT
        colnames(direct) <- gsub("HT", "direct", colnames(HT))
    }
    else {
        direct <- NULL
    }
    out <- list(direct = direct, smooth = proj, smooth.overall = proj.agg,
        fit = fit, CI = CI, Amat = Amat, response.type = response.type,
        formula = formula, msg = msg, HT = HT)
    if (save.draws) {
        out$draws <- sampAll
        out$draws.est <- draws.out
    }
    class(out) <- "SUMMERmodel.svy"
    return(out)
}


## =========================================================================
## rstan engine internals
## =========================================================================

#' Stan fit for smoothSurvey()
#'
#' Maps the smoothSurvey model branches onto the two package Stan models:
#' beta-binomial (unit-level binary) and gaussian (everything else, with
#' known or free observation variance and identity or logit link). Returns
#' a list with `$eta_draws` (posterior draws x data rows of the linear
#' predictor), `$samples` (nsim posterior samples shaped like
#' `INLA::inla.posterior.sample()` output: `$latent` with linear-predictor
#' rows first, then named `region.struct:k` and fixed-effect `<name>:1`
#' rows), and `$stanfit`.
#'
#' @keywords internal
.stan_smoothSurvey_fit <- function(dat, formulatext, Amat, is.iid.space,
                                   is.unit.level, response.type, svy,
                                   unmatched.link,
                                   pc.u, pc.alpha, pc.u.phi, pc.alpha.phi,
                                   nsim,
                                   stan.chains = 4, stan.iter = 1000,
                                   stan.warmup = 500, stan.seed = 1,
                                   stan.cores = getOption("mc.cores", 1L)) {
  ## ----- fixed-effects design from the shared formula text -----
  rhs <- sub("^[^~]*~", "", formulatext)
  ff <- stats::as.formula(paste("~", rhs))
  mf <- stats::model.frame(ff, data = dat, na.action = stats::na.pass)
  mm <- stats::model.matrix(ff, mf)
  mm[is.na(mm)] <- 0
  colnms <- colnames(mm)
  P <- ncol(mm)
  Nrow <- nrow(dat)
  R <- nrow(Amat)
  region <- dat$region.struct

  ## ----- spatial structure -----
  use_bym2 <- as.integer(!is.iid.space)
  if (use_bym2 == 1) {
    W <- as.matrix(Amat)
    diag(W) <- 0
    W <- (W != 0) * 1
    ut <- which(upper.tri(W) & W == 1, arr.ind = TRUE)
    node1 <- as.integer(ut[, 1]); node2 <- as.integer(ut[, 2])
    sf <- bym2_scaling_factor(W)
    phi_pc <- bym2_phi_pc_grid(sf$Qinv_c, sf$scaling, u = pc.u.phi,
                               alpha = pc.alpha.phi)
    scaling_factor <- sf$scaling
    phi_grid <- phi_pc$grid; phi_logp <- phi_pc$logp
  } else {
    node1 <- integer(0); node2 <- integer(0)
    scaling_factor <- 1
    phi_grid <- c(0, 1); phi_logp <- c(0, 0)
  }
  spatial <- list(use_bym2 = use_bym2, N_edges = length(node1),
                  node1 = as.array(node1), node2 = as.array(node2),
                  scaling_factor = scaling_factor,
                  Kphi = length(phi_grid),
                  phi_grid = as.array(phi_grid),
                  phi_logp = as.array(phi_logp))
  beta_prec <- as.array(ifelse(colnms == "(Intercept)", 0, 0.001))
  lambda <- -log(pc.alpha) / pc.u
  ctl <- list(control = list(max_treedepth = 12, adapt_delta = 0.95),
              chains = stan.chains, iter = stan.iter, warmup = stan.warmup,
              seed = stan.seed, cores = stan.cores, refresh = 0)

  if (is.unit.level && response.type == "binary") {
    ## ----- beta-binomial (reuses betabinom.stan) -----
    yresp <- dat$y
    ntot <- dat$total
    obs <- which(!is.na(yresp) & !is.na(ntot) & ntot > 0)
    stan_data <- c(list(
      N = length(obs), P = P,
      X = mm[obs, , drop = FALSE],
      y = as.array(as.integer(yresp[obs])),
      Ntrials = as.array(as.integer(ntot[obs])),
      R = R, region = as.array(as.integer(region[obs])),
      Npred = Nrow,
      Xpred = mm,
      region_pred = as.array(as.integer(region)),
      beta_prec = beta_prec,
      od_mean = 0, od_sd = 1 / sqrt(0.4),      # INLA betabinomial default
      lambda = lambda), spatial)
    fitS <- do.call(rstan::sampling,
                    c(list(object = get_betabinom_stanmodel(),
                           data = stan_data), ctl))
    post <- rstan::extract(fitS, pars = c("beta", "re", "fitted"),
                           permuted = TRUE)
    eta_draws <- stats::qlogis(post$fitted)             # draws x Nrow
  } else {
    ## ----- gaussian branches (fh_gaussian.stan) -----
    if (is.unit.level && response.type == "gaussian") {
      yresp <- dat$y; vknown <- NULL
      link_logit <- 0L; free_prec <- 1L
    } else if (!svy && response.type == "binary") {
      stop("engine = 'rstan' does not support the non-survey binomial model (unreachable in the current smoothSurvey).")
    } else if (!svy && response.type == "gaussian") {
      yresp <- dat$y; vknown <- rep(1, Nrow)
      link_logit <- 0L; free_prec <- 0L
    } else if (unmatched.link) {
      yresp <- dat$HT.est; vknown <- dat$HT.var
      link_logit <- 1L; free_prec <- 0L
    } else {
      yresp <- dat$HT.logit.est; vknown <- dat$HT.logit.var
      link_logit <- 0L; free_prec <- 0L
    }
    if (free_prec == 1L) {
      obs <- which(!is.na(yresp))
      sdv <- rep(1, length(obs))                        # ignored by the model
    } else {
      obs <- which(!is.na(yresp) & !is.na(vknown) & vknown > 0)
      sdv <- sqrt(vknown[obs])
    }
    stan_data <- c(list(
      Nrow = Nrow, R = R, P = P, Xall = mm,
      region = as.array(as.integer(region)),
      N = length(obs), obs_idx = as.array(as.integer(obs)),
      y = as.array(yresp[obs]), sdv = as.array(sdv),
      link_logit = link_logit, free_prec = free_prec,
      beta_prec = beta_prec, lambda = lambda), spatial)
    fitS <- do.call(rstan::sampling,
                    c(list(object = get_fh_stanmodel(),
                           data = stan_data), ctl))
    post <- rstan::extract(fitS, pars = c("beta", "re", "eta"),
                           permuted = TRUE)
    eta_draws <- post$eta                               # draws x Nrow
  }

  ## ----- nsim posterior samples with INLA-style latent layout -----
  ndraw <- nrow(eta_draws)
  sel <- sample.int(ndraw, nsim, replace = ndraw < nsim)
  re_names <- paste0("region.struct:", seq_len(R))
  fx_names <- paste0(colnms, ":1")
  samples <- lapply(sel, function(i) {
    latent <- matrix(c(eta_draws[i, ], post$re[i, ], post$beta[i, ]),
                     ncol = 1)
    rownames(latent) <- c(paste0("Predictor:", seq_len(Nrow)),
                          re_names, fx_names)
    list(latent = latent)
  })

  list(eta_draws = eta_draws, samples = samples, stanfit = fitS,
       engine = "rstan")
}

#' Linear-predictor posterior draws for one data row
#' @keywords internal
.linpred_draws <- function(fit, i, engine) {
  if (engine == "INLA") {
    matrix(INLA::inla.rmarginal(1e+05, fit$marginals.linear.predictor[[i]]))
  } else {
    matrix(fit$eta_draws[, i])
  }
}

#' Posterior samples in inla.posterior.sample() shape
#' @keywords internal
.posterior_sample_engine <- function(fit, nsim, engine) {
  if (engine == "INLA") {
    INLA::inla.posterior.sample(n = nsim, result = fit, intern = TRUE)
  } else {
    fit$samples
  }
}
