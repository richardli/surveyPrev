% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterModel.R
\name{clusterModel}
\alias{clusterModel}
\title{Calculate cluster model estimates using beta binomial model}
\usage{
clusterModel(
  data,
  cluster.info,
  admin.info,
  X = NULL,
  X.unit = NULL,
  X.pixel = NULL,
  admin,
  CI = 0.95,
  model = c("bym2", "iid"),
  stratification = FALSE,
  aggregation = FALSE,
  nested = FALSE,
  interact = FALSE,
  overdisp.mean = 0,
  overdisp.prec = 0.4,
  pc.u = 1,
  pc.alpha = 0.01,
  pc.u.phi = 0.5,
  pc.alpha.phi = 2/3
)
}
\arguments{
\item{data}{dataframe that contains the indicator of interests(column name is value), output of getDHSindicator function}

\item{cluster.info}{dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.}

\item{admin.info}{dataframe that contains population and urban/rural proportion at specific admin level}

\item{X}{dataframe that contains areal covariates, the first column should be the same admin name as in admin.info$data.}

\item{X.unit}{dataframe that contains unit covariates, must contain cluster}

\item{X.pixel}{dataframe that contains pixel covariates, must contain 1. admin1.name or admin2.name.full, 2. Population, 3. strata if stratification==T}

\item{admin}{admin level for the model}

\item{CI}{Credible interval to be used. Default to 0.95.}

\item{model}{smoothing model used in the random effect. Options are independent ("iid") or spatial ("bym2").}

\item{stratification}{whether or not to include urban/rural stratum.}

\item{aggregation}{whether or not report aggregation results.}

\item{nested}{whether or not to fit a nested model.}

\item{interact}{whether or not to fit a admin1 x urban/rural  model.}

\item{overdisp.mean}{prior mean for logit(d), where d is the intracluster correlation.}

\item{overdisp.prec}{prior precision for logit(d), where d is the intracluster correlation.}

\item{pc.u}{pc prior u for iid or bym2 precision.}

\item{pc.alpha}{pc prior alpha for iid or bym2 precision.}

\item{pc.u.phi}{pc prior u for bym2 mixing paramete.}

\item{pc.alpha.phi}{pc prior u for bym2 mixing paramete.}
}
\value{
This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
}
\description{
This function calculate smoothed direct estimates at given admin level.
}
\examples{
\dontrun{
geo <- getDHSgeo(country = "Zambia", year = 2018)
data(ZambiaAdm1)
data(ZambiaAdm2)
data(ZambiaPopWomen)
cluster.info <- clusterInfo(geo = geo,
                            poly.adm1 = ZambiaAdm1,
                            poly.adm2 = ZambiaAdm2)

dhsData <- getDHSdata(country = "Zambia",
                                 indicator = "ancvisit4+",
                                 year = 2018)

data <- getDHSindicator(dhsData, indicator = "ancvisit4")
admin.info1 <- adminInfo(poly.adm = ZambiaAdm1,
                        admin = 1,
                        agg.pop =ZambiaPopWomen$admin1_pop,
                        proportion = ZambiaPopWomen$admin1_urban)
cl_res_ad1 <- clusterModel(data=data,
                  cluster.info = cluster.info,
                  admin.info = admin.info1,
                  stratification = FALSE,
                  model = "bym2",
                  admin = 1,
                  aggregation = TRUE,
                  CI = 0.95)
cl_res_ad1$res.admin1

# compare with the DHS direct estimates
dhs_table <- get_api_table(country = "ZM",
                           survey = "ZM2018DHS",
                           indicator = "RH_ANCN_W_N4P",
                           simplify = TRUE)
subset(dhs_table, ByVariableLabel == "Five years preceding the survey")

}

}
\author{
Qianyu Dong
}
