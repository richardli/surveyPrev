% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intervalPlot.R
\name{intervalPlot}
\alias{intervalPlot}
\title{Get scatter plot for any two model results}
\usage{
intervalPlot(
  admin = 0,
  compare = FALSE,
  model = NULL,
  group = FALSE,
  sort_by = NULL,
  decreasing = FALSE
)
}
\arguments{
\item{admin}{level of plot}

\item{compare}{plot for compare multiple plot or not}

\item{model}{list of model results  using surveyPrev}

\item{group}{plot by group or not}

\item{sort_by}{the name of the model to sort the areas by. Default to be NULL}

\item{decreasing}{whether the regions are sorted in decreasing order. Default to be NULL}
}
\value{
This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level.
}
\description{
This function return scatter plot at admin 1 level for any two model results
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

data <- getDHSindicator(dhsData, indicator = "ancvisit4+")
admin.info2 <- adminInfo(poly.adm = ZambiaAdm2,
                        admin = 2,
                        agg.pop =ZambiaPopWomen$admin2_pop,
                        proportion = ZambiaPopWomen$admin2_urban)
cl_res_ad2_unstrat <- clusterModel(data = data,
                  cluster.info = cluster.info,
                  admin.info = admin.info2,
                  stratification = FALSE,
                  model = "bym2",
                  admin = 2,
                  aggregation = TRUE,
                  CI = 0.95)

head(cl_res_ad2_unstrat$res.admin2)
head(cl_res_ad2_unstrat$agg.admin1)
plots <- intervalPlot(cl_res_ad2_unstrat)
plots[["Central"]]

cl_res_ad2 <- clusterModel(data = data,
                  cluster.info = cluster.info,
                  admin.info = admin.info2,
                  stratification = TRUE,
                  model = "bym2",
                  admin = 2,
                  aggregation = TRUE,
                  CI = 0.95)
head(cl_res_ad2$res.admin2)
head(cl_res_ad2$agg.admin1)
plots <- intervalPlot(cl_res_ad2)
plots[["Central"]]

library(patchwork)
wrap_plots(plots, ncol = 5)

}

}
\author{
Qianyu Dong
}
