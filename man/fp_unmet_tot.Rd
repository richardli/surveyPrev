% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicators.R
\name{fp_unmet_tot}
\alias{fp_unmet_tot}
\title{FP_NADA_W_UNT
#unmet_family  IRdata
women with an unmet need for family planning for spacing and limiting}
\usage{
fp_unmet_tot(Rdata)
}
\arguments{
\item{Rdata}{data.frame from survryPrev::getDHSdata}
}
\value{
A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
}
\description{
FP_NADA_W_UNT
#unmet_family  IRdata
women with an unmet need for family planning for spacing and limiting
}
\examples{
\dontrun{
dhsData <- getDHSdata(country = "Zambia",
                                 indicator = "FP_NADA_W_UNT",
                                 year = 2018)
data <- getDHSindicator(dhsData, indicator = NULL,
                         FUN = surveyPrev::fp_unmet_tot)
}
}
\author{
Qianyu Dong
}
