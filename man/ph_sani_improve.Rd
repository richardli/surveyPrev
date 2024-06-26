% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicators.R
\name{ph_sani_improve}
\alias{ph_sani_improve}
\title{WS_TLET_H_IMP
Percentage of households using an improved sanitation facility
PH_SANI.do  PR
ph_sani_improve "Access to improved sanitation" country-specific}
\usage{
ph_sani_improve(Rdata)
}
\arguments{
\item{Rdata}{data.frame from survryPrev::getDHSdata}
}
\value{
A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
}
\description{
WS_TLET_H_IMP
Percentage of households using an improved sanitation facility
PH_SANI.do  PR
ph_sani_improve "Access to improved sanitation" country-specific
}
\examples{
\dontrun{
dhsData <- getDHSdata(country = "Zambia",
                                 indicator = "WS_TLET_H_IMP",
                                 year = 2018)
data <- getDHSindicator(dhsData, indicator = NULL,
                         FUN = surveyPrev::ph_sani_improve)
}
}
\author{
Qianyu Dong
}
