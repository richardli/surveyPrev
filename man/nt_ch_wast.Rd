% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/indicators.R
\name{nt_ch_wast}
\alias{nt_ch_wast}
\title{CN_NUTS_C_WH2
wasting
Children wasted
NT_CH_NUT.do PR "Wasted child under 5 years"
Wasting rate (Prevalence of wasted (HAZ < -2) children under five (0-59 months))
Percentage of children under age five with a weight-for-height z-score (WHZ) more than two standard deviations below the median WHO growth standards.}
\usage{
nt_ch_wast(Rdata)
}
\arguments{
\item{Rdata}{data.frame from survryPrev::getDHSdata}
}
\value{
A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
}
\description{
CN_NUTS_C_WH2
wasting
Children wasted
NT_CH_NUT.do PR "Wasted child under 5 years"
Wasting rate (Prevalence of wasted (HAZ < -2) children under five (0-59 months))
Percentage of children under age five with a weight-for-height z-score (WHZ) more than two standard deviations below the median WHO growth standards.
}
\examples{
\dontrun{
dhsData <- getDHSdata(country = "Zambia",
                                 indicator = "CN_NUTS_C_WH2",
                                 year = 2018)
data <- getDHSindicator(dhsData, indicator = NULL,
                         FUN = surveyPrev::nt_ch_wast)
}
}
\author{
Qianyu Dong
}
