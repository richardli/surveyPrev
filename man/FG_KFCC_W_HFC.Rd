% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FG_KFCC_W_HFC.R
\name{FG_KFCC_W_HFC}
\alias{FG_KFCC_W_HFC}
\title{FG_KFCC_W_HFC
IRdata
Percentage of women who have ever heard of female circumcision}
\usage{
FG_KFCC_W_HFC(Rdata)
}
\arguments{
\item{Rdata}{data.frame from survryPrev::getDHSdata}
}
\value{
A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
}
\description{
FG_KFCC_W_HFC
IRdata
Percentage of women who have ever heard of female circumcision
}
\examples{
\dontrun{
dhsData <- getDHSdata(country = "Zambia",
                                 indicator = "FP_CUSA_W_MOD",
                                 year = 2018)
data <- getDHSindicator(dhsData, indicator = NULL,
                         FUN = surveyPrev::fp_cruse_mod)
}
}
\author{
Qianyu Dong
}
