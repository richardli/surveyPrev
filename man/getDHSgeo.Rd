% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDHSgeo.R
\name{getDHSgeo}
\alias{getDHSgeo}
\title{Download DHS geo data}
\usage{
getDHSgeo(country, year)
}
\arguments{
\item{country}{Country name.}

\item{year}{Year the survey conducted.}

\item{indicator}{Indicator of interests.}
}
\value{
The function returns a spatial point dataset with coordinates for each cluster based on the chosen survey and year.
\itemize{
  \item geo
}
}
\description{
This function downloads cluster's coordinate data for country and survey.
}
\examples{
\dontrun{
}

}
\author{
Qianyu Dong
}