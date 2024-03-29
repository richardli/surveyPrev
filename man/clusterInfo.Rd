% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusterinfo.R
\name{clusterInfo}
\alias{clusterInfo}
\title{Get cluster information}
\usage{
clusterInfo(geo, poly.adm1, poly.adm2)
}
\arguments{
\item{geo}{spatial point dataframe}

\item{poly.adm1}{spatial polygons dataframe for admin 1}

\item{poly.adm2}{spatial polygons dataframe for admin 2.}
}
\value{
This function returns the dataset that contains admin 1 and admin 2 information and coordinates for each cluster.
}
\description{
This function add admin 1 and admin2 information to a paticular DHS survey.
}
\examples{
\dontrun{
geo <- getDHSgeo(country = "Zambia", year = 2018)
data(ZambiaAdm1)
data(ZambiaAdm2)
cluster.info <- clusterInfo(geo = geo,
                            poly.adm1 = ZambiaAdm1,
                            poly.adm2 = ZambiaAdm2)
}
}
\author{
Qianyu Dong
}
