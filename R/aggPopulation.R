#' Get population information
#'
#' This function aggregate population to particular admin levels
#'
#' @param tiff100  spatial point dataframe
#' @param SpatialPolygons spatial polygons dataframe.
#' @param varname column name of district name in SpatialPolygons file.
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level
#'   \item {agg.pop
#' }
#' @importFrom raster as.data.frame coordinates
#' @importFrom sp  proj4string over
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export


aggPopulation <- function(tiff_100m , SpatialPolygons, varname){

  pop_aggre <- aggregate(tiff_100m, fact=10,sum)
  pop_aggre.df<- raster::as.data.frame(pop_aggre, xy=TRUE)
  colnames(pop_aggre.df)[3] <- "population"

  loc_df <- data.frame(x = pop_aggre.df$x, y = pop_aggre.df$y)
  coordinates(loc_df) <- ~x+y
  proj4string(loc_df) <- proj4string(SpatialPolygons)
  DistrictName<-(over(loc_df, SpatialPolygons)[, varname])

  pop_dt <- cbind(pop_aggre.df, DistrictName)
  pop_dt <- pop_dt[!is.na(pop_dt$DistrictName), ]
  pop_dt[is.na(pop_dt$population), "population"] <- 0
  tab <- aggregate(population ~ DistrictName, data = pop_dt, FUN = sum)
  return(tab)
}
