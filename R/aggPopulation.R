#' Get population information
#'
#' This function aggregate population to particular admin levels
#'
#' @param tiff  spatial raster of population estimates.
#' @param fact factor to aggregate pixels. Default to be 10, i.e., the population estimates will be saved on 1km by 1km grids if the input is 100m by 100m tiff. Larger values of aggregation factor improves the computation speed, but can introduce more errors when the regions defined by the polygon are small in size.
#' @param SpatialPolygons spatial polygons dataframe.
#' @param varname column name of district name in SpatialPolygons file.
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level
#' @importFrom raster as.data.frame coordinates
#' @importFrom sp coordinates proj4string over
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' library(raster)
#'
#' # Download and find total population in age group 0 to 12 months
#' pre <- "https://data.worldpop.org/GIS/AgeSex_structures/"
#' f <- paste0(pre, "Global_2000_2020/2018/ZMB/zmb_f_0_2018.tif")
#' m <- paste0(pre, "Global_2000_2020/2018/ZMB/zmb_m_0_2018.tif")
#' pop_f_0 <- raster(f)
#' pop_m_0 <- raster(m)
#'
#' pop_raster <- pop_f_0 + pop_m_0
#'
#' # admin1 population
#' agg.pop1 <- aggPopulation(
#'   tiff = pop_raster,
#'   SpatialPolygons = ZambiaAdm1,
#'   varname = "NAME_1")
#'
#' # admin2 population
#' agg.pop2 <- aggPopulation(
#'   tiff = ZambiaPopWomen_raster,
#'   SpatialPolygons = ZambiaAdm2,
#'   varname = "NAME_2")
#' }
#'
#' @export


aggPopulation <- function(tiff, fact = 10, SpatialPolygons, varname){

  if(fact > 1){
    pop_aggre <- aggregate(tiff, fact = fact, sum)
  }else{
    pop_aggre <- tiff
  }
  pop_aggre.df<- raster::as.data.frame(pop_aggre, xy=TRUE)
  colnames(pop_aggre.df)[3] <- "population"

  if(varname=="NAME_2"){
    varname<-"fullname"
    SpatialPolygons$fullname<-paste0(SpatialPolygons$NAME_1,"_",SpatialPolygons$NAME_2)
  }


  loc_df <- data.frame(x = pop_aggre.df$x, y = pop_aggre.df$y)
  sp::coordinates(loc_df) <- ~x+y
  sp::proj4string(loc_df) <- proj4string(SpatialPolygons)
  admin2.name.full<-(over(loc_df, SpatialPolygons)[, varname])

  pop_dt <- cbind(pop_aggre.df, admin2.name.full)
  pop_dt <- pop_dt[!is.na(pop_dt$admin2.name.full), ]
  pop_dt[is.na(pop_dt$population), "population"] <- 0
  tab <- aggregate(population ~ admin2.name.full, data = pop_dt, FUN = sum)


  if(varname=="fullname"){
    tab$admin1.name<-do.call(cbind, strsplit(tab$admin2.name.full, "_"))[1,]
    tab$admin2.name<-do.call(cbind, strsplit(tab$admin2.name.full, "_"))[2,]
    # tab$admin2.name.full<-NULL
  }

  return(tab)
}
