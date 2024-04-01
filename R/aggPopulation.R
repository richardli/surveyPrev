#' Get population information
#'
#' This function aggregate population to particular admin levels
#'
#' @param tiff  spatial raster of population estimates.
#' @param fact factor to aggregate pixels. Default to be 10, i.e., the population estimates will be saved on 1km by 1km grids if the input is 100m by 100m tiff. Larger values of aggregation factor improves the computation speed, but can introduce more errors when the regions defined by the polygon are small in size.
#' @param SpatialPolygons spatial polygons dataframe.
#' @param by.adm the column name of column for Admin names for desired output Admin level, can be such as "NAME_1" or "NAME_2".
#' @param by.adm.upper the column name of column for Admin names for upper level of your desired output Admin level when admin=2, can be "NAME_1" when by.adm="NAME_2".
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
#'   by.adm = "NAME_1")
#'
#' # admin2 population
#' agg.pop2 <- aggPopulation(
#'   tiff = ZambiaPopWomen_raster,
#'   SpatialPolygons = ZambiaAdm2,
#'   by.adm = "NAME_2",
#'   by.adm.upper="NAME_1")
#' }
#'
#' @export


aggPopulation <- function(tiff, fact = 10, SpatialPolygons, by.adm, by.adm.upper=NULL){

  if(fact > 1){
    pop_aggre <- aggregate(tiff, fact = fact, sum)
  }else{
    pop_aggre <- tiff
  }
  pop_aggre.df<- raster::as.data.frame(pop_aggre, xy=TRUE)
  colnames(pop_aggre.df)[3] <- "population"

  if(!is.null(by.adm.upper)){
    # varname<-"fullname"
    SpatialPolygons$admin2.name.full<-paste0(SpatialPolygons@data[,by.adm.upper],"_",SpatialPolygons@data[,by.adm])
      # paste0(SpatialPolygons$NAME_1,"_",SpatialPolygons$NAME_2)
  }


  loc_df <- data.frame(x = pop_aggre.df$x, y = pop_aggre.df$y)
  sp::coordinates(loc_df) <- ~x+y
  sp::proj4string(loc_df) <- proj4string(SpatialPolygons)
  adm<-(over(loc_df, SpatialPolygons)[, by.adm])

  pop_dt <- cbind(pop_aggre.df, adm)
  pop_dt <- pop_dt[!is.na(pop_dt$adm), ]
  pop_dt[is.na(pop_dt$population), "population"] <- 0
  tab <- aggregate(population ~ adm, data = pop_dt, FUN = sum)


  if(!is.null(by.adm.upper)){
    # tab$admin1.name<-do.call(cbind, strsplit(tab$admin2.name.full, "_"))[1,]
    colnames(tab)[colnames(tab) =="adm"] <- 'NAME_2'
    tab =left_join(tab,SpatialPolygons@data[,c( by.adm,by.adm.upper,"admin2.name.full")],by=by.adm)
    colnames(tab)[colnames(tab) ==by.adm] <- 'admin2.name'
    colnames(tab)[colnames(tab) ==by.adm.upper] <- 'admin1.name'

    # tab$admin2.name.full<-NULL
  }else{
    colnames(tab)[colnames(tab) =="adm"] <- 'admin1.name'

  }


  return(tab)
}
