#' Get cluster information
#'
#' This function add admin 1 and admin2 information to a paticular DHS survey.
#'
#' @param geo  spatial point dataframe
#' @param poly.adm1 spatial polygons dataframe for admin 1
#' @param poly.adm2 spatial polygons dataframe for admin 2 or other lower admin level.
#' @param by.adm1 the column name of column for Admin names for admin 1
#' @param by.adm2 the column name of column for Admin names for admin 2 or other lower admin level.
#' @return This function returns the dataset that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @importFrom dplyr select
#' @import sf
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#' }
#' @export
#'
clusterInfo <- function(geo, poly.adm1, poly.adm2, by.adm1 = "NAME_1",by.adm2 = "NAME_2") {
 #  # SpatialPointsDataFrame for point shape file for each cluster
 #  points<-geo
 #  cluster.info<-points@data%>%dplyr::select(cluster=DHSCLUST, LONGNUM, LATNUM)
 #  # remove wrong points in the data if any
 #  wrong.points <- which(points@data$LATNUM < 0.0000001 & points@data$LONGNUM < 0.0000001)
 #  cluster.info <- cluster.info[!(cluster.info$cluster %in% points@data$DHSCLUST[wrong.points]),]
 #
 #  points.frame <- as.data.frame(cluster.info[,c("LONGNUM", "LATNUM")]) # retrieve GPS coordinates where data is sampled.
 #  points.frame <- sp::SpatialPoints(points.frame) # convert the GPS coordinates into "sp" object.
 #
 #  poly.over.adm1 <- sp::SpatialPolygons(poly.adm1@polygons)
 #  proj4string(points.frame) <- proj4string(poly.over.adm1)
 #  poly.over.adm1 <- sp::SpatialPolygons(poly.adm1@polygons)
 #  admin1.key <- over(points.frame, poly.over.adm1)
 #
 #
 #  # cluster.info$admin1 <- admin1.key
 #  # cluster.info$admin1.char <- paste0("admin1_", admin1.key)
 #  cluster.info$admin1.name <- as.character(eval(str2lang("poly.adm1@data$NAME_1")))[admin1.key]
 #
 #  poly.over.adm2 <- SpatialPolygons(poly.adm2@polygons)
 #  proj4string(points.frame) <- proj4string(poly.over.adm2)
 #  admin2.key <- over(points.frame, poly.over.adm2)
 #  miss.frame.adm2 <- points.frame@coords[which(is.na(admin2.key)),]
 #
 #
 #  # cluster.info$admin2 <- admin2.key
 #  # cluster.info$admin2.char <- paste0("admin2_", admin2.key)
 #  cluster.info$admin2.name <- as.character(eval(str2lang("poly.adm2@data$NAME_2")))[admin2.key]
 #  cluster.info$ admin2.name.full <- paste0(cluster.info$admin1.name,"_",cluster.info$admin2.name)
 #
 #
 # # return(cluster.info)
 #  return(list(cluster.info=cluster.info,wrong.points=wrong.points))

  poly.adm1<- sf::st_as_sf(poly.adm1)
  poly.adm2<-sf::st_as_sf(poly.adm2)
  points_sf <- sf::st_as_sf(geo)

  if( !any(sf::st_is_valid(poly.adm1)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}
  if( !any(sf::st_is_valid(poly.adm2)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}

  # Select required columns and filter out wrong points
  cluster.info <- points_sf %>%
    select(cluster = DHSCLUST, LONGNUM, LATNUM) #%>%
  #filter(!(LATNUM < 0.0000001 & LONGNUM < 0.0000001))
#removing wrong.points that has weird LONGNUM LATNUM

  wrong.points <- cluster.info[which(cluster.info$LATNUM < 0.0000001 & cluster.info$LONGNUM < 0.0000001),]$cluster





  admin1.sf <- st_join(cluster.info, poly.adm1) %>%
    sf::st_transform(st_crs(poly.adm1)) # Transform CRS if needed

  cluster.info$admin1.name <- as.data.frame( admin1.sf)[,by.adm1]

  # Spatial join for admin2
  admin2.sf <- st_join(cluster.info, poly.adm2) %>%
    sf::st_transform(st_crs(poly.adm2)) # Transform CRS if needed

  if(dim(admin2.sf)[1] > dim(cluster.info)[1]){
    admin2.sf <- admin2.sf[!duplicated(admin2.sf[,c('DHSCLUST')]),]
    warning('overlapping admin regions exist, the first match is kept')
    }


  # Add admin2 name to cluster.info
   cluster.info$admin2.name <- as.data.frame( admin2.sf)[,by.adm2]
   cluster.info$admin2.name.full <- paste0(cluster.info$admin1.name,"_",cluster.info$admin2.name)

   #removing wrong.points that has no coordinates
   cluster.info <- cluster.info[!(cluster.info$cluster %in% points_sf$DHSCLUST[wrong.points]),]


   #removing wrong.points that has no admin 1 name
   wrong.points <- c(wrong.points,cluster.info[ which(is.na(cluster.info$admin1.name)),]$cluster)
   cluster.info <- cluster.info[!(cluster.info$cluster %in% wrong.points),]

  # return(cluster.info)
   cluster.info<-as.data.frame(cluster.info)
  # remove points outside of shapefile
  # TODO: change those to nearest admin if within DHS jittering range


    return(list(data=cluster.info, wrong.points = wrong.points))


}

