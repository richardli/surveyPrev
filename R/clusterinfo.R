#' Get cluster information
#'
#' This function add admin 1 and admin2 information to a paticular DHS survey.
#'
#' @param geo  spatial point dataframe
#' @param poly.adm1 spatial polygons dataframe for admin 1
#' @param poly.adm2 spatial polygons dataframe for admin 2.
#'
#' @return This function returns the dataset that contains admin 1 and admin 2 information and coordinates for each cluster.
#' \itemize{
#'   \item cluster.info
#' }
#' @importFrom dplyr select
#' @import sp
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export
clusterInfo <- function(geo, poly.adm1, poly.adm2) {
  # SpatialPointsDataFrame for point shape file for each cluster
  points<-geo
  cluster.info<-points@data%>%dplyr::select(cluster=DHSCLUST, LONGNUM, LATNUM)
  # remove wrong points in the data if any
  wrong.points <- which(points@data$LATNUM < 0.0000001 & points@data$LONGNUM < 0.0000001)
  cluster.info <- cluster.info[!(cluster.info$cluster %in% points@data$DHSCLUST[wrong.points]),]

  points.frame <- as.data.frame(cluster.info[,c("LONGNUM", "LATNUM")]) # retrieve GPS coordinates where data is sampled.
  points.frame <- sp::SpatialPoints(points.frame) # convert the GPS coordinates into "sp" object.

  poly.over.adm1 <- sp::SpatialPolygons(poly.adm1@polygons)
  proj4string(points.frame) <- proj4string(poly.over.adm1)
  poly.over.adm1 <- sp::SpatialPolygons(poly.adm1@polygons)
  admin1.key <- over(points.frame, poly.over.adm1)


  cluster.info$admin1 <- admin1.key
  cluster.info$admin1.char <- paste0("admin1_", admin1.key)
  cluster.info$admin1.name <- as.character(eval(str2lang("poly.adm1@data$NAME_1")))[admin1.key]

  poly.over.adm2 <- SpatialPolygons(poly.adm2@polygons)
  proj4string(points.frame) <- proj4string(poly.over.adm2)
  admin2.key <- over(points.frame, poly.over.adm2)
  miss.frame.adm2 <- points.frame@coords[which(is.na(admin2.key)),]


  cluster.info$admin2 <- admin2.key
  cluster.info$admin2.char <- paste0("admin2_", admin2.key)
  cluster.info$admin2.name <- as.character(eval(str2lang("poly.adm2@data$NAME_2")))[admin2.key]

 # return(cluster.info)
  return(list(cluster.info=cluster.info,wrong.points=wrong.points))

}

