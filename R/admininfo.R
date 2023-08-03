#' Get admin information
#'
#' This function get admin information including name, character, population and unban/rural proportion.
#'
#' @param agg.pop  aggregated poplulation from aggPopulation function.
#' @param SpatialPolygons spatial polygons dataframe for admin 1 or admin 2.
#' @param proportion urban/rural proportion from proportion function
#' @param admin admin level
#' @return This function returns the 1. dataframe that contains admin 1 and admin 2 information and coordinates for each cluster and 2. Adjacency matrix
#' \itemize{
#'   \item admin.info
#' }
#' @importFrom dplyr left_join
#' @importFrom spdep poly2nb nb2mat
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export
admininfo <- function(agg.pop, SpatialPolygons, proportion, admin) {

  if (admin==1) {
    colnames(agg.pop)[colnames(agg.pop) == 'DistrictName'] <- 'admin1.name'
    admininfo<-agg.pop

    #Adjacency matrix
    poly.adm1<-SpatialPolygons
    admin.mat <- spdep::poly2nb(SpatialPolygons(poly.adm1@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm1$NAME_1



  }
  else{
    SpatialPolygons<-poly.adm2
    agg.pop<-agg.pop2
    adminname<-data.frame(admin1.name=SpatialPolygons$NAME_1,admin2.name=SpatialPolygons$NAME_2 )
    colnames(agg.pop)[colnames(agg.pop) == 'DistrictName'] <- 'admin2.name'
    admininfo<-dplyr::left_join(adminname,agg.pop,by='admin2.name')


    #Adjacency matrix
    poly.adm2<-SpatialPolygons
    admin.mat <- spdep::poly2nb(SpatialPolygons(poly.adm2@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm2$NAME_2

    }


  return(list(admininfo=admininfo,admin.mat=as.data.frame(admin.mat)))

}

