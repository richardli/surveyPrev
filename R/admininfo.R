#' Get admin information
#'
#' This function get admin information including name, character, population and unban/rural proportion.
#'
#' @param agg.pop  data frame of aggregated poplulation from aggPopulation function. It should have two columns: "DistrictName" and "population". 
#' @param SpatialPolygons spatial polygons dataframe for admin 1 or admin 2.
#' @param proportion data frame of urban/rural proportions. For admin1, is should have two columns: "admin1.name" and "urban". For admin2, it should have three columns: "admin1.name", "admin2.name", and "urban", in order to avoid issues merging datasets with duplicated admin2 names. 
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
adminInfo <- function(agg.pop, SpatialPolygons, proportion, admin) {

  if (admin==1) {
    colnames(agg.pop)[colnames(agg.pop) == 'DistrictName'] <- 'admin1.name'
    admininfo <- agg.pop
    admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "urban")])

    #Adjacency matrix
    poly.adm1<-SpatialPolygons
    admin.mat <- spdep::poly2nb(SpatialPolygons(poly.adm1@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm1$NAME_1

  }
  else{
    adminname<-data.frame(admin1.name=SpatialPolygons$NAME_1,
                          admin2.name=SpatialPolygons$NAME_2 )
    colnames(agg.pop)[colnames(agg.pop) == 'DistrictName'] <- 'admin2.name'

    admininfo<-dplyr::left_join(adminname, agg.pop, by='admin2.name') %>%
    group_by(admin1.name)%>%
    mutate(population1=sum(population))
    admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name", "urban")])


    #Adjacency matrix
    poly.adm2<-SpatialPolygons
    admin.mat <- spdep::poly2nb(SpatialPolygons(poly.adm2@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm2$NAME_2

    }


  return(list(admin.info= as.data.frame(admininfo),
              admin.mat=as.data.frame(admin.mat)))

}

