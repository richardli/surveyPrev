#' Get admin information
#'
#' This function get admin information including name, character, population and unban/rural proportion.
#'
#' @param geo spatial polygons dataframe for admin 1 or admin 2.
#' @param admin admin level
#' @param agg.pop  data frame of aggregated poplulation from aggPopulation function. It should have two columns: "DistrictName" and "population".
#' @param proportion data frame of urban/rural proportions. For admin1, is should have two columns: "admin1.name" and "urban". For admin2, it should have three columns: "admin1.name", "admin2.name", and "urban", in order to avoid issues merging datasets with duplicated admin2 names.
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
adminInfo <- function(geo, admin, agg.pop = NULL, proportion = NULL) {



  admin.info <- NULL

  if (admin==1) {
    # colnames(agg.pop)[colnames(agg.pop) == 'DistrictName'] <- 'admin1.name'
    if (is.null(agg.pop)&is.null(proportion)){

      # admininfo$population and admininfo$urban are cols of NA

      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo$population<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)


      }else if(is.null(agg.pop)& !is.null(proportion)){

      # Needed for cluster model when stratification=T and aggregation=F.
      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name","urban")])
      admin.info= as.data.frame(admininfo)
      admininfo$population<-NA
      admin.info= as.data.frame(admininfo)

      }else if(!is.null(agg.pop)&is.null(proportion)){
      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin1.name","population")])
      admin.info= as.data.frame(admininfo)
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)



      }else{

      admininfo <- agg.pop
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "urban")])
      admin.info= as.data.frame(admininfo)

    }



    #Adjacency matrix
    poly.adm1<-geo
    admin.mat <- spdep::poly2nb(sp::SpatialPolygons(poly.adm1@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm1$NAME_1

  }
  else if(admin==2){

    if (is.null(agg.pop)&is.null(proportion)){

      # admininfo$population and admininfo$urban are cols of NA

      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            DistrictName=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo$population<-NA
      # admininfo$population1<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)



    }else if(is.null(agg.pop)& !is.null(proportion)){

      # Needed for cluster model when stratification=T and aggregation=F.
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            DistrictName=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name","urban")])
      admin.info= as.data.frame(admininfo)
      admininfo$population<-NA
      # admininfo$population1<-NA
      admin.info= as.data.frame(admininfo)


    }else if(!is.null(agg.pop)&is.null(proportion)){
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            DistrictName=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("DistrictName","population")])
      admin.info= as.data.frame(admininfo)
      admininfo$urban<-NA
      admininfo<-admininfo%>%
        dplyr::group_by(admin1.name)%>%
        dplyr::mutate(population1=sum(population))
      admin.info= as.data.frame(admininfo)



    }else{

      admininfo <- agg.pop
      admininfo<-admininfo%>%
        dplyr::group_by(admin1.name)%>%
        dplyr::mutate(population1=sum(population))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name", "urban")])
      admin.info= as.data.frame(admininfo)
      admin.info= as.data.frame(admininfo)

    }



    #Adjacency matrix
    poly.adm2<-geo
    admin.mat <- spdep::poly2nb(sp::SpatialPolygons(poly.adm2@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm2$NAME_2

    }


  return(list(admin.info=admin.info,
              admin.mat=as.matrix(as.data.frame(admin.mat))))

}

