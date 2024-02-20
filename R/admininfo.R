#' Get admin information
#'
#' This function get admin information including name, character, population and unban/rural proportion.
#'
#' @param geo spatial polygons dataframe for admin 1 or admin 2. This object can be either an sp::SpatialPolygonsDataFrame object or an sf object.
#' @param admin admin level
#' @param agg.survey  data frame of aggregated poplulation from aggPopulation function. It should have two columns: "admin2.name.full" and "population".
#' @param proportion data frame of urban/rural proportions. For admin1, is should have two columns: "admin1.name" and "urban". For admin2, it should have three columns: "admin1.name", "admin2.name", and "urban", in order to avoid issues merging datasets with duplicated admin2 names.
#' @return This function returns the 1. dataframe that contains admin 1 and admin 2 information and coordinates for each cluster and 2. Adjacency matrix.
#' @importFrom dplyr left_join
#' @importFrom spdep poly2nb nb2mat
#' @author Qianyu Dong
#' @examples
#'
#' # For sp::SpatialPolygonsDataFrame object
#' data(ZambiaAdm1)
#' class(ZambiaAdm1)
#' info <- adminInfo(ZambiaAdm1, admin = 1)
#'
#' # For sf object
#' geo.sf <- sf::st_as_sf(ZambiaAdm1)
#' info <- adminInfo(geo.sf, admin = 1)
#'
#' # To include the population information
#' data(ZambiaPopWomen)
#' info <- adminInfo(geo = ZambiaAdm1,
#'                   admin = 1,
#'                   agg.pop = ZambiaPopWomen$admin1_pop,
#'                   proportion = ZambiaPopWomen$admin1_urban,
#'                   agg.survey = agg.survey1)
#' @export
adminInfo <- function(geo, admin, agg.pop = NULL, agg.survey = NULL, proportion = NULL) {


 if("sf" %in% class(geo)) geo <- sf::as_Spatial(geo)

  admin.info <- NULL

#  propo pop survey
# 1no    no    no
#2 yes    no    no
#3 no    yes    no
#4 no    no    yes
#5 yes   yes    no
#6 yes    no    yes
#7 no    yes    yes  now allow
#8 yes    yes    yes now allow


  if (admin==1) {
    if (is.null(proportion)&is.null(agg.pop)&is.null(agg.survey)){
      #1
      # admininfo$population and admininfo$urban are cols of NA

      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo$population<-NA
      admininfo$surveyweight<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)


      }else if(!is.null(proportion)&is.null(agg.pop)&is.null(agg.survey) ){
      #2proportion
      # Needed for cluster model when stratification=T and aggregation=F.
      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name","urban")])
      admin.info= as.data.frame(admininfo)
      admininfo$population<-NA
      admininfo$surveyweight<-NA
      admin.info= as.data.frame(admininfo)

      }else if(is.null(proportion)&!is.null(agg.pop)&is.null(agg.survey) ){
        #3agg.pop
      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin1.name","population")])
      admin.info= as.data.frame(admininfo)
      admininfo$surveyweight<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)

      }else if(is.null(proportion)&is.null(agg.pop)&!is.null(agg.survey)){
        #4
      admininfo<-data.frame(admin1.name=geo$NAME_1)
      admininfo <- dplyr::left_join(admininfo, agg.survey[, c("admin1.name","surveyWeight")])
      admin.info= as.data.frame(admininfo)
      admininfo$urban<-NA
      admininfo$population<-NA
      admin.info= as.data.frame(admininfo)


      }else if(!is.null(proportion)&!is.null(agg.pop)&is.null(agg.survey) ){
        #5proportion+pop
        admininfo <- agg.pop
        admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "urban")])
        admininfo$surveyweight<-NA
        admin.info= as.data.frame(admininfo)

      }else if(!is.null(proportion)&is.null(agg.pop)&!is.null(agg.survey) ){
        #6proportion+survey
        admininfo <- agg.survey
        admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "urban")])
        admininfo$population<-NA
        admin.info= as.data.frame(admininfo)
      }
    #else if(is.null(proportion)&!is.null(agg.pop)&!is.null(agg.survey) ){
    #     #7 agg.pop+agg.survey
    #     admininfo <- agg.survey
    #     admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin1.name", "population")])
    #     admininfo$urban<-NA
    #     admin.info= as.data.frame(admininfo)
    #
    #
    #   }else{
    #   admininfo <- agg.survey
    #   admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "urban")])
    #   admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin1.name", "population")])
    #   admin.info= as.data.frame(admininfo)
    #
    # }



    #Adjacency matrix
    poly.adm1<-geo
    admin.mat <- spdep::poly2nb(sp::SpatialPolygons(poly.adm1@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  poly.adm1$NAME_1

  }
  else if(admin==2){

    if (is.null(proportion)&is.null(agg.pop)&is.null(agg.survey)){
      #1
      # admininfo$populationadmininfo$surveyWeight and admininfo$urban are cols of NA

      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo$population<-NA
      admininfo$surveyweight<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)



    }else if(!is.null(proportion)&is.null(agg.pop)&is.null(agg.survey)){
      #2 proportion
      # Needed for cluster model when stratification=T and aggregation=F.
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name","urban")])
      admininfo$population<-NA
      admininfo$surveyweight<-NA
      admin.info= as.data.frame(admininfo)


    }else if(is.null(proportion)&!is.null(agg.pop)&is.null(agg.survey)){
      #3 agg.pop
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin2.name.full","population")])
      admininfo<-admininfo%>%
        dplyr::group_by(admin1.name)%>%
        dplyr::mutate(population1=sum(population))
      admin.info= as.data.frame(admininfo)


    }else if(is.null(proportion)&is.null(agg.pop)&!is.null(agg.survey)){
      #4 agg.survey
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, agg.survey[, c("admin2.name.full","surveyWeight","surveyWeight1")])
      admininfo$urban<-NA
      admininfo$population<-NA
      admin.info= as.data.frame(admininfo)


    }else if(!is.null(proportion)&!is.null(agg.pop)&is.null(agg.survey)){
      #5 proportion +agg.pop
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name","urban")])
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin2.name.full","population")])
      admininfo$surveyweight<-NA
      admininfo<-admininfo%>%
        dplyr::group_by(admin1.name)%>%
        dplyr::mutate(population1=sum(population))
      admin.info= as.data.frame(admininfo)


    }else if(!is.null(proportion)&is.null(agg.pop)&!is.null(agg.survey)){
      #6 proportion +agg.survey
      admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
                            admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name","urban")])
      admininfo <- dplyr::left_join(admininfo, agg.survey[, c("admin2.name.full","surveyWeight","surveyWeight1")])
      admininfo$population<-NA
      admin.info= as.data.frame(admininfo)

    }
    # else if(is.null(proportion)&!is.null(agg.pop)&!is.null(agg.survey)){
    #   #7agg.pop +agg.survey
    #   admininfo<-data.frame(admin1.name=geo$NAME_1,admin2.name=geo$NAME_2,
    #                         admin2.name.full=paste0(geo$NAME_1,"_",geo$NAME_2))
    #   admininfo <- dplyr::left_join(admininfo, agg.survey[, c("admin2.name.full","surveyWeight","surveyWeight1")])
    #   admininfo <- dplyr::left_join(admininfo, agg.survey[, c("admin2.name.full","surveyWeight","surveyWeight1")])
    #   admininfo$urban<-NA
    #   admin.info= as.data.frame(admininfo)
    #
    # }else{
    #
    #   admininfo <- agg.pop
    #   admininfo<-admininfo%>%
    #     dplyr::group_by(admin1.name)%>%
    #     dplyr::mutate(population1=sum(population))
    #   admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name", "urban")])
    #   admininfo <- dplyr::left_join(admininfo, proportion[, c("admin2.name.full", "surveyWeight","surveyWeight1")])
    #
    #   admin.info= as.data.frame(admininfo)
    #
    # }



    #Adjacency matrix
    poly.adm2<-geo
    admin.mat <- spdep::poly2nb(sp::SpatialPolygons(poly.adm2@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  paste0(geo$NAME_1,"_",geo$NAME_2)

    }


  return(list(admin.info=admin.info,
              admin.mat=as.matrix(as.data.frame(admin.mat))))

}

