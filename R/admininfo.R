#' Get admin information
#'
#' This function get admin information including name, character, population and unban/rural proportion.
#'
#' @param poly.adm spatial polygons dataframe for Admin levels such as Admin 1 or Admin 2. This object can be either an sp::SpatialPolygonsDataFrame object or an sf object.
#' @param admin desired admin level for the output, can be 1 or 2.
#' @param by.adm the column name of column for Admin names for desired output Admin level, can be such as "NAME_1" or "NAME_2".
#' @param by.adm.upper the column name of column for Admin names for upper level of your desired output Admin level when admin=2, can be "NAME_1" when by.adm="NAME_2".
#' @param agg.pop  data frame of aggregated poplulation from aggPopulation function. It should have two columns: "admin2.name.full" and "population".
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
#' info <- adminInfo(poly.adm=ZambiaAdm1, admin = 1, by.adm="NAME_1")
#' data(ZambiaAdm2)
#' class(ZambiaAdm2)
#' info2 <- adminInfo(poly.adm=ZambiaAdm2, admin = 2,by.adm="NAME_2",by.adm.upper="NAME_1")
#'
#' # For sf object
#' geo.sf <- sf::st_as_sf(ZambiaAdm1)
#' info <- adminInfo(poly.adm=geo.sf, admin = 1,by.adm="NAME_1")
#'
#' # To include the population information
#' data(ZambiaPopWomen)
#' info <- adminInfo(poly.adm = ZambiaAdm1,
#'                   admin = 1,by.adm="NAME_1",
#'                   agg.pop = ZambiaPopWomen$admin1_pop,
#'                   proportion = ZambiaPopWomen$admin1_urban )
#' @export
adminInfo <- function(poly.adm, by.adm, admin, by.adm.upper=NULL, agg.pop = NULL, proportion = NULL) {



  geo <- poly.adm
 if("sf" %in% class(geo)) geo <- sf::as_Spatial(geo)

  admin.info <- NULL

#  propo pop
  #1  no    no
  #2 yes    no
  #3 no    yes
  #5 yes   yes

   if("surveyWeight" %in% colnames(agg.pop)){
     colnames(agg.pop)[colnames(agg.pop) == 'surveyWeight'] <- 'population'
   }

  if (admin==1) {
    if (is.null(proportion)&is.null(agg.pop)){
      #1
      # admininfo$population and admininfo$urban are cols of NA

      admininfo<-data.frame(admin1.name=geo@data[,by.adm])
      admininfo$population<-NA
      admininfo$surveyWeight<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)


      }else if(!is.null(proportion)&is.null(agg.pop) ){
      #2proportion
      # Needed for cluster model when stratification=T and aggregation=F.
      admininfo<-data.frame(admin1.name=geo@data[,by.adm])
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name","urban")])
      admin.info= as.data.frame(admininfo)
      admininfo$population<-NA
      admin.info= as.data.frame(admininfo)

      }else if(is.null(proportion)&!is.null(agg.pop) ){
        #3agg.pop
      admininfo<-data.frame(admin1.name=geo@data[,by.adm])
      admininfo <- dplyr::left_join(admininfo, agg.pop)
      # %>%
                 # dplyr::select(.,admininfo,agg.pop)
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)


      }else if(!is.null(proportion)&!is.null(agg.pop) ){
        #5proportion+pop
        admininfo <- agg.pop
        admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "urban")])
        admin.info= as.data.frame(admininfo)

      }


    #Adjacency matrix
    poly.adm1<-geo
    admin.mat <- spdep::poly2nb(sp::SpatialPolygons(poly.adm1@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  geo@data[,by.adm]

  }
  else if(admin==2){

    if(is.null(by.adm.upper)){
      stop("Upper level admin region membership needs to be specified in 'by.admin.upper'.")
    }



    if (is.null(proportion)&is.null(agg.pop)){
      #1
      # admininfo$populationadmininfo$surveyWeight and admininfo$urban are cols of NA

      admininfo<-data.frame(admin1.name=geo@data[,by.adm.upper],admin2.name=geo@data[,by.adm],
                            admin2.name.full=paste0(geo@data[,by.adm.upper],"_",geo@data[,by.adm]))
      admininfo$population<-NA
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)



    }else if(!is.null(proportion)&is.null(agg.pop)){
      #2 proportion
      # Needed for cluster model when stratification=T and aggregation=F.
      admininfo<-data.frame(admin1.name=geo@data[,by.adm.upper],admin2.name=geo@data[,by.adm],
                            admin2.name.full=paste0(geo@data[,by.adm.upper],"_",geo@data[,by.adm]))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name","urban")])
      admininfo$population<-NA
      admin.info= as.data.frame(admininfo)


    }else if(is.null(proportion)&!is.null(agg.pop)){
      #3 agg.pop
      admininfo<-data.frame(admin1.name=geo@data[,by.adm.upper],admin2.name=geo@data[,by.adm],
                            admin2.name.full=paste0(geo@data[,by.adm.upper],"_",geo@data[,by.adm]))
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin2.name.full","population")])
      admininfo<-admininfo%>%
        dplyr::group_by(admin1.name)%>%
      dplyr::mutate(population.admin1=sum(population))
      admininfo$urban<-NA
      admin.info= as.data.frame(admininfo)



    }else if(!is.null(proportion)&!is.null(agg.pop)){
      #5 proportion +agg.pop
      admininfo<-data.frame(admin1.name=geo@data[,by.adm.upper],admin2.name=geo@data[,by.adm],
                            admin2.name.full=paste0(geo@data[,by.adm.upper],"_",geo@data[,by.adm]))
      admininfo <- dplyr::left_join(admininfo, proportion[, c("admin1.name", "admin2.name","urban")])
      admininfo <- dplyr::left_join(admininfo, agg.pop[, c("admin2.name.full","population")])
      admininfo<-admininfo%>%
        dplyr::group_by(admin1.name)%>%
        dplyr::mutate(population.admin1=sum(population))
      admin.info= as.data.frame(admininfo)

    }




    #Adjacency matrix
    poly.adm2<-geo
    admin.mat <- spdep::poly2nb(sp::SpatialPolygons(poly.adm2@polygons))
    admin.mat <- spdep::nb2mat(admin.mat, zero.policy = TRUE)
    colnames(admin.mat) <- rownames(admin.mat) <-  paste0(geo@data[,by.adm.upper],"_",geo@data[,by.adm])

    }


  return(list(data=admin.info,
              mat=as.matrix(as.data.frame(admin.mat))))

}

