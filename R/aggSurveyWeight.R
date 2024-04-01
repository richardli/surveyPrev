#' Get survey weight by admin levels
#'
#' This function aggregate survey weight to particular admin levels
#'
#'
#' @param data dataframe that contains the indicator of interests, output of getDHSindicator function
#' @param cluster.info list that contains admin 1 and admin 2 information and coordinates for each cluster, output of clusterinfo function
#' @param admin desired admin level for aggregation
#' @param poly.adm spatial polygons dataframe
#' @param by.adm the column name of column for Admin names for desired output Admin level, can be such as "NAME_1" or "NAME_2".
#' @param by.adm.upper the column name of column for Admin names for upper level of your desired output Admin level when admin=2, can be "NAME_1" when by.adm="NAME_2".
#'
#'
#' @return This function returns the dataset that contain admin name and survey weight.
#' @importFrom raster as.data.frame coordinates
#' @importFrom sp coordinates proj4string over
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#'
#' # admin1 population
#'
#' year <- 2018
#' country <- "Zambia"
#' indicator="nmr"
#'
#' geo <- getDHSgeo(country = country, year = year)
#' dhsData <- getDHSdata(country = country, indicator=indicator, year = year)
#' data<- getDHSindicator(dhsData, indicator = indicator)
#'
#' poly.adm1=ZambiaAdm1
#' poly.adm2=ZambiaAdm2
#'
#' cluster.info<-clusterInfo(geo=geo, poly.adm1=poly.adm1, poly.adm2=poly.adm2,by.adm1 = "NAME_1",by.adm2 = "NAME_2")
#'
#'
#' agg.survey1<-aggSurveyWeight(data=data,cluster.info=cluster.info,admin=1)
#' agg.survey2<-aggSurveyWeight(data=data,cluster.info=cluster.info,admin=2,
#'                              poly.adm = poly.adm2,  by.adm="NAME_2" ,by.adm.upper ="NAME_1")
#' }
#'
#' @export


aggSurveyWeight <- function(data, cluster.info, admin, poly.adm=NULL, by.adm=NULL ,by.adm.upper =NULL){


  #make admin2.name.full from poly.adm2
  if(!is.null(poly.adm)){
    admin2.name.full=paste0(poly.adm@data[,by.adm.upper],"_",poly.adm@data[,by.adm])
  }
  if(admin==1)
  {
    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    # modt<-  modt[order(modt$admin1.name,modt$admin2.name), ]
    #

  weight_dt<- modt%>%group_by(admin1.name)%>%
    mutate(surveyWeight=sum(weight),digits = 4)%>%
    distinct(admin1.name,surveyWeight)%>%
    ungroup()
  weight_dt=as.data.frame(weight_dt)
  return(weight_dt)
  }else{

    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt<-  modt[order(modt$admin1.name,modt$admin2.name), ]

    weight_dt<- modt%>%group_by(admin2.name.full)%>%
      mutate(surveyWeight=sum(weight))%>%
      distinct(admin2.name.full,surveyWeight,admin1.name,admin2.name)%>%
      group_by(admin1.name)
    # %>%
    #   mutate(surveyWeight.admin1=sum(surveyWeight))

    if(!is.null(admin2.name.full)& dim(weight_dt)[1]<length(admin2.name.full)){
      missing=admin2.name.full[!admin2.name.full %in% weight_dt$admin2.name.full]

      # strsplit(missing, "_")


      hh= data.frame(admin2.name.full=missing,
         surveyWeight=rep(0,length(missing)),
         admin1.name=sapply(strsplit(missing, "_"), `[`, 1),
         admin2.name=sapply(strsplit(missing, "_"), `[`, 2))

      dd=unique(weight_dt[weight_dt$admin1.name %in% sapply(strsplit(missing, "_"), `[`, 1),c("admin1.name")])
      weight_dt[(nrow(weight_dt)+1): (nrow(weight_dt)+length(missing)),]<-  left_join(hh,dd,by="admin1.name")


    }

    weight_dt=as.data.frame(weight_dt)
      return(weight_dt)
  }
}
