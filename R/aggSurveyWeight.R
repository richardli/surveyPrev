#' Get survey weight by admin levels
#'
#' This function aggregate survey weight to particular admin levels
#'
#'
#' @param data factor to aggregate pixels. Default to be 10, i.e., the population estimates will be saved on 1km by 1km grids if the input is 100m by 100m tiff. Larger values of aggregation factor improves the computation speed, but can introduce more errors when the regions defined by the polygon are small in size.
#' @param admin.info outcome from admininfo
#' @param cluster.info outcome from clusterinfo
#'
#' @return This function returns the dataset that contain admin name and survey weight.
#' @importFrom raster as.data.frame coordinates
#' @importFrom sp coordinates proj4string over
#' @author Qianyu Dong
#'
#' @export


aggSurveyWeight <- function(data, cluster.info, admin,admin2.name.full=NULL){






  if(admin==1)
  {
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
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

    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt<-  modt[order(modt$admin1.name,modt$admin2.name), ]

    weight_dt<- modt%>%group_by(admin2.name.full)%>%
      mutate(surveyWeight=sum(weight))%>%
      distinct(admin2.name.full,surveyWeight,admin1.name,admin2.name)%>%
      group_by(admin1.name)%>%
      mutate(surveyWeight1=sum(surveyWeight))

    if(!is.null(admin2.name.full)& dim(weight_dt)[1]<length(admin2.name.full)){
      missing=admin2.name.full[!admin2.name.full %in% weight_dt$admin2.name.full]

      # strsplit(missing, "_")


      hh= data.frame(admin2.name.full=missing,
         surveyWeight=rep(0,length(missing)),
         admin1.name=sapply(strsplit(missing, "_"), `[`, 1),
         admin2.name=sapply(strsplit(missing, "_"), `[`, 2))

      dd=unique(weight_dt[weight_dt$admin1.name %in% sapply(strsplit(missing, "_"), `[`, 1),c("admin1.name","surveyWeight1")])
      weight_dt[(nrow(weight_dt)+1): (nrow(weight_dt)+length(missing)),]<-  left_join(hh,dd,by="admin1.name")


    }

    weight_dt=as.data.frame(weight_dt)
      return(weight_dt)
  }
}
