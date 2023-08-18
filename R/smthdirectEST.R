#' Calculate smoothed direct estimates
#'
#' This function calculate smoothed direct estimates at given admin level.
#'
#' @param dat.tem  dataframe that contains the indicator of interests
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admininfo dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
#'   \item { smthdirectEST.result
#' }
#' @import dplyr
#' @importFrom survey svydesign svyby
#' @importFrom SUMMER smoothSurvey
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export


smthdirectEST <- function(dat.tem, cluster.info, admininfo, admin, Amat ){
  if(sum(is.na(dat.tem$value))>0){
    dat.tem <- dat.tem[rowSums(is.na(dat.tem)) == 0, ]
    message("Removing NAs in indicator response")
  }

  if(admin==2){

    #prepare data
    modt<- left_join(dat.tem,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)

    #model
    fit2 <- smoothSurvey(as.data.frame(modt),
                         responseType ="binary",
                         responseVar= "value",
                         regionVar = "admin2.name",
                         clusterVar = "~cluster+householdID",#+householdID same result
                         weightVar = "weight",
                         strataVar = "strata.full",
                         Amat =Amat,
                         CI = 0.95)

    admin2_res <- fit2$smooth
    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'admin2.name'
    colnames(admin2_res)[colnames(admin2_res) == 'mean'] <- 'value'





    #aggregate results
    admin1_agg<- left_join(admin2_res,admininfo,by="admin2.name")%>%
      mutate(prop=population/sum(population))%>%
      group_by(admin1.name) %>%
      summarise(weighted_avg = weighted.mean(value, prop))




    nation_agg<- left_join(admin2_res,admininfo,by="admin2.name")%>%
      mutate(prop=population/sum(population))%>%
      summarise(weighted_avg = weighted.mean(value, prop))%>%
      mutate(weighted_avg = sprintf("%.4f", weighted_avg))

    return(list(admin2.res=admin2_res,agg.admin1=admin1_agg,agg.natl=nation_agg))


    }else if(admin==1){

    #prepare data
    modt<- left_join(dat.tem,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)

    #model
    fit1 <- smoothSurvey(as.data.frame(modt),
                         responseType ="binary",
                         responseVar= "value",
                         regionVar = "admin1.name",
                         clusterVar = "~cluster+householdID",
                         weightVar = "weight",
                         strataVar = "strata.full",
                         Amat =Amat,
                         CI = 0.95)

    admin1_res <- fit1$smooth
    colnames(admin1_res)[colnames(admin1_res) == 'region'] <- 'admin1.name'
    colnames(admin1_res)[colnames(admin1_res) == 'mean'] <- 'value'
    admin1_res$sd<-sqrt(admin1_res$var)



    # aggregate results
    nation_agg<- left_join(admin1_res,admininfo,by="admin1.name")%>%
      mutate(prop=population/sum(population))%>%
      summarise(weighted_avg = weighted.mean(value, prop))%>%
      mutate(weighted_avg = sprintf("%.4f", weighted_avg))

    return(list(admin1.res=admin1_res,agg.natl=nation_agg,fit1))

    }else if(admin==0){

    dat.tem$admin0.name="Zambia"
    modt<- left_join(dat.tem,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)

    # Amat=NULL

    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt)
    #
    # admin0_res <- survey::svyby(formula = ~value, by = ~admin0.name,
    #                       design = design,
    #                       survey::svymean,
    #                       drop.empty.groups = FALSE)


    smoothSurvey_res<-smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "admin0.name",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = 0.95,
                                   is.unit.level=FALSE,
                                   smooth=T)
    admin0_res<-smoothSurvey_res$smooth
    colnames(admin0_res)[1:3] <- c("admin0.name","value","var")
    colnames(admin0_res)[5:6] <- c("quant025","quant975")

    admin0_res$sd<-sqrt(admin0_res$var)

    return(admin0.res=admin0_res)

    }

}
