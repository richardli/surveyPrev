#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param dat.tem  dataframe that contains the indicator of interests
#' @param clusterinfo dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admininfo dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
#'   \item { directEST.result
#' }
#' @import dplyr
#' @importFrom SUMMER smoothSurvey
#' @importFrom survey svydesign svyby
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export


directEST <- function(dat.tem, clusterinfo, admininfo, admin,Amat ){
  if(sum(is.na(dat.tem$value))>0){
    dat.tem <- dat.tem[rowSums(is.na(dat.tem)) == 0, ]
    message("Removing NAs in indicator response")
  }
  if(admin==2){
    #prepare data
    modt<- left_join(dat.tem,clusterinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
    # model
    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt)

    # admin2_res <- survey::svyby(formula = ~value, by = ~admin2.name,
    #                       design = design, survey::svymean, drop.empty.groups = FALSE)
    #

    smoothSurvey_res<-smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "admin2.name",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = 0.95,
                                   is.unit.level=FALSE,
                                   smooth=FALSE)
    admin2_res<-smoothSurvey_res$HT
    admin2_res$se<-sqrt(admin2_res$HT.var)

    # return(admin2_res)
    colnames(admin2_res)[1:2] <- c("admin2.name","value")
    #
    # #aggregate results
    admin1_agg<- left_join(admin2_res,admininfo,by="admin2.name")%>%
      mutate(prop=round(population/sum(population),digits = 4))%>%
      group_by(admin1.name) %>%
      # mutate_all(~replace(., is.na(.), 0)) %>%
      summarise(weighted_avg = weighted.mean(value, prop))
      # na.omit %>%
      # summarise(weighted_avg =(prop/sum(prop))%*%value )
      # mutate(weighted_avg = as.numeric(sprintf("%.5f", weighted_avg)))
      # mutate_at(c('weighted_avg'), as.numeric)
    #
    #
    admin1pop<-admininfo%>%
      group_by(admin1.name) %>%
      summarise(pop=sum(population))

    nation_agg<-left_join(admin1pop,admin1_agg,by="admin1.name")%>%
      mutate(prop=pop/sum(pop))%>%
      summarise(weighted_avg = weighted.mean(weighted_avg, prop))%>%
      mutate(weighted_avg = sprintf("%.5f", weighted_avg))
    # mutate_at(c('weighted_avg'), as.numeric)
    # mutate_at(c('weighted_avg'), as.numeric)

    # nation_agg<- left_join(admininfo,clusterinfo,by="admin2.name")%>%
    #   # mutate_all(~replace(., is.na(.), 0)) %>%
    #   mutate(prop=population/sum(population))%>%
    #   summarise(weighted_avg = weighted.mean(value, prop))

    return(list(admin2_res,admin1_agg,nation_agg))
    }
  else if(admin==1){
    modt<- left_join(dat.tem,clusterinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)

    # model
    clusterVar = "~cluster+householdID"
    design <- survey::svydesign(ids = stats::formula(clusterVar),
                                weights = ~weight , data = modt)

    # admin1_res <- survey::svyby(formula = ~value, by = ~admin1.name,
                                # design = design, survey::svymean, drop.empty.groups = FALSE)
  #  aggregate results
    smoothSurvey_res<-smoothSurvey(as.data.frame(modt),
                 responseType ="binary",
                 responseVar= "value",
                 regionVar = "admin1.name",
                 clusterVar = "~cluster+householdID",
                 weightVar = "weight",
                 strataVar = "strata.full",
                 Amat =Amat,
                 CI = 0.95,
                 is.unit.level=FALSE,
                 smooth=FALSE)
    admin1_res<-smoothSurvey_res$HT
    admin1_res$se<-sqrt(admin1_res$HT.var)

    colnames(admin1_res)[1:2] <- c("admin1.name","value")


    nation_agg<- left_join(admininfo,admin1_res,by="admin1.name")%>%
      mutate(prop=population/sum(population))%>%
      summarise(weighted_avg = weighted.mean(value, prop))%>%
      mutate(weighted_avg = sprintf("%.5f", weighted_avg))%>%
      mutate_at(c('weighted_avg'), as.numeric)

    return(list(admin1_res,nation_agg))
  }
  else if(admin==0){
    dat.tem$admin0.name="Zambia"
    modt<- left_join(dat.tem,clusterinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin0.name, modt$strata)

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
                                   smooth=FALSE)
    admin0_res<-smoothSurvey_res$HT
    admin0_res$se<-sqrt(admin0_res$HT.var)

    CI <- 0.95
    admin0_res$quant025 <- expit(admin0_res$HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(admin0_res$HT.logit.var))[1]
    admin0_res$quant975 <- expit(admin0_res$HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(admin0_res$HT.logit.var))[2]

     # lims <- expit(HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(HT.logit.var))

    colnames(admin0_res)[1:2] <- c("admin0.name","value")


   return(admin0_res)

  }

}
