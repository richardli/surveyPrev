#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param data  dataframe that contains the indicator of interests
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


directEST <- function(data, clusterinfo, admininfo, admin,Amat,strata){
  if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }
  if(admin==2){
    #prepare data
    modt<- left_join(data,clusterinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
    # model
    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt)

    # admin2_res <- survey::svyby(formula = ~value, by = ~admin2.name,
    #                       design = design, survey::svymean, drop.empty.groups = FALSE)
    #

    smoothSurvey_res<-SUMMER::smoothSurvey(as.data.frame(modt),
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
    admin2_res<-as.data.frame(smoothSurvey_res$HT)
    admin2_res$sd<-sqrt(admin2_res$HT.var)

    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'admin2.name'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.est'] <- 'value'



    dd=data.frame(admin2.name=admin2_res$admin2.name,value=admin2_res$HT.logit.est,sd=sqrt(admin2_res$HT.logit.var))
    draw.all=  apply(dd[,2:3], 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2])) # sqrt(colVars(draw.all))

    weight<-left_join(dd,distinct(admininfo), by="admin2.name")%>%
    group_by(admin1.name)%>%
    mutate(prop=round(population/sum(population),digits = 4))


   logit.post.all <-  data.table::data.table(t(t(draw.all)*weight$prop))#nrow=lenth(weight)
   colnames(logit.post.all) <- weight$admin1.name
   subgroups<-split.default(logit.post.all, names(logit.post.all))

   sums_list <- lapply(subgroups, function(subgroup) {
     rowSums(subgroup)
   })
   logit.admin1.samp <- do.call(cbind, sums_list)
   admin1.samp<- expit(logit.admin1.samp)


   admin1_agg <- data.frame(admin1.name= colnames(admin1.samp),
                            value = colMeans(admin1.samp[]),
                            sd =  apply(admin1.samp, 2, sd),
                            quant025= apply(admin1.samp, 2,  quantile, probs = c(0.025,0.975))[1,],
                            quant975= apply(admin1.samp, 2,  quantile, probs = c(0.025,0.975))[2,]
   )

   admin1.distinct=distinct(data.frame(admin1.name=admininfo$admin1.name, population=admininfo$population1))
   weight=admin1.distinct$population/sum(admin1.distinct$population)

   logit.nation.samp<-logit.admin1.samp%*%weight

   nation.samp<-expit(logit.nation.samp)
   nation_agg <- data.frame(value = mean(nation.samp),
                            sd = sd(nation.samp),
                            quant025=quantile(nation.samp, probs = c(0.025,0.975))[1],
                            quant975=quantile(nation.samp, probs = c(0.025,0.975))[2])


    return(list(res.admin2=admin2_res,agg.admin1=admin1_agg,agg.natl=nation_agg))
    }
  else if(admin==1){
    modt<- left_join(data,clusterinfo$cluster.info,by="cluster")
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
                 Amat =NULL,
                 CI = 0.95,
                 is.unit.level=FALSE,
                 smooth=FALSE)
    admin1_res<-smoothSurvey_res$HT
    admin1_res$sd<-sqrt(admin1_res$HT.var)

    colnames(admin1_res)[1:2] <- c("admin1.name","value")

    dd=data.frame(mean=admin1_res$HT.logit.est,sd=sqrt(admin1_res$HT.logit.var))
    draw.all= apply(dd, 1, FUN = function(x) rnorm(5000, mean = x[1], sd = x[2])) # sqrt(colVars(draw.all))
    weight=admininfo$population/sum(admininfo$population)

    logit.nation.samp<-draw.all%*%weight

    #HT.logit.est=mean(logit.nation.samp)
    #HT.logit.var=var(logit.nation.samp)
    #CI <- 0.95
    #lims <- expit(HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(HT.logit.var))

    nation.samp<-expit(logit.nation.samp)
    nation_agg <- data.frame(value = mean(nation.samp),
                             sd = sd(nation.samp),
                             quant025=quantile(nation.samp, probs = c(0.025,0.975))[1],
                             quant975=quantile(nation.samp, probs = c(0.025,0.975))[2])




    # nation_agg<- left_join(admininfo,admin1_res,by="admin1.name")%>%
    #   mutate(prop=population/sum(population))%>%
    #   summarise(weighted_avg = weighted.mean(value, prop))%>%
    #   mutate(weighted_avg = sprintf("%.5f", weighted_avg))%>%
    #   mutate_at(c('weighted_avg'), as.numeric)

    return(list(res.admin1=admin1_res,agg.natl=nation_agg))
  }
  else if(admin==0){
    data$admin0.name="Zambia"
    modt<- left_join(data,clusterinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)


    if(strata=="all"){
    }else if(strata=="urban"){
      modt<-modt%>% filter(., strata == "urban")
    }else if(strata=="rural"){
      modt<-modt%>% filter(., strata == "rural")
    }



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
    admin0_res$sd<-sqrt(admin0_res$HT.var)

    CI <- 0.95
    admin0_res$quant025 <- expit(admin0_res$HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(admin0_res$HT.logit.var))[1]
    admin0_res$quant975 <- expit(admin0_res$HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(admin0_res$HT.logit.var))[2]

     # lims <- expit(HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(HT.logit.var))

    colnames(admin0_res)[1:2] <- c("admin0.name","value")


   return(res.admin0=admin0_res)

  }

}
