#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param data  dataframe that contains the indicator of interests
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admin.info list from the output of adminInfo function
#' @param admin admin level for the model
#' @param strata admin level for the model
#' @param weight the weight used for aggregating result, "population" or "SamplingWeight"
#'
#'
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
#'   \item { directEST.result
#' }
#' @import dplyr
#' @importFrom SUMMER smoothSurvey expit
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export




directEST <- function(data, cluster.info, admin.info, admin, strata, weight ){
  if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }
  if(admin==2){
    #prepare data
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)

    # model
    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt)

    # admin2_res <- survey::svyby(formula = ~value, by = ~admin2.name,
    #                       design = design, survey::svymean, drop.empty.groups = FALSE)
    #

    ##
    ## TODO: this does not consider potential duplication of admin2.name when specifying regionVar (which needs to be unique identifier). This can be fixed by using unique identifiers of cluster.info and map back to both admin1 and admin2 names.
    ##



    smoothSurvey_res<-SUMMER::smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "DistrictName",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = 0.95,
                                   is.unit.level=FALSE,
                                   smooth=FALSE)
    admin2_res<-as.data.frame(smoothSurvey_res$HT)
    admin2_res$sd<-sqrt(admin2_res$HT.var)

    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'DistrictName'
    a<-strsplit(admin2_res$DistrictName,"_")
    # admin2_res$admin1.name<-matrix(unlist(a),ncol =2, byrow =T)[,1]
    admin2_res$admin2.name<-matrix(unlist(a),ncol =2, byrow =T)[,2]


    colnames(admin2_res)[colnames(admin2_res) == 'HT.est'] <- 'value'


    dd=data.frame(DistrictName=admin2_res$DistrictName,value=admin2_res$HT.logit.est,sd=sqrt(admin2_res$HT.logit.var))   #dd$value has <0 bc it's HT.logit.est
    draw.all=  expit(apply(dd[,2:3], 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))

    ##
    ## TODO: Similar to above, using distinct() can create problems. Instead, use both admin1 and admin2 names to join the two dataset.
    ##




    ##aggregation
    ##If J-th admin2 nested within the i-th admin 1, and the k-th region has no data,
    ##the admin 1 estimate is $p_i = \sum_{j\neq k}^J p_{ij} * n_{ij} /\sum_{j\neq k}^J n_{ij} )$.
    ##And the national estimate is p = $p_i *(\sum_{j=1}^J n_{ij} )/ n$ + other admin1 estimates weighted by the admin1 pop fraction)


    ####aggregation for variance
    if(weight=="population"){
      #weight using worldpop
      weight_dt<-left_join(dd,distinct(admin.info$admin.info), by="DistrictName")%>%
        group_by(admin1.name)%>%
        mutate(prop=round(population/sum(population),digits = 4))
    }else{
      #weight using dhs sampling weight (modt$weight
      weight_dt<- modt%>%group_by(DistrictName)%>%
        mutate(sumweight2=sum(weight),digits = 4)%>%
        distinct(DistrictName,sumweight2,admin1.name,admin2.name)%>%
        group_by(admin1.name)%>%
        mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))%>%
        left_join(dd, by="DistrictName")
    }
    weight_dt <- weight_dt[match(admin2_res$DistrictName, weight_dt$DistrictName), ]


    ##
    ## here making weight the same order as draw.all
    ##
    ## TODO: Similar to above, the match() does not work when admin2 has duplicated names, need to rewrite this when above is fixed.
    ##




   admin1.list <- unique(weight_dt$admin1.name)
   admin1.samp <- matrix(NA, 10000, length(admin1.list))
   for(i in 1:length(admin1.list)){
      which.admin2 <- which(weight_dt$admin1.name == admin1.list[i])
      admin1.samp[, i] <- apply(draw.all[, which.admin2, drop = FALSE], 1, function(x, w){sum(x * w)}, weight_dt$prop[which.admin2])
   }
   colnames(admin1.samp) <- admin1.list



   ####aggregation for mean

   if(weight=="population"){
     #weight using worldpop
     weight_dt_mean<-left_join(admin2_res,distinct(admin.info$admin.info), by="DistrictName")%>%
       group_by(admin1.name)%>%
       mutate(prop=round(population/sum(population),digits = 4))%>%
       mutate(value1=prop*value)
   }else{
     #weight using dhs sampling weight (modt$weight
     weight_dt_mean<- modt%>%group_by(DistrictName)%>%
       mutate(sumweight2=sum(weight),digits = 4)%>%
       distinct(DistrictName,sumweight2,admin1.name,admin2.name)%>%
       group_by(admin1.name)%>%
       mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))%>%
       left_join(admin2_res, by="DistrictName")%>%
       mutate(value1=prop*value)

   }



   admin1_agg <- data.frame(admin1.name= colnames(admin1.samp),
                            sd =  apply(admin1.samp, 2, sd),
                            quant025= apply(admin1.samp, 2,  quantile, probs = c(0.025,0.975))[1,],
                            quant975= apply(admin1.samp, 2,  quantile, probs = c(0.025,0.975))[2,]
   )

   admin1_agg<- admin1_agg%>% left_join( aggregate(value1 ~ admin1.name, data = weight_dt_mean, sum), by="admin1.name")%>%
   rename( value = value1)#admin1_agg: admin2toadmin1 result



   if(weight=="population"){
  #for variance
   admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin.info$admin1.name, population=admin.info$admin.info$population1))
   weight_dt=admin1.distinct$population[match(colnames(admin1.samp), admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
   nation.samp<- admin1.samp%*%weight_dt


   #for mean
   weight_dt_mean<-weight_dt%*%admin1_agg$value



   }else{
     admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin.info$admin1.name, population=admin.info$admin.info$population1))
     # weight_dt=admin1.distinct$population[match(colnames(admin1.samp), admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
     weight_dt<- modt%>%group_by(admin1.name)%>%
       mutate(sumweight2=sum(weight),digits = 4)%>%
       distinct(admin1.name,sumweight2)%>%
       ungroup()%>%
       mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))


     nation.samp<- admin1.samp%*%weight_dt$prop  #for variance
     weight_dt_mean<-weight_dt$prop%*%admin1_agg$value #for mean

   }




   nation_agg <- data.frame(value =weight_dt_mean,
                            sd = sd(nation.samp),
                            quant025=quantile(nation.samp, probs = c(0.025,0.975))[1],
                            quant975=quantile(nation.samp, probs = c(0.025,0.975))[2])


    return(list(res.admin2=admin2_res,agg.admin1=admin1_agg,agg.natl=nation_agg))
    }
  else if(admin==1){
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)

    # model
    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt)

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
    draw.all= expit(apply(dd, 1, FUN = function(x) rnorm(5000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))
    weight_dt=admin.info$admin.info$population[match(admin1_res$admin1.name, admin.info$admin.info$admin1.name)]/sum(admin.info$admin.info$population)

    nation.samp<-draw.all%*%weight_dt

    #HT.logit.est=mean(logit.nation.samp)
    #HT.logit.var=var(logit.nation.samp)
    #CI <- 0.95
    #lims <- expit(HT.logit.est + stats::qnorm(c((1 - CI) / 2, 1 - (1 - CI) / 2)) * sqrt(HT.logit.var))

    nation_agg <- data.frame(value = mean(nation.samp),
                             sd = sd(nation.samp),
                             quant025=quantile(nation.samp, probs = c(0.025,0.975))[1],
                             quant975=quantile(nation.samp, probs = c(0.025,0.975))[2])




    # nation_agg<- left_join(admin.info,admin1_res,by="admin1.name")%>%
    #   mutate(prop=population/sum(population))%>%
    #   summarise(weighted_avg = weighted.mean(value, prop))%>%
    #   mutate(weighted_avg = sprintf("%.5f", weighted_avg))%>%
    #   mutate_at(c('weighted_avg'), as.numeric)

    return(list(res.admin1=admin1_res, agg.natl=nation_agg))
  }
  else if(admin==0){
    data$admin0.name="country"
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
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
