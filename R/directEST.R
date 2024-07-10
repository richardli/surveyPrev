#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param data dataframe that contains the indicator of interests, output of getDHSindicator function
#' @param cluster.info list contains data and wrong.points. data contains admin 1 and admin 2 information and coordinates for each cluster. wrong.points. contains cluster id for cluster without coordinates or admin 1 information. Output of getDHSindicator function
#' @param admin.info list contains data and mat, data contains population and urban/rural proportion at specific admin level and mat is the adjacency matrix, output of adminInfo function
#' @param admin admin level for the model.
#' @param strata use only urban or rural data, only for national level model
#' @param CI Credible interval to be used. Default to 0.95.
#' @param weight the weight used for aggregating result, "population" or "survey"
#' @param aggregation whether or not report aggregation results.
#' @param alt.strata the variable name in the data frame that correspond to the stratification variable. Most of the DHS surveys are stratified by admin 1 area crossed with urban/rural, which is the default stratification variable created by the function (when \code{alt.strata = NULL}). When a different set of strata is used. The stratification variable should be included in the data and \code{alt.strata} should be set to the column name.
#' @param ... Additional arguments passed on to the `smoothSurvey` function
#'
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,

#' @import dplyr
#' @importFrom SUMMER smoothSurvey expit
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#'
#' ##
#' ## Direct estimation of ANC visit 4+ proportion
#' ##
#'
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' data(ZambiaPopWomen)
#' cluster.info<-clusterInfo(geo=geo, poly.adm1=ZambiaAdm1, poly.adm2=ZambiaAdm2,
#' by.adm1 = "NAME_1",by.adm2 = "NAME_2")
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#'
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4+")
#' res_ad1 <- directEST(data = data,
#'                   cluster.info = cluster.info,
#'                   admin = 1,
#'                   aggregation = FALSE)
#' res_ad1
#' # compare with the DHS direct estimates
#' dhs_table <- get_api_table(country = "ZM",
#'                            survey = "ZM2018DHS",
#'                            indicator = "RH_ANCN_W_N4P",
#'                            simplify = TRUE)
#' subset(dhs_table, ByVariableLabel == "Five years preceding the survey")
#'
#' ##
#' ## Changing customized stratification variable
#' ##
#'
#' data_alt <- data
#' # Assuming the stratification is done with only admin1 area
#' # and not stratified by urban and rural
#' # Note that this is not the correct stratification, but we use
#' #  this as an illustration to create user-specified strata variable
#' data_alt$new_strata <- data_alt$v024
#' res_ad1_wrong <- directEST(data = data_alt,
#'                   cluster.info = cluster.info,
#'                   admin = 1,
#'                   aggregation = FALSE,
#'                   alt.strata = "new_strata")
#' res_ad1_wrong
#' }
#'
#' @export

directEST <- function(data, cluster.info, admin, strata="all", CI = 0.95, weight = c("population", "survey")[1], admin.info = NULL, aggregation = FALSE, alt.strata = NULL, ...){
  if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }

 if(!is.null(admin.info)){
   admin.info=admin.info$data
 }

  if(admin==2){
    if(strata != "all"){
      message("Subnational stratum-specific direct estimates are not implemented yet. Only overall estimates are computed")
    }
    #prepare data
    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$admin2.name)), ]
    modt$strata.full <- factor(paste(modt$admin1.name, modt$strata))
    if(!is.null(alt.strata)){
        modt$strata.full <- factor(modt[, alt.strata])
    }
    modt<-  modt[order(modt$admin1.name,modt$admin2.name), ]

    options(survey.adjust.domain.lonely=TRUE)
    options(survey.lonely.psu="adjust")

    smoothSurvey_res<-SUMMER::smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "admin2.name.full",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = CI,
                                   is.unit.level=FALSE,
                                   smooth=FALSE,...)
    admin2_res<-as.data.frame(smoothSurvey_res$HT)
    admin2_res$direct.se<-sqrt(admin2_res$HT.var)

    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'admin2.name.full' # region is the col name in smoothSurvey_res$HT

    colnames(admin2_res)[colnames(admin2_res) == 'HT.est'] <- 'direct.est'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.var'] <- 'direct.var'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.logit.est'] <- 'direct.logit.est'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.logit.var'] <- 'direct.logit.var'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.logit.prec'] <- 'direct.logit.prec'


    admin2_res$direct.lower <- expit(admin2_res$direct.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin2_res$direct.logit.var))
    admin2_res$direct.upper <- expit(admin2_res$direct.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin2_res$direct.logit.var))
    admin2_res$cv<-sqrt(admin2_res$direct.var)/admin2_res$direct.est

    a<-strsplit(admin2_res$admin2.name.full,"_")
    admin2_res$admin2.name<-matrix(unlist(a),ncol =2, byrow =T)[,2]#needed in mapplot()
    admin2_res$admin1.name<-matrix(unlist(a),ncol =2, byrow =T)[,1]


    res.admin2=admin2_res

    ####message for aggregation=T but missing some components and return results without aggregation
    if(aggregation==FALSE){
    }else{
      if((is.null(admin.info)||sum(is.na(admin.info$population))>0)|| is.null(weight=="population")){
        message("Need population information for aggregation")
        aggregation=FALSE
      }

    }


   if(aggregation==FALSE){
     # colnames(res.admin2)[colnames(res.admin2) == 'admin2.name.full'] <- 'admin2.name.full'
     res.admin2=list(res.admin2=res.admin2)
   }else{


        ##aggregation
        # admin2_res<-na.omit(admin2_res)# exclude NA when weighted mean to admin1

     # make direct.logit.est to 36 or -36 for HT=1 or 0.
        for (i in 1:dim(admin2_res)[1]) {

          if(is.na(admin2_res[i,]$direct.logit.est)&& round(admin2_res[i,]$direct.est,digits = 8)==1 ){
            admin2_res[i,]$direct.logit.est=36
          }
          if(is.na(admin2_res[i,]$direct.logit.est)&&admin2_res[i,]$direct.est==0 ){
            admin2_res[i,]$direct.logit.est=-36
          }

          if(is.na(admin2_res[i,]$direct.logit.var)){
            admin2_res[i,]$direct.logit.var=0
          }
}




        dd=data.frame(admin2.name.full=admin2_res$admin2.name.full,value=admin2_res$direct.logit.est,sd=sqrt(admin2_res$direct.logit.var))   #dd$value has <0 bc it's direct.logit.est
        draw.all=  expit(apply(dd[,2:3], 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))

        ##
        ## TODO: Similar to above, using distinct() can create problems. Instead, use both admin1 and admin2 names to join the two dataset.
        ## 1/8/24: joined use admin2.name.full which is admin1_admin2


        ##If J-th admin2 nested within the i-th admin 1, and the k-th region has no data,
        ##the admin 1 estimate is $p_i = \sum_{j\neq k}^J p_{ij} * n_{ij} /\sum_{j\neq k}^J n_{ij} )$.
        ##And the national estimate is p = $p_i *(\sum_{j=1}^J n_{ij} )/ n$ + other admin1 estimates weighted by the admin1 pop fraction)

        ### ### ### ### ### ### ### ### ### ###
        ### admin2 to admin1 for admin2 result
        ### ### ### ### ### ### ### ### ### ###

        ####aggregation for variance
        if(weight=="population"){
          #weight using worldpop
          weight_dt<-left_join(dd, distinct(admin.info), by="admin2.name.full")%>%
            group_by(admin1.name)%>%
            mutate(prop=round(population/sum(population),digits = 4))
        }else{
          #weight using dhs sampling weight (modt$weight
          weight_dt<- modt%>%group_by(admin2.name.full)%>%
            mutate(sumweight2=sum(weight),digits = 4)%>%
            distinct(admin2.name.full,sumweight2,admin1.name,admin2.name)%>%
            group_by(admin1.name)%>%
            mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))%>%
            left_join(dd, by="admin2.name.full")
        }
        weight_dt <- weight_dt[match(admin2_res$admin2.name.full, weight_dt$admin2.name.full), ]



       admin1.list <- sort(unique(weight_dt$admin1.name))
       admin1.samp <- matrix(NA, 10000, length(admin1.list))
       for(i in 1:length(admin1.list)){
          which.admin2 <- which(weight_dt$admin1.name == admin1.list[i])
          admin1.samp[, i] <- apply(draw.all[, which.admin2, drop = FALSE], 1, function(x, w){sum(x * w)}, weight_dt$prop[which.admin2])
       }
       colnames(admin1.samp) <- admin1.list



       ## aggregation for mean
       if(weight=="population"){
         #weight using worldpop
         weight_dt_mean<-left_join(admin2_res[,c("admin2.name.full","direct.est")],distinct(admin.info), by="admin2.name.full")%>%
           group_by(admin1.name)%>%
           mutate(prop=round(population/sum(population),digits = 4))%>%
           mutate(value1=prop*direct.est)
       }else{
         #weight using dhs sampling weight (modt$weight
         weight_dt_mean<- modt%>%group_by(admin2.name.full)%>%
           mutate(sumweight2=sum(weight),digits = 4)%>%
           distinct(admin2.name.full,sumweight2,admin1.name,admin2.name)%>%
           group_by(admin1.name)%>%
           mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))%>%
           left_join(admin2_res[,c("admin2.name.full","direct.est")], by="admin2.name.full")%>%
           mutate(value1=prop*direct.est)

       }



       admin1_agg <- data.frame(admin1.name= colnames(admin1.samp),
                                direct.se =  apply(admin1.samp, 2, sd),
                                direct.lower= apply(admin1.samp, 2,  quantile, probs = c((1 - CI)/2, 1 - (1 - CI)/2))[1,],
                                direct.upper= apply(admin1.samp, 2,  quantile, probs = c((1 - CI)/2, 1 - (1 - CI)/2))[2,]
       )

       admin1_agg <- admin1_agg%>% left_join( aggregate(value1 ~ admin1.name, data = weight_dt_mean, sum), by="admin1.name")%>%
       rename( direct.est = value1)#admin1_agg: admin2toadmin1 result

       admin1_agg <- admin1_agg[, c("admin1.name", "direct.est", "direct.se", "direct.lower", "direct.upper")]


       ### ### ### ### ### ### ### ### ### ###
       ### admin1 to national for admin2 result
       ### ### ### ### ### ### ### ### ### ###

       if(weight=="population"){
           #for variance
           admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin1.name, population=admin.info$population.admin1))
           weight_dt=admin1.distinct$population[match(colnames(admin1.samp), admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
           nation.samp<- admin1.samp%*%weight_dt

           #for mean
           weight_dt_mean<-weight_dt%*%admin1_agg$direct.est

       }else{
         # admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin1.name, population=admin.info$population.admin1))
         # weight_dt=admin1.distinct$population[match(colnames(admin1.samp), admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
         weight_dt<- modt%>%group_by(admin1.name)%>%
           mutate(sumweight2=sum(weight),digits = 4)%>%
           distinct(admin1.name,sumweight2)%>%
           ungroup()%>%
           mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))


         nation.samp<- admin1.samp%*%weight_dt$prop  #for variance
         weight_dt_mean<-weight_dt$prop%*%admin1_agg$direct.est #for mean

       }


       nation_agg <- data.frame(
         # admin0.name="country",
                               direct.est=weight_dt_mean,
                                #meanfromsample =mean(nation.samp),
                               direct.se = sd(nation.samp),
                               direct.var = var(nation.samp),
                               direct.lower=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[1],
                               direct.upper=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[2])


       #cleaning up colnames
       # colnames(res.admin2)[colnames(res.admin2) == 'admin2.name.full'] <- 'admin2.name.full'


       res.admin2<-list(res.admin2=res.admin2,agg.admin1=admin1_agg, agg.natl=nation_agg)

   }
    attr(res.admin2,"class")="directEST"
    attr(res.admin2,"domain.names") <- admin.info$admin2.name.full
    return(res.admin2)

  }else if(admin==1){

    if(strata != "all"){
      message("Subnational stratum-specific direct estimates are not implemented yet. Only overall estimates are computed")
    }

    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$admin1.name)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
     if(!is.null(alt.strata)){
        modt$strata.full <- factor(modt[, alt.strata])
    }
    modt<-  modt[order(modt$admin1.name), ]

    # model
    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt,
    #                             strata=~strata)
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
                 CI = CI,
                 is.unit.level=FALSE,
                 smooth=FALSE)

    admin1_res<-smoothSurvey_res$HT
    admin1_res$direct.se<-sqrt(admin1_res$HT.var)


    colnames(admin1_res)[colnames(admin1_res) == 'region'] <- 'admin1.name'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.est'] <- 'direct.est'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.var'] <- 'direct.var'

    colnames(admin1_res)[colnames(admin1_res) == 'HT.logit.est'] <- 'direct.logit.est'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.logit.var'] <- 'direct.logit.var'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.logit.prec'] <- 'direct.logit.prec'


    admin1_res$direct.lower <- expit(admin1_res$direct.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin1_res$direct.logit.var))
    admin1_res$direct.upper <- expit(admin1_res$direct.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin1_res$direct.logit.var))
    admin1_res$cv<-sqrt(admin1_res$direct.var)/admin1_res$direct.est

    res.admin1=admin1_res


    ####message for aggregation=T but missing some components and return results without aggregation
    if(aggregation==FALSE){
    }else{
      if((is.null(admin.info)||sum(is.na(admin.info$population))>0)& is.null(weight=="population") ){
        message("Need population information for aggregation")
        aggregation=FALSE
      }
    }


    if(aggregation==FALSE){
      res.admin1=list(res.admin1=res.admin1)

    }else{


      ### ### ### ### ### ### ### ### ### ###
      ### admin1 to national for admin1 result
      ### ### ### ### ### ### ### ### ### ###

     # make direct.logit.est to 36 or -36 for HT=1 or 0.

      for (i in 1:dim(admin1_res)[1]) {

        if(is.na(admin1_res[i,]$direct.logit.est)&& round(admin1_res[i,]$direct.est,digits = 8)==1 ){
          admin1_res[i,]$direct.logit.est=36
        }
        if(is.na(admin1_res[i,]$direct.logit.est)&&admin1_res[i,]$direct.est==0 ){
          admin1_res[i,]$direct.logit.est=-36
        }

        if(is.na(admin1_res[i,]$direct.logit.var)){
          admin1_res[i,]$direct.logit.var=0
        }
      }

    dd=data.frame(mean=admin1_res$direct.logit.est,sd=sqrt(admin1_res$direct.logit.var))
    draw.all= expit(apply(dd, 1, FUN = function(x) rnorm(5000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))

   if(weight=="population"){
      #for variance
      admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin1.name, population=admin.info$population))
      weight_dt=admin1.distinct$population[match(admin1_res$admin1.name, admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
      nation.samp<- draw.all%*%weight_dt
      #for mean
      weight_dt_mean<-weight_dt%*%admin1_res$direct.est


    }else{
      weight_dt<- modt%>%group_by(admin1.name)%>%
        mutate(sumweight2=sum(weight),digits = 4)%>%
        distinct(admin1.name,sumweight2)%>%
        ungroup()%>%
        mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))

      nation.samp<- draw.all%*%weight_dt$prop  #for variance
      weight_dt_mean<-weight_dt$prop%*%admin1_res$direct.est #for mean

    }

    nation_agg <- data.frame(
                             # admin1.name= "country",
                             direct.est=weight_dt_mean,
                             # meanFROMsample =mean(nation.samp),
                             direct.se = sd(nation.samp),
                             direct.var = var(nation.samp),
                             direct.lower=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[1],
                             direct.upper=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[2])



    res.admin1=(list(res.admin1=res.admin1, agg.natl=nation_agg))

    }
    attr(res.admin1,"class")="directEST"
    attr(res.admin1,"domain.names") <- admin.info$admin1.name
    return(res.admin1)

}else if(admin==0){


    if (!is.null(alt.strata)) {
      data$admin0.name="country"
      modt<- left_join(data,cluster.info$data,by="cluster")
      # modt<- modt[!(is.na(modt$admin1.name)), ]
      modt$strata.full <- factor(modt[, alt.strata])
      message("Using alt.strata and including all clusters")

    }else{
      data$admin0.name="country"
      modt<- left_join(data,cluster.info$data,by="cluster")
      modt<- modt[!(is.na(modt$admin1.name)), ]
     # modt<- modt[!(is.na(modt$LONGNUM)), ]
      modt$strata.full <- paste(modt$admin1.name, modt$strata)
    }



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
                                   CI = CI,
                                   is.unit.level=FALSE,
                                   smooth=FALSE)
    admin0_res<-smoothSurvey_res$HT
    admin0_res$direct.se<-sqrt(admin0_res$HT.var)
    colnames(admin0_res)[colnames(admin0_res) == 'HT.est'] <- 'direct.est'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.var'] <- 'direct.var'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.logit.est'] <- 'direct.logit.est'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.logit.var'] <- 'direct.logit.var'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.logit.prec'] <- 'direct.logit.prec'


    admin0_res$direct.lower <- expit(admin0_res$direct.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin0_res$direct.logit.var))
    admin0_res$direct.upper <- expit(admin0_res$direct.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin0_res$direct.logit.var))

   # colnames(admin0_res)[1] <- c("admin0.name")
   # return(list(res.admin0=admin0_res[,-1]))
    res.admin0=list(res.admin0=admin0_res[,-1])
    attr(res.admin0,"class")="directEST"
    # attr(res.admin0,"domain.names") <- ""
    return(res.admin0)

  }

}
