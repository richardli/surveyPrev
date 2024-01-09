#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param data  dataframe that contains the indicator of interests
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admin.info list from the output of adminInfo function
#' @param admin admin level for the model
#' @param strata admin level for the model
#' @param CI Credible interval to be used. Default to 0.95.
#' @param weight the weight used for aggregating result, "population" or "survey"
#' @param aggregation whether or not report aggregation results.
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


directEST <- function(data, cluster.info, admin, strata="all", CI = 0.95, weight = c("population", "survey")[1], admin.info = NULL, aggregation = FALSE){
  if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }

  if(admin==2){
    #prepare data
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
    modt<-  modt[order(modt$admin1.name,modt$admin2.name), ]

    smoothSurvey_res<-SUMMER::smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "DistrictName",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = CI,
                                   is.unit.level=FALSE,
                                   smooth=FALSE)
    admin2_res<-as.data.frame(smoothSurvey_res$HT)
    admin2_res$sd<-sqrt(admin2_res$HT.var)

    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'DistrictName' # region is the col name in smoothSurvey_res$HT
    a<-strsplit(admin2_res$DistrictName,"_")
    admin2_res$admin2.name<-matrix(unlist(a),ncol =2, byrow =T)[,2]#needed in mapplot()
    colnames(admin2_res)[colnames(admin2_res) == 'HT.est'] <- 'value'

     admin2_res$lower <- expit(admin2_res$HT.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin2_res$HT.logit.var))
    admin2_res$upper <- expit(admin2_res$HT.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin2_res$HT.logit.var))
    admin2_res$admin1.name

    res.admin2=admin2_res


   if(aggregation==F){

   }else{



        if(is.null(weight) || is.null(admin.info)){
          stop("Need admin.info and weight for aggregation")
        }

        ##aggregation

        dd=data.frame(DistrictName=admin2_res$DistrictName,value=admin2_res$HT.logit.est,sd=sqrt(admin2_res$HT.logit.var))   #dd$value has <0 bc it's HT.logit.est
        draw.all=  expit(apply(dd[,2:3], 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))

        ##
        ## TODO: Similar to above, using distinct() can create problems. Instead, use both admin1 and admin2 names to join the two dataset.
        ## 1/8/24: joined use DistrictName which is admin1_admin2


        ##If J-th admin2 nested within the i-th admin 1, and the k-th region has no data,
        ##the admin 1 estimate is $p_i = \sum_{j\neq k}^J p_{ij} * n_{ij} /\sum_{j\neq k}^J n_{ij} )$.
        ##And the national estimate is p = $p_i *(\sum_{j=1}^J n_{ij} )/ n$ + other admin1 estimates weighted by the admin1 pop fraction)

        ### ### ### ### ### ### ### ### ### ###
        ### admin2 to admin1 for admin2 result
        ### ### ### ### ### ### ### ### ### ###

        ####aggregation for variance
        if(weight=="population"){
          #weight using worldpop
          weight_dt<-left_join(dd, distinct(admin.info$admin.info), by="DistrictName")%>%
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
                                lower= apply(admin1.samp, 2,  quantile, probs = c((1 - CI)/2, 1 - (1 - CI)/2))[1,],
                                upper= apply(admin1.samp, 2,  quantile, probs = c((1 - CI)/2, 1 - (1 - CI)/2))[2,]
       )

       admin1_agg <- admin1_agg%>% left_join( aggregate(value1 ~ admin1.name, data = weight_dt_mean, sum), by="admin1.name")%>%
       rename( value = value1)#admin1_agg: admin2toadmin1 result
       admin1_agg <- admin1_agg[, c("admin1.name", "value", "sd", "lower", "upper")]


       ### ### ### ### ### ### ### ### ### ###
       ### admin1 to national for admin2 result
       ### ### ### ### ### ### ### ### ### ###

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
                                var = var(nation.samp),
                                lower=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[1],
                                upper=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[2])

       res.admin2<-list(res.admin2=admin2_res,agg.admin1=admin1_agg, agg.natl=nation_agg)

    }
    return(res.admin2)

  }else if(admin==1){

    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
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
    admin1_res$sd<-sqrt(admin1_res$HT.var)

    colnames(admin1_res)[1:2] <- c("admin1.name","value")

    admin1_res<-admin1_res
    admin1_res$lower <- expit(admin1_res$HT.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin1_res$HT.logit.var))
    admin1_res$upper <- expit(admin1_res$HT.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin1_res$HT.logit.var))


    if(aggregation==F){




    }else{

      if(is.null(weight) || is.null(admin.info)==T){
        stop("Need admin.info and weight for aggregation")
      }
      ### ### ### ### ### ### ### ### ### ###
      ### admin1 to national for admin1 result
      ### ### ### ### ### ### ### ### ### ###

    dd=data.frame(mean=admin1_res$HT.logit.est,sd=sqrt(admin1_res$HT.logit.var))
    draw.all= expit(apply(dd, 1, FUN = function(x) rnorm(5000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))

   if(weight=="population"){
      #for variance
      admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin.info$admin1.name, population=admin.info$admin.info$population))
      weight_dt=admin1.distinct$population[match(admin1_res$admin1.name, admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
      nation.samp<- draw.all%*%weight_dt
      #for mean
      weight_dt_mean<-weight_dt%*%admin1_res$value


    }else{
      weight_dt<- modt%>%group_by(admin1.name)%>%
        mutate(sumweight2=sum(weight),digits = 4)%>%
        distinct(admin1.name,sumweight2)%>%
        ungroup()%>%
        mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))

      nation.samp<- draw.all%*%weight_dt$prop  #for variance
      weight_dt_mean<-weight_dt$prop%*%admin1_res$value #for mean

    }



    weight_dt=admin.info$admin.info$population[match(admin1_res$admin1.name, admin.info$admin.info$admin1.name)]/sum(admin.info$admin.info$population)

    nation.samp<-draw.all%*%weight_dt

    nation_agg <- data.frame(value =mean(nation.samp),
                             sd = sd(nation.samp),
                             var = var(nation.samp),
                             lower=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[1],
                             upper=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[2])



    admin1_res=list(res.admin1=admin1_res, agg.natl=nation_agg)

    }
    return(admin1_res)

}else if(admin==0){
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
                                   CI = CI,
                                   is.unit.level=FALSE,
                                   smooth=FALSE)
    admin0_res<-smoothSurvey_res$HT
    admin0_res$sd<-sqrt(admin0_res$HT.var)

    admin0_res$lower <- expit(admin0_res$HT.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin0_res$HT.logit.var))
    admin0_res$upper <- expit(admin0_res$HT.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin0_res$HT.logit.var))


    colnames(admin0_res)[1:2] <- c("admin0.name","value")


   return(res.admin0=admin0_res)

  }

}
