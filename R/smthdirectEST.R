#' Calculate smoothed direct estimates
#'
#' This function calculate smoothed direct estimates at given admin level.
#'
#' @param data dataframe that contains the indicator of interests, output of getDHSindicator function
#' @param cluster.info list contains data and wrong.points. data contains admin 1 and admin 2 information and coordinates for each cluster. wrong.points. contains cluster id for cluster without coordinates or admin 1 information. Output of getDHSindicator function
#' @param admin.info list contains data and mat, data contains population and urban/rural proportion at specific admin level and mat is the adjacency matrix, output of adminInfo function
#' @param admin admin level for the model
#' @param X dataframe that contains areal covariates, the first column should be the same admin name as in admin.info$data.
#' @param CI Credible interval to be used. Default to 0.95.
#' @param model  smoothing model used in the random effect. Options are independent ("iid") or spatial ("bym2").
#' @param aggregation whether or not report aggregation results.
#' @param alt.strata the variable name in the data frame that correspond to the stratification variable. Most of the DHS surveys are stratified by admin 1 area crossed with urban/rural, which is the default stratification variable created by the function (when \code{alt.strata = NULL}). When a different set of strata is used. The stratification variable should be included in the data and \code{alt.strata} should be set to the column name.

#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,

#' @import dplyr
#' @importFrom survey svydesign svyby
#' @importFrom SUMMER smoothSurvey
#' @importFrom stats weighted.mean var
#' @importFrom data.table data.table
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' data(ZambiaPopWomen)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#'
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#'
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4+")
#' admin.info1 <- adminInfo(poly.adm = ZambiaAdm1,
#'                         admin = 1,
#'                         agg.pop =ZambiaPopWomen$admin1_pop,
#'                         proportion = ZambiaPopWomen$admin1_urban)
#' smth_res_ad1 <- fhModel(data,
#'                        cluster.info = cluster.info,
#'                        admin.info = admin.info1,
#'                        admin = 1,
#'                        model = "bym2",
#'                        aggregation = F)
#' smth_res_ad1
#' }
#'
#' @export

fhModel <- function(data, cluster.info, admin.info = NULL, X= NULL, admin, CI = 0.95,  model = c("bym2", "iid"), aggregation = FALSE, alt.strata = NULL){

  if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }

  if(is.null(admin.info) && model != "iid"){
    message("No admin.info supplied. Using IID random effects.")
    model <- "iid"
  }
  if(model == "bym2"){
    Amat <- admin.info$mat

  }else{
    Amat <- NULL
  }
  admin.info <- admin.info$data

  if(admin==2){

    #prepare data
    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- factor(paste(modt$admin1.name, modt$strata))
    if(!is.null(alt.strata)){
        modt$strata.full <- factor(modt[, alt.strata])
    }

    # modt1<- left_join(admin.info2$admin.info,modt,by="admin2.name.full")
    # modt2<- modt1%>%
    #   filter(is.na( value)) %>%
    #   mutate(cluster=c(546,547,548),householdID=c(1,1,1),
    #          weight=c(1892890,1892890,1892890),
    #          strata=c("urban","urban","urban"),
    #          strata.full=c("Central urban","Lusaka urban","Lusaka urban"))
    # modt2$value[is.na( modt2$value)] <- 0
    # modt1 <- modt1 %>%
    #   filter(!is.na(value))
    # modt<-rbind(modt2,modt1,modt1)
    # vector1<-admin.info2$admin.info$admin2.name
    # vector2<-unique(modt$admin2.name)
    # missing_admin2 <- vector1[!(vector1 %in% vector2)]



    #model
    fit2 <- smoothSurvey(as.data.frame(modt),
                         responseType ="binary",
                         responseVar= "value",
                         regionVar = "admin2.name.full",
                         region.list = unique(admin.info$admin2.name.full),
                         clusterVar = "~cluster+householdID",#+householdID same result
                         weightVar = "weight",
                         strataVar = "strata.full",
                         Amat =Amat,
                         CI = CI,
                         smooth=T,
                         save.draws = TRUE,
                         X=X)


    admin2_res <- fit2$smooth
    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'admin2.name.full'
    admin2_res$sd<-sqrt(admin2_res$var)

    admin2_res$cv=admin2_res$sd/admin2_res$mean
    ####message for aggregation=T but missing some components and return results without aggregation



      if(is.null(admin.info)||sum(is.na(admin.info$population))>0){
        message("Need population or survey weight information for aggregation")
        aggregation=F
      }



    if(aggregation==F){
      admin2.res=admin2_res
      # colnames(admin2.res)[colnames(admin2.res) == 'admin2.name.full'] <- 'admin2.name.full'
      draw.all=expit((fit2$draws.est[,-c(1,2)]))

      admin2.res=list(res.admin2=admin2.res,  model = fit2, admin2_post=draw.all)

    }else{



      #aggregate results

      # draw.all=expit(t(fit2$draws.est[,-c(1,2)]))
      draw.all=expit((fit2$draws.est[,-c(1,2)]))


      weight=admin.info$population/admin.info$population.admin1

      post.all <-  data.table(t(weight*draw.all))
      # post.all <-  (data.table(weight*draw.all))

      colnames(post.all) <- admin.info$admin1.name

      subgroups<-split.default(post.all, names(post.all))

      sums_list <- lapply(subgroups, function(subgroup) {
        rowSums(subgroup)
      })
      admin1.samp <- do.call(cbind, sums_list)


      agg.admin1 <- data.frame(mean = colMeans(admin1.samp),
                               # median = apply(admin1.samp, 2, median),
                               sd =  apply(admin1.samp, 2, sd),
                               var =  apply(admin1.samp, 2, var),
                               lower= apply(admin1.samp, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[1,],
                               upper= apply(admin1.samp, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[2,]
      )
      agg.admin1$admin1.name=rownames(agg.admin1)

      #agg national
      unique( admin.info$population.admin1)/sum(unique( admin.info$population.admin1))

      post.all <- admin1.samp%*% unique( admin.info$population.admin1)/sum(unique( admin.info$population.admin1))
      agg.natl <- data.frame(mean = mean(post.all),
                             # median = median(post.all),
                             sd = sd(post.all),
                             var = var(post.all),
                             lower=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[1],
                             upper=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[2])


      # colnames(admin2_res)[colnames(admin2_res) == 'admin2.name.full'] <- 'admin2.name.full'
      admin2.res=list(res.admin2=admin2_res,agg.admin1=agg.admin1,agg.natl=agg.natl, model = fit2,
                      admin2_post=draw.all,admin1_post=admin1.samp,nation_post=post.all)
    }

    return(admin2.res)


    }else if(admin==1){

    #prepare data
    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
    if(!is.null(alt.strata)){
        modt$strata.full <- factor(modt[, alt.strata])
    }
    #model
    fit1 <- smoothSurvey(as.data.frame(modt),
                         responseType ="binary",
                         responseVar= "value",
                         regionVar = "admin1.name",
                         clusterVar = "~cluster+householdID",
                         region.list = unique(admin.info$admin1.name),
                         weightVar = "weight",
                         strataVar = "strata.full",
                         Amat =Amat,
                         CI = CI,
                         save.draws = TRUE,
                         X=X)

    admin1_res <- fit1$smooth
    colnames(admin1_res)[colnames(admin1_res) == 'region'] <- 'admin1.name'
    admin1_res$sd<-sqrt(admin1_res$var)
    admin1_res$cv=admin1_res$sd/admin1_res$mean


    ####message for aggregation=T but missing some components and return results without aggregation
    if(aggregation==F){
    }else{

      if(!is.null(admin.info$surveyWeight)&sum(is.na(admin.info$population))>0){
        admin.info$population=admin.info$surveyWeight
      }else{}

      if(is.null(admin.info)||sum(is.na(admin.info$population))>0){
        message("Need population or survey weight information for aggregation")
        aggregation=F
      }


    }


    if(aggregation==F){
      admin1.res=list(res.admin1 =admin1_res)
    }else{


    # aggregate results
      draw.all=expit(t(fit1$draws.est[,-c(1,2)]))
      post.all <- draw.all%*% admin.info$population/sum(admin.info$population)
      agg.natl <- data.frame(mean = mean(post.all),
                             # median=median(post.all),
                            sd = sd(post.all),
                             var = var(post.all),
                             lower=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[1],
                             upper=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[2])






      admin1.res=list(res.admin1 = admin1_res, agg.natl= agg.natl, model = fit1,
                      admin1_post=draw.all,nation_post=post.all)

    }


    return(admin1.res)




    }else if(admin==0){
    stop("No estimates to smooth at admin0 level.")
    # data$admin0.name="Zambia"
    # modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    # modt<- modt[!(is.na(modt$LONGNUM)), ]
    # modt$strata.full <- paste(modt$admin1.name, modt$strata)

    # # Amat=NULL

    # # clusterVar = "~cluster+householdID"
    # # design <- survey::svydesign(ids = stats::formula(clusterVar),
    # #                             weights = ~weight , data = modt)
    # #
    # # admin0_res <- survey::svyby(formula = ~value, by = ~admin0.name,
    # #                       design = design,
    # #                       survey::svymean,
    # #                       drop.empty.groups = FALSE)


    # smoothSurvey_res<-smoothSurvey(as.data.frame(modt),
    #                                responseType ="binary",
    #                                responseVar= "value",
    #                                regionVar = "admin0.name",
    #                                clusterVar = "~cluster+householdID",
    #                                weightVar = "weight",
    #                                strataVar = "strata.full",
    #                                Amat =NULL,
    #                                CI = 0.95,
    #                                is.unit.level=FALSE,
    #                                smooth=T)
    # admin0_res<-smoothSurvey_res$smooth
    # colnames(admin0_res)[1:3] <- c("admin0.name","value","var")
    # colnames(admin0_res)[5:6] <- c("lower","upper")

    # admin0_res$sd<-sqrt(admin0_res$var)

    # return(admin0.res=admin0_res)

    }

}
