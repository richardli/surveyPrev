#' Calculate smoothed direct estimates
#'
#' This function calculate smoothed direct estimates at given admin level.
#'
#' @param data  dataframe that contains the indicator of interests
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admin.info dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#' @param CI Credible interval to be used. Default to 0.95.
#' @param model  smoothing model used in the random effect. Options are independent ("iid") or spatial ("bym2").
#' @param aggregation whether or not report aggregation results.
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,

#' @import dplyr
#' @importFrom survey svydesign svyby
#' @importFrom SUMMER smoothSurvey
#' @importFrom stats weighted.mean var
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
#' dhsData <-surveyPrev::getDHSdata(country = "Zambia", 
#'                                  indicator = "ancvisit4+", 
#'                                  year = 2018)
#' 
#' data <- getDHSindicator(dhsData, indicator = indicator)
#' admin.info1 <- adminInfo(geo = ZambiaAdm1, 
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

fhModel <- function(data, cluster.info, admin.info = NULL, admin, CI = 0.95,  model = c("bym2", "iid"), aggregation = FALSE){

  if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }

  if(is.null(admin.info) && model != "iid"){
    message("No admin.info supplied. Using IID random effects.")
    model <- "iid"
  }
  if(model == "bym2"){
    Amat <- admin.info$admin.mat
  }else{
    Amat <- NULL
  }



  if(admin==2){

    #prepare data
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)


    # modt1<- left_join(admin.info2$admin.info,modt,by="DistrictName")
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
                         regionVar = "DistrictName",
                         clusterVar = "~cluster+householdID",#+householdID same result
                         weightVar = "weight",
                         strataVar = "strata.full",
                         Amat =Amat,
                         CI = CI,
                         smooth=T)


    admin2_res <- fit2$smooth
    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'DistrictName'
    colnames(admin2_res)[colnames(admin2_res) == 'mean'] <- 'value'
    admin2_res$sd<-sqrt(admin2_res$var)


    if(aggregation==F){
      res.admin2=admin2_res
    }else{


      if(is.null(weight) || is.null(admin.info)==T){
        stop("Need admin.info and weight for aggregation")
      }

    # admin2_res$value<-1:115
    # admin2_res$var<-0.6


    ##
    ## TODO: also need to deal with duplicated admin2 names
    ##       the below codes computes only overall proportion?
    # The group_by call needs to be before mutate? same results

    #aggregate results
    admin1_agg<- left_join(admin2_res,admin.info$admin.info,by="DistrictName")%>%
      group_by(admin1.name) %>%
      mutate(prop=population/sum(population))%>%
      summarise(value = stats::weighted.mean(value, prop))

    #code used for agg direct est
    # weight_dt_mean<-left_join(admin2_res,distinct(admin.info$admin.info), by="DistrictName")%>%
    #   group_by(admin1.name)%>%
    #   mutate(prop=round(population/sum(population),digits = 4))%>%
    #   mutate(value1=prop*value)
    # admin1_agg<-aggregate(value1 ~ admin1.name, data = weight_dt_mean, sum)



    nation_agg<- admin.info$admin.info%>%
                  distinct(admin1.name,.keep_all = TRUE)%>%
                  left_join(admin1_agg,by="admin1.name")%>%
                  mutate(prop=population1/sum(population1))%>%
                  summarise(weighted_avg = weighted.mean(value, prop))%>%
                  mutate(value = sprintf("%.4f", weighted_avg))

    admin2.res=list(res.admin2=admin2_res,agg.admin1=admin1_agg,agg.natl=nation_agg, model = fit2)
    }

    return(admin2.res)

    }else if(admin==1){

    #prepare data
    modt<- left_join(data,cluster.info$cluster.info,by="cluster")
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
                         CI = CI)

    admin1_res <- fit1$smooth
    colnames(admin1_res)[colnames(admin1_res) == 'region'] <- 'admin1.name'
    colnames(admin1_res)[colnames(admin1_res) == 'mean'] <- 'value'
    admin1_res$sd<-sqrt(admin1_res$var)



    if(aggregation==F){
      admin1.res=admin1_res
    }else{

      if(is.null(admin.info)){
        stop("Need admin.info for aggregation")
      }

    # aggregate results
    nation_agg<- left_join(admin1_res,admin.info$admin.info,by="admin1.name")%>%
      mutate(prop=population/sum(population))%>%
      summarise(value = weighted.mean(value, prop))%>%
      mutate(value = sprintf("%.4f", value))



    admin1.res=list(res.admin1 = admin1_res, agg.natl=nation_agg, model = fit1)

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
