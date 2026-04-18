#' Process DHS data
#'
#' This function processes DHS data from getDHSdata function.
#'
#' @param Rdata Result from getDHSdata function, the raw DHS survry data from get_datasets.
#' @param indicator Indicator of interests.
#' @param FUN a function to process the DHS data into a binary indicator if not using one of the implemented indicators. See surveyPrev::AN_ANEM_W_ANY for an example function to obtain the indicator for women classified as having any anemia.
#' @param nmr.year This is an argument specifically for NMR calculation. It specifies births how many years do we include prior to the date of survey. Default to be 10, i.e., NMR in the last 10 years prior to survey.
#' @param filter This arguments specifies how the data should be filtered for creating a customized indicator. It should be a character string or a vector of character strings specifying the expression used to filter the data. See example for details
#' @param yesCondition  This arguments specifies how to define a yes label, i.e., value = 1, for creating a customized indicator. It should be a character string specifying the expression used to define the outcome value equal to 1. See example for details.
#' @param noCondition   This arguments specifies how to define a no label, i.e., value = 0, for creating a customized indicator. It should be a character string specifying the expression used to define the outcome value equal to 0. See example for details.
#' @return The function returns processed survey data that contains the indicator of interests.
#'
#' @importFrom naniar replace_with_na
#' @importFrom rlang parse_expr
#' @importFrom stats as.formula
#' @import dplyr
#' @importFrom sjlabelled set_label add_labels
#' @importFrom utils getFromNamespace
#' @import labelled
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData1 <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#' data1 <- getDHSindicator(dhsData1, indicator = "ancvisit4+")
#'
#'
#' #------------------------------------------------------------------#
#' # User-specified function to process the data
#' # For example see the internal function surveyPrev::AN_ANEM_W_ANY
#' #------------------------------------------------------------------#
#' dhsData2 <- getDHSdata(country = "Zambia",
#'                                  indicator = NULL,
#'                                  year = 2018)
#' data2 <- getDHSindicator(dhsData2, indicator = NULL,
#'                          FUN = surveyPrev::AN_ANEM_W_ANY)
#' # which should be identical to the following
#' dhsData3 <- getDHSdata(country = "Zambia",
#'                                  indicator = "womananemia",
#'                                  year = 2018)
#' data3 <- getDHSindicator(dhsData3, indicator = "womananemia")
#'
#' #------------------------------------------------------------------#
#' # User-specified filtering condition
#' # Demonstrating NMR data preparation by specifying how to subset data
#' #    and specify the outcome variable and its levels
#' #------------------------------------------------------------------#
#' Recode <- "Births Recode"
#' dhsData4 <- getDHSdata(country = "Zambia", indicator = NULL,
#'                        Recode=Recode,year = "2018")
#' #
#' # Here we filter the births to be within the last 10 years
#' #   this is specified by condition = "v008 - b3 < 120"
#' # b3 is the date of births in CMC format
#' # v008 is the date of interview in CMC format
#' # the difference is the number of months between the two dates
#' # b7 is the age of death for the child. We consider neonatal deaths where
#' #   b7 = 0.
#' # b7 = NA when the child is alive at the time of interview.
#' data4 <- getDHSindicator(Rdata = dhsData4,
#'                         indicator = NULL,
#'                         filter = "v008 - b3 < 120",
#'                         yesCondition = "b7 == 0",
#'                         noCondition = "b7 > 0 | is.na(b7)")
#'
#' # This will return the same dataset as below
#' data5 <- getDHSindicator(Rdata = dhsData4, indicator = "nmr")
#'
#' # Notice that filter can have more than one conditions specified by vector
#' # The following four specifications lead to the same dataset for
#' #  neonatal deaths in the last 5 years
#' data6a <- getDHSindicator(Rdata = dhsData4,
#'                         indicator = NULL,
#'                         filter = "v008 - b3 < 120 & v008 - b3 < 60",
#'                         yesCondition = "b7 == 0",
#'                         noCondition = "b7 > 0 | is.na(b7)")
#' data6b <- getDHSindicator(Rdata = dhsData4,
#'                         indicator = NULL,
#'                         filter = c("v008 - b3 < 120", "v008 - b3 < 60"),
#'                         yesCondition = "b7 == 0",
#'                         noCondition = "b7 > 0 | is.na(b7)")
#' data6c <- getDHSindicator(Rdata = dhsData4,
#'                         indicator = NULL,
#'                         filter = "v008 - b3 < 60",
#'                         yesCondition = "b7 == 0",
#'                         noCondition = "b7 > 0 | is.na(b7)")
#' data7 <- getDHSindicator(Rdata = dhsData4, indicator = "nmr", nmr.year = 5)
#' }
#'
#' @export
getDHSindicator <- function(Rdata, indicator = NULL, FUN = NULL, nmr.year = 10,
        filter = NULL, yesCondition = NULL, noCondition = NULL) {
# data(match_all_result)
  raw.dat.tmp <- NULL
  ## U5MR ##
  if(!is.null(indicator) && indicator == "u5mr"){
    # convert v5 to factor using libraries already imported in surveyPrev
    Rdata$b5 <- labelled::to_factor(Rdata$b5)
    # Using the same way as surveyPrev in defining 10 year cutoff by month
    Rdata <- subset(Rdata, v008-12*nmr.year-b3 < 0)
    Rdata$strata <- NA
    # Get birth coded as person-months
    # year.cut is specified for a wide range to avoid SUMMER's rule of dropping partial year observations for now
    raw.dat.tmp <- SUMMER::getBirths(data = Rdata,  strata = c("strata"),
                   year.cut = c(0, 30000))
    raw.dat.tmp$value <- raw.dat.tmp$died
  }

  if(is.null(indicator) && !is.null(FUN)){

    raw.dat.tmp <- tryCatch(
      FUN(Rdata),
      error = function(e) {
        stop(
          paste0(
            "Failed to compute indicator '", indicator, "'. ",
            "This happens when the function isn’t appropriate for this survey or indicator.",
            "or wrong DHS recode was provided (e.g., HR/MR/IR/KR mismatch).","Please contact the surveyPrev team for further help.","\n",
            "Original error: ", conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )


  }else if(is.null(indicator) && !is.null(yesCondition)){

    data <- data.frame(Rdata)
    if(!is.null(filter)){
      for(f in filter) data = data %>% filter(!! rlang::parse_expr(f))
    }
    raw.dat.tmp <- data %>% mutate(value =
              case_when((!!rlang::parse_expr(noCondition)) ~ 0,
                        (!!rlang::parse_expr(yesCondition)) ~ 1))

  }else if(indicator %in% indicatorList$ID){

    FUN=getFromNamespace(indicator, "surveyPrev")

    raw.dat.tmp <- tryCatch(
      FUN(Rdata),
      error = function(e) {
        stop(
          paste0(
            "Failed to compute indicator '", indicator, "'. ",
            "This happens when the function isn’t appropriate for this survey or indicator.",
            "or wrong DHS recode was provided (e.g., HR/MR/IR/KR mismatch).", "Please contact the surveyPrev team for further help.","\n",
            "Original error: ", conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )


  }else if(indicator == "unmet_family"||indicator == "FP_NADA_W_UNT"){

    raw.dat.tmp <- fp_unmet_tot( Rdata)
  }else if(indicator == "FP_CUSA_W_MOD"){
    raw.dat.tmp <- fp_cruse_mod(Rdata)

  }else if(indicator == "nmr"||indicator == "CM_ECMR_C_NNR"){
     if(indicator == "nmr"){
       message("nmr, in the ten years preceding the survey is used")
     }
    raw.dat.tmp <- CM_ECMR_C_NNR(Rdata)
  }else if(indicator =="ancvisit4+"||indicator == "RH_ANCN_W_N4P"){

    raw.dat.tmp <- RH_ANCN_W_N4P(Rdata)


  }else if(indicator == "RH_DELA_C_SKP"){

    raw.dat.tmp <- RH_DELA_C_SKP(Rdata)
  }else if(indicator == "DPT3"||indicator == "CH_VACC_C_DP3"){


    raw.dat.tmp <- CH_VACC_C_DP3(Rdata)


  }else if(indicator== "CH_VACC_C_MSL") {

    raw.dat.tmp <- CH_VACC_C_MSL(Rdata)

  }else if(indicator== "PCV3") {

    raw.dat.tmp <- ch_pneumo3_either(Rdata)

  } else if(indicator== "RotaC1") {

    raw.dat.tmp <- ch_rotav1_either(Rdata)

  } else if(indicator =="CH_VACC_C_DP1"){
    raw.dat.tmp <- CH_VACC_C_DP1(Rdata)
  }else if(indicator =="CH_VACC_C_BAS"){
    raw.dat.tmp <- CH_VACC_C_BAS(Rdata)
  }else if(indicator =="CH_VACC_C_NON"){
    raw.dat.tmp <- CH_VACC_C_NON(Rdata)
  }else if(indicator =="CH_DIAT_C_ORT"){
    raw.dat.tmp <- CH_DIAT_C_ORT(Rdata)
  }else if(indicator == "wasting"||indicator=="CN_NUTS_C_WH2"){

    raw.dat.tmp <- CN_NUTS_C_WH2(Rdata)

  } else if(indicator == "stunting"||indicator=="CN_NUTS_C_HA2") {
      raw.dat.tmp <- CN_NUTS_C_HA2(Rdata)

  }else if (indicator == "womananemia"|| indicator == "AN_ANEM_W_ANY"){

  raw.dat.tmp <- AN_ANEM_W_ANY(Rdata)

  }else if(indicator=="CN_BRFS_C_EXB"){

    raw.dat.tmp <- CN_BRFS_C_EXB(Rdata)

  } else if(indicator=="CN_ANMC_C_ANY") {

    raw.dat.tmp <- CN_ANMC_C_ANY(Rdata)

  } else if(indicator== "AN_NUTS_W_THN") {

    raw.dat.tmp <- AN_NUTS_W_THN(Rdata)

  }else if(indicator== "ML_NETP_H_IT2") {

    raw.dat.tmp <- ML_NETP_H_IT2(Rdata)

  } else if(indicator== "ML_PMAL_C_RDT") {

    raw.dat.tmp <- ML_PMAL_C_RDT(Rdata)

  } else if(indicator== "HA_HIVP_B_HIV") {

    raw.dat.tmp <- HA_HIVP_B_HIV(Rdata)

  } else if(indicator== "WS_TLET_H_IMP"||indicator== "sanitation") {

    raw.dat.tmp <- WS_TLET_H_IMP(Rdata)

  }else if(indicator== "WS_TLET_P_BAS") {

    raw.dat.tmp <- WS_TLET_P_BAS(Rdata)

  } else if(indicator== "WS_SRCE_P_BAS") {

    raw.dat.tmp <- WS_SRCE_P_BAS(Rdata)

  }else{
    stop(paste0(indicator," is not defined in the surveyPrev library right now, please check surveyPrev::indicatorList.",
                "Please contact the surveyPrev team for further help."))
  }



  if (is.null(raw.dat.tmp)) {
    stop("No indicator output was created. Provide a valid `indicator`, or `FUN`. Please contact the surveyPrev team for further help.",
         call. = FALSE)
  }
  if (!is.data.frame(raw.dat.tmp)) {
    stop("Indicator computation did not return a data.frame. Please contact the surveyPrev team for further help.", call. = FALSE)
  }




  dat.tmp <- tryCatch({
    if("hv001" %in% colnames(raw.dat.tmp)){
      pre <- "h"
    }else if("mv001" %in% colnames(raw.dat.tmp)){
      pre <- "m"
    }else if("v001" %in% colnames(raw.dat.tmp)){
      pre <- ""
    }


    strat <- attr(raw.dat.tmp[, paste0(pre, "v025")], which='labels')
    names(strat) <- tolower(names(strat))
    raw.dat.tmp[, paste0(pre, "v025")] <- ifelse(raw.dat.tmp[, paste0(pre, "v025")] == strat["urban"][[1]],'urban','rural')
    raw.dat.tmp[, paste0(pre, "v025")] <- factor(raw.dat.tmp[, paste0(pre, "v025")], levels = c('urban','rural'))
    raw.dat.tmp[, paste0(pre, "v024")] <- factor(labelled::unlabelled(raw.dat.tmp[, paste0(pre, "v024")]))
    raw.dat.tmp[, paste0(pre, "v023")] <- factor(labelled::unlabelled(raw.dat.tmp[, paste0(pre, "v023")]))
    raw.dat.tmp[, paste0(pre, "v022")] <- factor(labelled::unlabelled(raw.dat.tmp[, paste0(pre, "v022")]))


    is_mortality <- (!is.null(indicator) &&
                       indicator %in% c( "CM_ECMR_C_U5M", "CM_ECMR_C_U5F",
                                        "CM_ECMR_C_IMR","CM_ECMR_C_IMF")) ||
      (!is.null(FUN) &&
         any(sapply(c("CM_ECMR_C_U5M", "CM_ECMR_C_U5F","CM_ECMR_C_IMR","CM_ECMR_C_IMF"),
                    function(nm) identical(FUN, getFromNamespace(nm, "surveyPrev")))))



  if (is_mortality) {
    raw.dat.tmp %>% dplyr::select(
      cluster     = paste0(pre, "v001"),
      householdID = paste0(pre, "v002"),
      v022        = paste0(pre, "v022"),
      v023        = paste0(pre, "v023"),
      v024        = paste0(pre, "v024"),
      weight      = paste0(pre, "v005"),
      strata      = paste0(pre, "v025"),
      value       = "value",
      age         = "age"
    )
  } else {
    raw.dat.tmp %>% dplyr::select(
      cluster     = paste0(pre, "v001"),
      householdID = paste0(pre, "v002"),
      v022        = paste0(pre, "v022"),
      v023        = paste0(pre, "v023"),
      v024        = paste0(pre, "v024"),
      weight      = paste0(pre, "v005"),
      strata      = paste0(pre, "v025"),
      value       = "value"
    )



  }

  }, error = function(e) {
    warning(
      paste0(
        "Post-processing failed, likely due to unexpected column names. If indicator = NULL and a user-defined FUN is used, this may indicate that the wrong DHS recode (e.g., HR/MR/IR/KR mismatch) was provided.",

        "Original error: ", conditionMessage(e), "\n",
        "Please contact the surveyPrev team for further help."
      ),
      call. = FALSE
    )
  })

# if("hv001" %in% colnames(raw.dat.tmp)){
#   pre <- "h"
# }else if("mv001" %in% colnames(raw.dat.tmp)){
#   pre <- "m"
# }else if("v001" %in% colnames(raw.dat.tmp)){
#   pre <- ""
# }


#
# strat <- attr(raw.dat.tmp[, paste0(pre, "v025")], which='labels')
# names(strat) <- tolower(names(strat))
# raw.dat.tmp[, paste0(pre, "v025")] <- ifelse(raw.dat.tmp[, paste0(pre, "v025")] == strat["urban"][[1]],'urban','rural')
# raw.dat.tmp[, paste0(pre, "v025")] <- factor(raw.dat.tmp[, paste0(pre, "v025")], levels = c('urban','rural'))
# raw.dat.tmp[, paste0(pre, "v024")] <- factor(labelled::unlabelled(raw.dat.tmp[, paste0(pre, "v024")]))
# raw.dat.tmp[, paste0(pre, "v023")] <- factor(labelled::unlabelled(raw.dat.tmp[, paste0(pre, "v023")]))
# raw.dat.tmp[, paste0(pre, "v022")] <- factor(labelled::unlabelled(raw.dat.tmp[, paste0(pre, "v022")]))


# if(indicator %in% c("u5mr","CM_ECMR_C_U5M","CM_ECMR_C_U5F")){
#   dat.tmp<-  raw.dat.tmp %>%
#     dplyr::  select(c(cluster= paste0(pre, "v001"),
#                       householdID= paste0(pre, "v002"),
#                       v022= paste0(pre, "v022"),
#                       v023= paste0(pre, "v023"),
#                       v024= paste0(pre, "v024"),
#                       weight= paste0(pre, "v005"),
#                       strata= paste0(pre, "v025"),
#                       value="value",
#                       age = "age"))
# }else{
#   dat.tmp<-  raw.dat.tmp %>%
#     dplyr::  select(c(cluster= paste0(pre, "v001"),
#                       householdID= paste0(pre, "v002"),
#                       v022= paste0(pre, "v022"),
#                       v023= paste0(pre, "v023"),
#                       v024= paste0(pre, "v024"),
#                       weight= paste0(pre, "v005"),
#                       strata= paste0(pre, "v025"),
#                       value="value"))
# }




  # Check urban/rural
  test <- aggregate(strata ~ cluster, data = dat.tmp, FUN = function(x) { length(unique(x)) })
  inconsistent_clusters <- test$cluster[test$strata > 1]

  if(length(inconsistent_clusters) > 0){
    for (cluster_id in inconsistent_clusters) {
      # Subset data for the inconsistent cluster
      cluster_data <- dat.tmp[dat.tmp$cluster == cluster_id, ]
      # Determine majority vote for `strata` in the cluster
      majority_strata <- names(sort(table(cluster_data$strata), decreasing = TRUE))[1]
      # Replace `strata` values in the inconsistent cluster with the majority value
      dat.tmp$strata[dat.tmp$cluster == cluster_id] <- majority_strata
    }
    # Issue a warning with the inconsistent cluster IDs
    message("Inconsistent strata variable in the following clusters have been replaced with majority assignment: ", paste(inconsistent_clusters, collapse = ", "))
  }







  return(dat.tmp)

}

