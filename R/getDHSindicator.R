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
#' @import tidyverse
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

  if(is.null(indicator) && !is.null(FUN)){
    raw.dat.tmp <- FUN(Rdata)
  }else if(is.null(indicator) && !is.null(yesCondition)){

    data <- data.frame(Rdata)
    if(!is.null(filter)){
      for(f in filter) data = data %>% filter(!! rlang::parse_expr(f))
    }
    raw.dat.tmp <- data %>% mutate(value =
              case_when((!!rlang::parse_expr(noCondition)) ~ 0,
                        (!!rlang::parse_expr(yesCondition)) ~ 1))

  }else if(indicator %in% match_all_result$indicator_ID_DHS){

    FUN=getFromNamespace(indicator, "surveyPrev")
    raw.dat.tmp <-FUN(Rdata)

  }else if(indicator == "unmet_family"||indicator == "FP_NADA_W_UNT"){
    # IRdata <- Rdata %>%
    #   mutate(wt = v005/1000000)
    #
    # IRdata <- IRdata %>%
    #   mutate(n_un_fam_plan =
    #            case_when(
    #              v626 == 1 ~ 1,
    #              v626 == 2 ~ 1,
    #              v626 == 3 ~ 0,
    #              v626 == 4 ~ 0,
    #              v626 %in% c(5, 6, 7, 8, 9) ~ NA)) %>%
    #   set_value_labels(n_un_fam_plan = c("Yes" = 1, "No"=0  )) %>%
    #   set_variable_labels(n_un_fam_plan = "Unmet need for family planning")
    #
    # raw.dat.tmp<-IRdata
    # colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'n_un_fam_plan'] <- "value"
    raw.dat.tmp <- fp_unmet_tot( Rdata)
  }
  else if(indicator == "FP_CUSA_W_MOD"){
    raw.dat.tmp <- fp_cruse_mod(Rdata)

  }

  else if(indicator == "nmr"||indicator == "CM_ECMR_C_NNR"){
    # BRdata <- Rdata %>%
    #   mutate(wt = v005/1000000)
    # BRdata$value<- ifelse(BRdata$b7==0, 1, 0)
    # BRdata$value[is.na( BRdata$value)] <- 0
    #
    # raw.dat.tmp=BRdata
    raw.dat.tmp <- NMR(Rdata, nmr.year)
  }

  # else if(indicator == "sanitation"){
  #   PRdata <- Rdata %>%
  #     mutate(wt = hv005/1000000)
  #   PRdata <- PRdata %>%
  #     mutate(n_improved_sani =
  #              case_when(
  #                # household using improved sanitation = 1
  #                hv205 %in% c(10, 11, 12, 13, 14, 15, 20, 21, 22, 41) ~ 1,
  #                # household using unimproved sanitation = 0
  #                hv205 %in% c(14, 23, 42, 43, 96, 31, 30) ~ 0)) %>%
  #     #replace_with_na(replace = list(n_improved_sani = c(99))) %>%
  #     set_value_labels(n_improved_sani = c("Yes" = 1, "No"=0  )) %>%
  #     set_variable_labels(n_improved_sani = "Household using improved sanitation")
  #
  #   raw.dat.tmp<-PRdata
  #   colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'n_improved_sani'] <- "value"
  #
  # }


  else if(indicator =="ancvisit4+"||indicator == "RH_ANCN_W_N4P"){
    # IRdata <- Rdata%>%
    #   mutate(wt = v005/1000000) %>%
    #   mutate(period = 60)
    #
    # # age of child. If b19_01 is not available in the data use v008 - b3_01
    # if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    #   IRdata [[paste("b19_01")]] <- NA
    # if ("TRUE" %in% all(is.na(IRdata $b19_01)))
    # { b19_included <- 0} else { b19_included <- 1}
    #
    # if (b19_included==1) {
    #   IRdata <- IRdata %>%
    #     mutate(age = b19_01)
    # } else {
    #   IRdata <- IRdata %>%
    #     mutate(age = v008 - b3_01)
    # }
    # # //Number of ANC visits in 4 categories that match the table in the final report
    # IRdata <- IRdata %>%
    #   mutate(rh_anc_numvs =
    #            case_when(
    #              m14_1 == 0 ~ 0 ,
    #              m14_1 == 1 ~ 1 ,
    #              m14_1  %in% c(2,3)   ~ 2 ,
    #              m14_1>=4 & m14_1<=90  ~ 3 ,
    #              m14_1>90  ~ 9 ,
    #              age>=period ~ 99 )) %>%
    #   replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
    #   set_value_labels(rh_anc_numvs = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) %>%
    #   set_variable_labels(rh_anc_numvs = "Number of ANC visits")
    #
    # # //4+ ANC visits
    # IRdata <- IRdata %>%
    #   mutate(rh_anc_4vs =
    #            case_when(
    #              rh_anc_numvs==3 ~ 1,
    #              rh_anc_numvs %in% c(0,1,2,9)   ~ 0 )) %>%
    #   set_value_labels(rh_anc_4vs = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(rh_anc_4vs = "Attended 4+ ANC visits")
    # raw.dat.tmp<-IRdata
    # colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'rh_anc_4vs'] <- "value"
    # raw.dat.tmp <- raw.dat.tmp %>% filter( age<60)
    raw.dat.tmp <- rh_anc_4vs(Rdata)


  }
  else if(indicator == "RH_DELA_C_SKP"){
    # BRdata <- Rdata %>%
    #   mutate(wt = v005/1000000)
    # BRdata$value<- ifelse(BRdata$b7==0, 1, 0)
    # BRdata$value[is.na( BRdata$value)] <- 0
    #
    # raw.dat.tmp=BRdata
    raw.dat.tmp <- rh_del_pvskill(Rdata)
  }


  else if(indicator == "DPT3"||indicator == "CH_VACC_C_DP3"){
    # KRdata <- Rdata %>%
    #   mutate(wt = v005/1000000)
    #
    # # age of child. If b19 is not available in the data use v008 - b3
    # if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    #   KRdata [[paste("b19")]] <- NA
    # if ("TRUE" %in% all(is.na(KRdata$b19)))
    # { b19_included <- 0} else { b19_included <- 1}
    #
    # if (b19_included==1) {
    #   KRdata <- KRdata %>%
    #     mutate(age = b19)
    # } else {
    #   KRdata <- KRdata %>%
    #     mutate(age = v008 - b3)
    # }
    #
    # # *** Two age groups used for reporting.
    # KRdata <- KRdata %>%
    #   mutate(agegroup =
    #            case_when(
    #              age>=12 & age<=23 ~ 1,
    #              age>=24 & age<=35 ~ 2  )) %>%
    #   set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    #   set_variable_labels(agegroup = "age group of child for vaccination")
    #
    # # Selecting children
    # # Create subset of KRfile to select for children for VAC indicators
    # # Select agegroup 1 or agegroup 2
    # KRvac <- KRdata %>%
    #   subset(agegroup==1 & b5==1) # select age group and live children
    #
    # # *******************************************************************************
    #
    # # Source of vaccination information. We need this variable to code vaccination indicators by source.
    # KRvac <- KRvac %>%
    #   mutate(source =
    #            case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
    #   set_value_labels(source = c("card" = 1, "mother"=2)) %>%
    #   set_variable_labels(source = "source of vaccination information")
    #
    # # *** Pentavalent ***
    # # //DPT 1, 2, 3 either source
    # KRvac <- KRvac %>%
    #   mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    #   mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    #   mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    #   mutate(dptsum = dpt1 + dpt2 + dpt3)
    # # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
    # # See DHS guide to statistics for further explanation
    # KRvac <- KRvac %>%
    #   mutate(ch_pent1_either = case_when(dptsum >=1 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent1_either = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent1_either = "Pentavalent 1st dose vaccination according to either source") %>%
    #   mutate(ch_pent2_either = case_when(dptsum >=2 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent2_either = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent2_either = "Pentavalent 2nd dose vaccination according to either source") %>%
    #   mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent3_either = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent3_either = "Pentavalent 3rd dose vaccination according to either source")
    #
    # # //DPT 1, 2, 3 mother's report
    # KRvac <- KRvac %>%
    #   mutate(ch_pent1_moth = case_when(dptsum >=1 & source==2~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent1_moth = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent1_moth = "Pentavalent 1st dose vaccination according to mother") %>%
    #   mutate(ch_pent2_moth = case_when(dptsum >=2 & source==2 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent2_moth = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent2_moth = "Pentavalent 2nd dose vaccination according to mother") %>%
    #   mutate(ch_pent3_moth = case_when(dptsum >=3 & source==2 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent3_moth = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent3_moth = "Pentavalent 3rd dose vaccination according to mother")
    #
    # # //DPT 1, 2, 3 by card
    # KRvac <- KRvac %>%
    #   mutate(ch_pent1_card = case_when(dptsum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent1_card = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent1_card = "Pentavalent 1st dose vaccination according to card") %>%
    #   mutate(ch_pent2_card = case_when(dptsum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent2_card = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent2_card = "Pentavalent 2nd dose vaccination according to card") %>%
    #   mutate(ch_pent3_card = case_when(dptsum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
    #   set_value_labels(ch_pent3_card = c("Yes" = 1, "No"=0)) %>%
    #   set_variable_labels(ch_pent3_card = "Pentavalent 3rd dose vaccination according to card")
    #
    # raw.dat.tmp<-KRvac
    # colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'ch_pent3_either'] <- "value"
    raw.dat.tmp <- ch_pent3_either(Rdata)


  }
  else if(indicator== "CH_VACC_C_MSL") {

    raw.dat.tmp <- ch_meas_either(Rdata)

  }
  else if(indicator== "PCV3") {

    raw.dat.tmp <- ch_pneumo3_either(Rdata)

  }
  else if(indicator== "RotaC1") {

    raw.dat.tmp <- ch_rotav1_either(Rdata)

  }


  else if(indicator =="CH_VACC_C_DP1"){
    raw.dat.tmp <- ch_pent1_either(Rdata)
  }
  else if(indicator =="CH_VACC_C_BAS"){
    raw.dat.tmp <- ch_allvac_either(Rdata)
  }
  else if(indicator =="CH_VACC_C_NON"){
    raw.dat.tmp <- ch_novac_either(Rdata)
  }


  else if(indicator =="CH_DIAT_C_ORT"){
    raw.dat.tmp <- ch_diar_ors_rhf(Rdata)
  }

  else if(indicator == "wasting"||indicator=="CN_NUTS_C_WH2"){
  #   PRdata <- Rdata %>%
  #   mutate(wt = hv005/1000000)
  # PRdata <- PRdata %>%
  #   mutate(nt_ch_wast =
  #            case_when(
  #              hv103==1 &  hc72< -200  ~ 1 ,
  #              hv103==1 &  hc72>= -200 ~ 0 ,
  #              hc72>=9996 ~ 99)) %>%
  #   replace_with_na(replace = list(nt_ch_wast = c(99))) %>%
  #   set_value_labels(nt_ch_wast = c("Yes" = 1, "No"=0  )) %>%
  #   set_variable_labels(nt_ch_wast = "Wasted child under 5 years")
  #
  #   raw.dat.tmp<-PRdata
  #   colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'nt_ch_wast'] <- "value"
    raw.dat.tmp <- nt_ch_wast(Rdata)

  }
  else if(indicator == "stunting"||indicator=="CN_NUTS_C_HA2") {
  #   PRdata <- Rdata %>%
  #   mutate(wt = hv005/1000000)
  # PRdata <- PRdata %>%
  #   mutate(nt_ch_stunt =
  #            case_when(
  #              hv103==1 &  hc70< -200  ~ 1 ,
  #              hv103==1 &  hc70>= -200 ~ 0 ,
  #              hc70>=9996 ~ 99)) %>%
  #   replace_with_na(replace = list(nt_ch_stunt = c(99))) %>%
  #   set_value_labels(nt_ch_stunt = c("Yes" = 1, "No"=0  )) %>%
  #   set_variable_labels(nt_ch_stunt = "Stunted child under 5 years")
  #
  #   raw.dat.tmp<-PRdata
  #   colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'nt_ch_stunt'] <- "value"
    raw.dat.tmp <- nt_ch_stunt(Rdata)

  }
  else if (indicator == "womananemia"|| indicator == "AN_ANEM_W_ANY"){

  #
  # *** Anemia indicators ***
  #
  # //Any anemia
  # IRdata <- IRdata %>%
  #   mutate(nt_wm_any_anem =
  #            case_when(
  #              v042==1 & v457<4 ~ 1 ,
  #              v042==1 &  v455==0 ~ 0)) %>%
  #   set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
  #   set_variable_labels(nt_wm_any_anem = "Any anemia - women")

  #  raw.dat.tmp<-IRdata
  #   colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'nt_wm_any_anem'] <- "value"
  raw.dat.tmp <- nt_wm_any_anem(Rdata)

  }

  else if(indicator=="CN_BRFS_C_EXB"){

    raw.dat.tmp <- nt_ebf(Rdata)

  }
  else if(indicator=="CN_ANMC_C_ANY") {

    raw.dat.tmp <- nt_ch_any_anem(Rdata)

  }
  else if(indicator== "AN_NUTS_W_THN") {

    raw.dat.tmp <- nt_wm_thin(Rdata)

  }

  else if(indicator== "ML_NETP_H_IT2") {

    raw.dat.tmp <- ml_hhaccess(Rdata)

  }
  else if(indicator== "ML_PMAL_C_RDT") {

    raw.dat.tmp <- ml_test_rdtmal(Rdata)

  }

  else if(indicator== "HA_HIVP_B_HIV") {

    raw.dat.tmp <- hv_hiv_pos(Rdata)

  }


  else if(indicator== "WS_TLET_H_IMP"||indicator== "sanitation") {

    raw.dat.tmp <- ph_sani_improve(Rdata)

  }
  else if(indicator== "WS_TLET_P_BAS") {

    raw.dat.tmp <- ph_sani_basic(Rdata)

  }

  else if(indicator== "WS_SRCE_P_BAS") {

    raw.dat.tmp <- ph_wtr_basic(Rdata)

  }


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

  dat.tmp<-  raw.dat.tmp %>%
      dplyr::  select(c(cluster= paste0(pre, "v001"),
                        householdID= paste0(pre, "v002"),
                        v022= paste0(pre, "v022"),
                        v023= paste0(pre, "v023"),
                        v024= paste0(pre, "v024"),
                        weight= paste0(pre, "v005"),
                        strata= paste0(pre, "v025"),
                        value="value"))


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

