
################
#Family Planning ch7
################

#'FP_NADA_W_UNT
#'#unmet_family  IRdata
#'women with an unmet need for family planning for spacing and limiting
#'
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "FP_NADA_W_UNT",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::fp_unmet_tot)
#' }
#' @export

fp_unmet_tot<- function(Rdata){
  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)
  IRdata <- IRdata %>%
    mutate(
      # fp_unmet_space  = ifelse(v626a==1, 1, 0),
      # fp_unmet_limit  = ifelse(v626a==2, 1, 0),
      fp_unmet_tot    = ifelse(v626a==1|v626a==2, 1, 0),
      # fp_met_space    = ifelse(v626a==3, 1, 0),
      # fp_met_limit    = ifelse(v626a==4, 1, 0),
      # fp_met_tot      = ifelse(v626a==3|v626a==4, 1, 0),
      # fp_demand_space = ifelse(v626a==1|v626a==3, 1, 0),
      # fp_demand_limit = ifelse(v626a==2|v626a==4, 1, 0),
      # fp_demand_tot   = ifelse(fp_unmet_tot|fp_met_tot, 1, 0),
      # fp_demsat_mod   = ifelse(fp_demand_tot, ifelse(fp_met_tot & v313==3, 1, 0), NA),
      # fp_demsat_any   = ifelse(fp_demand_tot, ifelse(fp_met_tot, 1, 0), NA),
    )

  colnames(IRdata)[colnames(IRdata) == 'fp_unmet_tot'] <- "value"
  return(IRdata)
}

#'FP_CUSM_W_MOD
#'IRdata
#'Modern contraceptive prevalence rate (Married women currently using any modern method of contraception)
#'
#'
#'
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "FP_CUSA_W_MOD",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::fp_cruse_mod)
#' }
#' @export
#'
fp_cruse_mod <- function(Rdata){
  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  IRdata <- IRdata %>%
    mutate(fp_cruse_mod =
             ifelse(v313 == 3, 1, 0)) %>%
    set_value_labels(fp_cruse_mod = c(yes = 1, no = 0)) %>%
    set_variable_labels(fp_cruse_mod ="Currently used any modern method")
  colnames(IRdata)[colnames(IRdata) == 'fp_cruse_mod'] <- "value"
  return(IRdata)
}


################
#Infant and Child Mortality ch8
################
#' CM_ECMR_C_NNR
#' nmr CM_ECMR_C_NNR BR (not from dhs github)
#' Neonatal mortality rate !!!!!!
#' @param Rdata  data.frame from survryPrev::getDHSdata
#' @param nmr.year This is an argument specifically for NMR calculation. It specifies births how many years do we include prior to the date of survey. Default to be 10, i.e., NMR in the last 10 years prior to survey.

#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CM_ECMR_C_NNR",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::NMR)
#' }
#' @export
#'

NMR<- function(Rdata, nmr.year){

  BRdata <- Rdata%>%
    mutate(wt = v005/1000000)%>%
    mutate(bo10 = Rdata$v008-12*nmr.year-b3)%>%
    filter(bo10<0)

  BRdata$value<- ifelse(BRdata$b7==0, 1, 0)
  BRdata$value[is.na( BRdata$value)] <- 0

  return(BRdata)
}


################
#Maternal Healthcare  cp9
################
#'RH_ANCN_W_N4P
#'ancvisit4+ RH_ANCN_W_N4P  IR
#'Antenatal visits for pregnancy: 4+ visits
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "RH_ANCN_W_N4P",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::rh_anc_4vs)
#' }
#' @export
#'

rh_anc_4vs<- function(Rdata){
  IRdata <- Rdata%>%
    mutate(wt = v005/1000000) %>%
    mutate(period = 60)

  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $b19_01)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }
  # //Number of ANC visits in 4 categories that match the table in the final report
  IRdata <- IRdata %>%
    mutate(rh_anc_numvs =
             case_when(
               m14_1 == 0 ~ 0 ,
               m14_1 == 1 ~ 1 ,
               m14_1  %in% c(2,3)   ~ 2 ,
               m14_1>=4 & m14_1<=90  ~ 3 ,
               m14_1>90  ~ 9 ,
               age>=period ~ 99 )) %>%
    replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
    set_value_labels(rh_anc_numvs = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) %>%
    set_variable_labels(rh_anc_numvs = "Number of ANC visits")
IRdata <- IRdata %>%
  mutate(rh_anc_4vs =
           case_when(
             rh_anc_numvs==3 ~ 1,
             rh_anc_numvs %in% c(0,1,2,9)   ~ 0 )) %>%
  set_value_labels(rh_anc_4vs = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_anc_4vs = "Attended 4+ ANC visits")

colnames(IRdata)[colnames(IRdata) == 'rh_anc_4vs'] <- "value"
return(IRdata)
}

#'RH_DELA_C_SKP
#'IR or BR
#'Assistance during delivery from a skilled provider
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "RH_DELA_C_SKP",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::rh_del_pvskill)
#' }
#' @export
#'

rh_del_pvskill <- function(Rdata){

  BRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # period and age of child
  # choose reference period, last 2 years (24 months) or last 5 years (60 months)
  # Using a period of the last 2 years will not match final report but would provide more recent information.
  BRdata <- BRdata %>%
    mutate(period = 60)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(BRdata))))
    BRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(BRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    BRdata <- BRdata %>%
      mutate(age = b19)
  } else {
    BRdata <- BRdata %>%
      mutate(age = v008 - b3)
  }
  # //Assistance during delivery
  # **Note: Assistance during delivery and skilled provider indicators are both country specific indicators.
  # **The table for these indicators in the final report would need to be checked to confirm the code below.
  BRdata <- BRdata %>%
    mutate(rh_del_pv =
             case_when(
               m3a == 1   ~ 1 ,
               m3b == 1 ~ 2,
               m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1~ 3 ,
               m3g == 1 ~ 4 ,
               m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
               m3n ==1 ~ 6,
               m3a ==8 | m3a==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pv = c(99))) %>%
    set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

  # //Skilled provider during delivery
  # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.

  BRdata <- BRdata %>%
    mutate(rh_del_pvskill =
             case_when(
               rh_del_pv %in% c(1,2)   ~ 1 ,
               rh_del_pv %in% c(3,4,5) ~ 2,
               rh_del_pv ==6 ~ 3 ,
               rh_del_pv==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
    set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")


  BRdata$rh_del_pvskill= ifelse( BRdata$rh_del_pvskill == 1, 1, 0)
  colnames(BRdata)[colnames(BRdata) == 'rh_del_pvskill'] <- "value"
return(BRdata)
}



################
#Child Health cp10
################

#'CH_DIAT_C_ORT
#'KR  Diarrhea treatment (Children under five with diarrhea treated with either ORS or RHF)
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_DIAT_C_ORT",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ch_diar_ors_rhf)
#' }
#' @export
#'
ch_diar_ors_rhf<-function(Rdata){

  KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # //Diarrhea symptoms
  KRdata <- KRdata %>%
    mutate(ch_diar =
             case_when(
               (h11==1 | h11==2) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_diar = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar = "Diarrhea in the 2 weeks before the survey")

  # //ORS or RHF
   KRdata <- KRdata %>%
    mutate(ch_diar_ors_rhf =
             case_when(
               ch_diar==1 & (h13==1 | h13==2 | h13b==1 | h14==1 | h14==2) ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(ch_diar_ors_rhf = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar_ors_rhf = "Given either ORS or RHF for diarrhea")


   colnames(KRdata)[colnames(KRdata) == 'ch_diar_ors_rhf'] <- "value"
  return(KRdata)
}

#ARI treatment (Children under five with ARI for whom advice or treatment was sought from a health facility or provider)
#ch_ari_care_day  "Advice or treatment sought for ARI symptoms on the same or next day"
#CH_ARI_FV.do KR



#'CH_VACC_C_DP1
#' KR
#'Percentage of children (age 12-23)
#'Pentavalent 1rd dose vaccination according to either source"
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACC_C_DP1",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ch_pent1_either)
#' }
#' @export
#'
ch_pent1_either<-function(Rdata){

  KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }

  # *** Two age groups used for reporting.
  KRdata <- KRdata %>%
    mutate(agegroup =
             case_when(
               age>=12 & age<=23 ~ 1,
               age>=24 & age<=35 ~ 2  )) %>%
    set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    set_variable_labels(agegroup = "age group of child for vaccination")

  # Selecting children
  # Create subset of KRfile to select for children for VAC indicators
  # Select agegroup 1 or agegroup 2
  KRvac <- KRdata %>%
    subset(agegroup==1 & b5==1) # select age group and live children

  # *******************************************************************************

  # Source of vaccination information. We need this variable to code vaccination indicators by source.
  KRvac <- KRvac %>%
    mutate(source =
             case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
    set_value_labels(source = c("card" = 1, "mother"=2)) %>%
    set_variable_labels(source = "source of vaccination information")

  # *** Pentavalent ***
  # //DPT 1, 2, 3 either source
  KRvac <- KRvac %>%
    mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3)
  # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
  # See DHS guide to statistics for further explanation
  KRvac <- KRvac %>%
    mutate(ch_pent1_either = case_when(dptsum >=1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent1_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent1_either = "Pentavalent 1st dose vaccination according to either source") %>%
    mutate(ch_pent2_either = case_when(dptsum >=2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent2_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent2_either = "Pentavalent 2nd dose vaccination according to either source") %>%
    mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent3_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent3_either = "Pentavalent 3rd dose vaccination according to either source")


  colnames(KRvac)[colnames(KRvac) == 'ch_pent1_either'] <- "value"
  return(KRvac)

}


#'CH_VACC_C_DP3
#'DPT3  KR
#'Percentage of children (age 12-23)
#'Pentavalent 3rd dose vaccination according to either source"
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACC_C_DP3",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ch_pent3_either)
#' }
#' @export


ch_pent3_either<-function(Rdata){

   KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }

  # *** Two age groups used for reporting.
  KRdata <- KRdata %>%
    mutate(agegroup =
             case_when(
               age>=12 & age<=23 ~ 1,
               age>=24 & age<=35 ~ 2  )) %>%
    set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    set_variable_labels(agegroup = "age group of child for vaccination")

  # Selecting children
  # Create subset of KRfile to select for children for VAC indicators
  # Select agegroup 1 or agegroup 2
  KRvac <- KRdata %>%
    subset(agegroup==1 & b5==1) # select age group and live children

  # *******************************************************************************

  # Source of vaccination information. We need this variable to code vaccination indicators by source.
  KRvac <- KRvac %>%
    mutate(source =
             case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
    set_value_labels(source = c("card" = 1, "mother"=2)) %>%
    set_variable_labels(source = "source of vaccination information")

  # *** Pentavalent ***
  # //DPT 1, 2, 3 either source
  KRvac <- KRvac %>%
    mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3)
  # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
  # See DHS guide to statistics for further explanation
  KRvac <- KRvac %>%
    mutate(ch_pent1_either = case_when(dptsum >=1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent1_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent1_either = "Pentavalent 1st dose vaccination according to either source") %>%
    mutate(ch_pent2_either = case_when(dptsum >=2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent2_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent2_either = "Pentavalent 2nd dose vaccination according to either source") %>%
    mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent3_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent3_either = "Pentavalent 3rd dose vaccination according to either source")

  # //DPT 1, 2, 3 mother's report
  KRvac <- KRvac %>%
    mutate(ch_pent1_moth = case_when(dptsum >=1 & source==2~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent1_moth = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent1_moth = "Pentavalent 1st dose vaccination according to mother") %>%
    mutate(ch_pent2_moth = case_when(dptsum >=2 & source==2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent2_moth = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent2_moth = "Pentavalent 2nd dose vaccination according to mother") %>%
    mutate(ch_pent3_moth = case_when(dptsum >=3 & source==2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent3_moth = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent3_moth = "Pentavalent 3rd dose vaccination according to mother")

  # //DPT 1, 2, 3 by card
  KRvac <- KRvac %>%
    mutate(ch_pent1_card = case_when(dptsum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent1_card = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent1_card = "Pentavalent 1st dose vaccination according to card") %>%
    mutate(ch_pent2_card = case_when(dptsum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent2_card = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent2_card = "Pentavalent 2nd dose vaccination according to card") %>%
    mutate(ch_pent3_card = case_when(dptsum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent3_card = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent3_card = "Pentavalent 3rd dose vaccination according to card")

  # raw.dat.tmp<-KRvac
  # colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'ch_pent3_either'] <- "value"

  colnames(KRvac)[colnames(KRvac) == 'ch_pent3_either'] <- "value"
  return(KRvac)

}






#'CH_VACC_C_MSL
#'MCV: Measles
#'Measles vaccination received
#'Percentage of children (age 12-23)
#'ch_meas_either   CH_VAC.do  KR
#'"Measles vaccination according to either source"
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACC_C_MSL",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ch_meas_either)
#' }
#' @export
#'

ch_meas_either<-function(Rdata){

  # weight variable
  KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }


  # *** Two age groups used for reporting.
  KRdata <- KRdata %>%
    mutate(agegroup =
             case_when(
               age>=12 & age<=23 ~ 1,
               age>=24 & age<=35 ~ 2  )) %>%
    set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    set_variable_labels(agegroup = "age group of child for vaccination")

  # Selecting children
  # Create subset of KRfile to select for children for VAC indicators
  # Select agegroup 1 or agegroup 2
  KRvac <- KRdata %>%
    subset(agegroup==1 & b5==1) # select age group and live children
  # *******************************************************************************

  # Source of vaccination information. We need this variable to code vaccination indicators by source.
  KRvac <- KRvac %>%
    mutate(source =
             case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
    set_value_labels(source = c("card" = 1, "mother"=2)) %>%
    set_variable_labels(source = "source of vaccination information")


  # *** Measles ***
  # //Measles either source
  KRvac <- KRvac %>%
    mutate(ch_meas_either =
             case_when(h9%in%c(1,2,3) ~ 1, h9%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(ch_meas_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_meas_either = "Measles vaccination according to either source")




  colnames(KRvac)[colnames(KRvac) == 'ch_meas_either'] <- "value"
  return(KRvac)
}


#PCV:Pneumococcal
#Percentage of children (age 12-23)
#ch_pneumo3_either   CH_VAC.do  KR
#Pneumococcal 3rd dose vaccination according to either source


 ch_pneumo3_either<-function(Rdata){

   # weight variable
   KRdata <- Rdata %>%
     mutate(wt = v005/1000000)

   # age of child. If b19 is not available in the data use v008 - b3
   if ("TRUE" %in% (!("b19" %in% names(KRdata))))
     KRdata [[paste("b19")]] <- NA
   if ("TRUE" %in% all(is.na(KRdata$b19)))
   { b19_included <- 0} else { b19_included <- 1}

   if (b19_included==1) {
     KRdata <- KRdata %>%
       mutate(age = b19)
   } else {
     KRdata <- KRdata %>%
       mutate(age = v008 - b3)
   }

   # *** Two age groups used for reporting.
   KRdata <- KRdata %>%
     mutate(agegroup =
              case_when(
                age>=12 & age<=23 ~ 1,
                age>=24 & age<=35 ~ 2  )) %>%
     set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
     set_variable_labels(agegroup = "age group of child for vaccination")

   # Selecting children
   # Create subset of KRfile to select for children for VAC indicators
   # Select agegroup 1 or agegroup 2
   KRvac <- KRdata %>%
     subset(agegroup==1 & b5==1) # select age group and live children

   # *******************************************************************************

   # Source of vaccination information. We need this variable to code vaccination indicators by source.
   KRvac <- KRvac %>%
     mutate(source =
              case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
     set_value_labels(source = c("card" = 1, "mother"=2)) %>%
     set_variable_labels(source = "source of vaccination information")


   # *** Pneumococcal  ***
   # //Pneumococcal 1, 2, 3 either source
   # Some surveys that do not have information on this vaccine.
   KRvac <- KRvac %>%
     mutate(Pneumo1 = case_when(h54%in%c(1,2,3) ~ 1, h54%in%c(0,8) ~ 0  )) %>%
     mutate(Pneumo2 = case_when(h55%in%c(1,2,3) ~ 1, h55%in%c(0,8) ~ 0  )) %>%
     mutate(Pneumo3 = case_when(h56%in%c(1,2,3) ~ 1, h56%in%c(0,8) ~ 0  )) %>%
     mutate(Pneumosum= Pneumo1+Pneumo2+Pneumo3)
   # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
   # See DHS guide to statistics for further explanation
   KRvac <- KRvac %>%
     mutate(ch_pneumo1_either = case_when(Pneumosum >=1 ~ 1, TRUE ~ 0  )) %>%
     set_value_labels(ch_pneumo1_either = c("Yes" = 1, "No"=0)) %>%
     set_variable_labels(ch_pneumo1_either = "Pneumococcal 1st dose vaccination according to either source") %>%
     mutate(ch_pneumo2_either = case_when(Pneumosum >=2 ~ 1, TRUE ~ 0  )) %>%
     set_value_labels(ch_pneumo2_either = c("Yes" = 1, "No"=0)) %>%
     set_variable_labels(ch_pneumo2_either = "Pneumococcal 2nd dose vaccination according to either source") %>%
     mutate(ch_pneumo3_either = case_when(Pneumosum >=3 ~ 1, TRUE ~ 0  )) %>%
     set_value_labels(ch_pneumo3_either = c("Yes" = 1, "No"=0)) %>%
     set_variable_labels(ch_pneumo3_either = "Pneumococcal 3rd dose vaccination according to either source")

   colnames(KRvac)[colnames(KRvac) == 'ch_pneumo3_either'] <- "value"
   return(KRvac)


 }

#RotaC1?
#Percentage of children (age 12-23)
#ch_rotav1_either
#Rotavirus 1st dose vaccination according to either source
 ch_rotav1_either<-function(Rdata){

   # weight variable
   KRdata <- Rdata %>%
     mutate(wt = v005/1000000)

   # age of child. If b19 is not available in the data use v008 - b3
   if ("TRUE" %in% (!("b19" %in% names(KRdata))))
     KRdata [[paste("b19")]] <- NA
   if ("TRUE" %in% all(is.na(KRdata$b19)))
   { b19_included <- 0} else { b19_included <- 1}

   if (b19_included==1) {
     KRdata <- KRdata %>%
       mutate(age = b19)
   } else {
     KRdata <- KRdata %>%
       mutate(age = v008 - b3)
   }

   # *** Two age groups used for reporting.
   KRdata <- KRdata %>%
     mutate(agegroup =
              case_when(
                age>=12 & age<=23 ~ 1,
                age>=24 & age<=35 ~ 2  )) %>%
     set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
     set_variable_labels(agegroup = "age group of child for vaccination")

   # Selecting children
   # Create subset of KRfile to select for children for VAC indicators
   # Select agegroup 1 or agegroup 2
   KRvac <- KRdata %>%
     subset(agegroup==1 & b5==1) # select age group and live children

   # *******************************************************************************

   # Source of vaccination information. We need this variable to code vaccination indicators by source.
   KRvac <- KRvac %>%
     mutate(source =
              case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
     set_value_labels(source = c("card" = 1, "mother"=2)) %>%
     set_variable_labels(source = "source of vaccination information")


   # *** Pneumococcal  ***
   # //Pneumococcal 1, 2, 3 either source
   # Some surveys that do not have information on this vaccine.
   # *** Rotavirus  ****
   # //Rotavirus 1, 2, 3 either source
   # Some surveys that do not have information on this vaccine.
   KRvac <- KRvac %>%
     mutate(rotav1 = case_when(h57%in%c(1,2,3) ~ 1, h57%in%c(0,8) ~ 0  )) %>%
     mutate(rotav2 = case_when(h58%in%c(1,2,3) ~ 1, h58%in%c(0,8) ~ 0  )) %>%
     mutate(rotav3 = case_when(h59%in%c(1,2,3) ~ 1, h59%in%c(0,8) ~ 0  )) %>%
     mutate(rotavsum= rotav1+rotav2+rotav3)
   # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
   # See DHS guide to statistics for further explanation
   KRvac <- KRvac %>%
     mutate(ch_rotav1_either = case_when(rotavsum >=1 ~ 1, TRUE ~ 0  )) %>%
     set_value_labels(ch_rotav1_either = c("Yes" = 1, "No"=0)) %>%
     set_variable_labels(ch_rotav1_either = "Rotavirus 1st dose vaccination according to either source") %>%
     mutate(ch_rotav2_either = case_when(rotavsum >=2 ~ 1, TRUE ~ 0  )) %>%
     set_value_labels(ch_rotav2_either = c("Yes" = 1, "No"=0)) %>%
     set_variable_labels(ch_rotav2_either = "Rotavirus 2nd dose vaccination according to either source") %>%
     mutate(ch_rotav3_either = case_when(rotavsum >=3 ~ 1, TRUE ~ 0  )) %>%
     set_value_labels(ch_rotav3_either = c("Yes" = 1, "No"=0)) %>%
     set_variable_labels(ch_rotav3_either = "Rotavirus 3rd dose vaccination according to either source")

   colnames(KRvac)[colnames(KRvac) == 'ch_rotav1_either'] <- "value"
   return(KRvac)


 }



#'CH_VACS_C_BAS
#'Children with all 8 basic vaccinations (age 12-23)
#'"All basic vaccinations according to either source"
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACS_C_BAS",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ch_allvac_either)
#' }
#' @export
#'


ch_allvac_either<-function(Rdata){

  # weight variable
  KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }

  # *** Two age groups used for reporting.
  KRdata <- KRdata %>%
    mutate(agegroup =
             case_when(
               age>=12 & age<=23 ~ 1,
               age>=24 & age<=35 ~ 2  )) %>%
    set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    set_variable_labels(agegroup = "age group of child for vaccination")

  # Selecting children
  # Create subset of KRfile to select for children for VAC indicators
  # Select agegroup 1 or agegroup 2
  KRvac <- KRdata %>%
    subset(agegroup==1 & b5==1) # select age group and live children

  # *** BCG ***
  # //BCG either source
  KRvac <- KRvac %>%
    mutate(ch_bcg_either =
             case_when(h2%in%c(1,2,3) ~ 1, h2%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(ch_bcg_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_bcg_either = "BCG vaccination according to either source")

  # *** Pentavalent ***
  # //DPT 1, 2, 3 either source
  KRvac <- KRvac %>%
    mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3)
  # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
  # See DHS guide to statistics for further explanation
  KRvac <- KRvac %>%
    mutate(ch_pent1_either = case_when(dptsum >=1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent1_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent1_either = "Pentavalent 1st dose vaccination according to either source") %>%
    mutate(ch_pent2_either = case_when(dptsum >=2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent2_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent2_either = "Pentavalent 2nd dose vaccination according to either source") %>%
    mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent3_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent3_either = "Pentavalent 3rd dose vaccination according to either source")


  # *** Polio ***
  # //polio 0, 1, 2, 3 either source
  KRvac <- KRvac %>%
    mutate(polio1 = case_when(h4%in%c(1,2,3) ~ 1, h4%in%c(0,8) ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c(1,2,3) ~ 1, h6%in%c(0,8) ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c(1,2,3) ~ 1, h8%in%c(0,8) ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3)
  # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
  # See DHS guide to statistics for further explanation
  KRvac <- KRvac %>%
    mutate(ch_polio0_either = case_when(h0%in%c(1,2,3) ~ 1, h0%in%c(0,8) ~ 0 )) %>%
    set_value_labels(ch_polio0_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio0_either = "Polio at birth vaccination according to either source") %>%
    mutate(ch_polio1_either = case_when(poliosum >=1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_polio1_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio1_either = "Polio 1st dose vaccination according to either source") %>%
    mutate(ch_polio2_either = case_when(poliosum >=2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_polio2_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio2_either = "Polio 2nd dose vaccination according to either source") %>%
    mutate(ch_polio3_either = case_when(poliosum >=3 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_polio3_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio3_either = "Polio 3rd dose vaccination according to either source")


  # *** Measles ***
  # //Measles either source
  KRvac <- KRvac %>%
    mutate(ch_meas_either =
             case_when(h9%in%c(1,2,3) ~ 1, h9%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(ch_meas_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_meas_either = "Measles vaccination according to either source")


  KRvac <- KRvac %>%
    mutate(ch_allvac_either =
             case_when(ch_bcg_either==1&ch_pent3_either==1&ch_polio3_either==1&ch_meas_either==1 ~ 1, TRUE ~ 0)) %>%
    set_value_labels(ch_allvac_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_allvac_either = "All basic vaccinations according to either source")


  colnames(KRvac)[colnames(KRvac) == 'ch_allvac_either'] <- "value"
  return(KRvac)


}

#'CH_VACS_C_NON
#'KR
#'Children with no vaccinations (age 12-23)
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACS_C_NON",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ch_novac_either)
#' }
#' @export
#'

ch_novac_either<-function(Rdata){


  # weight variable
  KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }

  # *** Two age groups used for reporting.
  KRdata <- KRdata %>%
    mutate(agegroup =
             case_when(
               age>=12 & age<=23 ~ 1,
               age>=24 & age<=35 ~ 2  )) %>%
    set_value_labels(agegroup = c("12-23" = 1, "24-35"=2)) %>%
    set_variable_labels(agegroup = "age group of child for vaccination")

  # Selecting children
  # Create subset of KRfile to select for children for VAC indicators
  # Select agegroup 1 or agegroup 2
  KRvac <- KRdata %>%
    subset(agegroup==1 & b5==1) # select age group and live children

  # *** BCG ***
  # //BCG either source
  KRvac <- KRvac %>%
    mutate(ch_bcg_either =
             case_when(h2%in%c(1,2,3) ~ 1, h2%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(ch_bcg_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_bcg_either = "BCG vaccination according to either source")

  # *** Pentavalent ***
  # //DPT 1, 2, 3 either source
  KRvac <- KRvac %>%
    mutate(dpt1 = case_when(h3%in%c(1,2,3) ~ 1, h3%in%c(0,8) ~ 0  )) %>%
    mutate(dpt2 = case_when(h5%in%c(1,2,3) ~ 1, h5%in%c(0,8) ~ 0  )) %>%
    mutate(dpt3 = case_when(h7%in%c(1,2,3) ~ 1, h7%in%c(0,8) ~ 0  )) %>%
    mutate(dptsum = dpt1 + dpt2 + dpt3)
  # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
  # See DHS guide to statistics for further explanation
  KRvac <- KRvac %>%
    mutate(ch_pent1_either = case_when(dptsum >=1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent1_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent1_either = "Pentavalent 1st dose vaccination according to either source") %>%
    mutate(ch_pent2_either = case_when(dptsum >=2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent2_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent2_either = "Pentavalent 2nd dose vaccination according to either source") %>%
    mutate(ch_pent3_either = case_when(dptsum >=3 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_pent3_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_pent3_either = "Pentavalent 3rd dose vaccination according to either source")


  # *** Polio ***
  # //polio 0, 1, 2, 3 either source
  KRvac <- KRvac %>%
    mutate(polio1 = case_when(h4%in%c(1,2,3) ~ 1, h4%in%c(0,8) ~ 0  )) %>%
    mutate(polio2 = case_when(h6%in%c(1,2,3) ~ 1, h6%in%c(0,8) ~ 0  )) %>%
    mutate(polio3 = case_when(h8%in%c(1,2,3) ~ 1, h8%in%c(0,8) ~ 0  )) %>%
    mutate(poliosum=polio1 + polio2 + polio3)
  # This step is performed for multi-dose vaccines to take care of any gaps in the vaccination history.
  # See DHS guide to statistics for further explanation
  KRvac <- KRvac %>%
    mutate(ch_polio0_either = case_when(h0%in%c(1,2,3) ~ 1, h0%in%c(0,8) ~ 0 )) %>%
    set_value_labels(ch_polio0_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio0_either = "Polio at birth vaccination according to either source") %>%
    mutate(ch_polio1_either = case_when(poliosum >=1 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_polio1_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio1_either = "Polio 1st dose vaccination according to either source") %>%
    mutate(ch_polio2_either = case_when(poliosum >=2 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_polio2_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio2_either = "Polio 2nd dose vaccination according to either source") %>%
    mutate(ch_polio3_either = case_when(poliosum >=3 ~ 1, TRUE ~ 0  )) %>%
    set_value_labels(ch_polio3_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_polio3_either = "Polio 3rd dose vaccination according to either source")


  # *** Measles ***
  # //Measles either source
  KRvac <- KRvac %>%
    mutate(ch_meas_either =
             case_when(h9%in%c(1,2,3) ~ 1, h9%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(ch_meas_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_meas_either = "Measles vaccination according to either source")




  # *** No vaccinations ***
  KRvac <- KRvac %>%
    mutate(ch_novac_either =
             case_when(ch_bcg_either==0&ch_pent1_either==0&ch_pent2_either==0&ch_pent3_either==0&
                         ch_polio0_either==0&ch_polio1_either==0&ch_polio2_either==0&ch_polio3_either==0&
                         ch_meas_either==0 ~ 1,
                       TRUE ~ 0)) %>%
    set_value_labels(ch_novac_either = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_novac_either = "No vaccinations according to either source")



  colnames(KRvac)[colnames(KRvac) == 'ch_novac_either'] <- "value"
  return(KRvac)
}





################
##Nutrition  ch11
################

#'CN_NUTS_C_HA2
#'stunting
#'Children stunted
#'NT_CH_NUT.do PR "Stunted child under 5 years"
#'Stunting rate (Prevalence of stunted (HAZ < -2) children under five (0-59 months))
#'Percentage of children under age five stunted (below -2 standard deviations of height-for-age according to the WHO standard).
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CN_NUTS_C_HA2",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::nt_ch_stunt)
#' }
#' @export
#'


nt_ch_stunt<-function(Rdata){
  PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)
  PRdata <- PRdata %>%
    mutate(nt_ch_stunt =
             case_when(
               hv103==1 &  hc70< -200  ~ 1 ,
               hv103==1 &  hc70>= -200 ~ 0 ,
               hc70>=9996 ~ 99)) %>%
    replace_with_na(replace = list(nt_ch_stunt = c(99))) %>%
    set_value_labels(nt_ch_stunt = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_ch_stunt = "Stunted child under 5 years")

  colnames(PRdata)[colnames(PRdata) == 'nt_ch_stunt'] <- "value"
  return(PRdata)
}


#' CN_NUTS_C_WH2
#' wasting
#' Children wasted
#' NT_CH_NUT.do PR "Wasted child under 5 years"
#'Wasting rate (Prevalence of wasted (HAZ < -2) children under five (0-59 months))
#'Percentage of children under age five with a weight-for-height z-score (WHZ) more than two standard deviations below the median WHO growth standards.
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CN_NUTS_C_WH2",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::nt_ch_wast)
#' }
#' @export
#'

nt_ch_wast<-function(Rdata){
    PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)
  PRdata <- PRdata %>%
    mutate(nt_ch_wast =
             case_when(
               hv103==1 &  hc72< -200  ~ 1 ,
               hv103==1 &  hc72>= -200 ~ 0 ,
               hc72>=9996 ~ 99)) %>%
    replace_with_na(replace = list(nt_ch_wast = c(99))) %>%
    set_value_labels(nt_ch_wast = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_ch_wast = "Wasted child under 5 years")

    colnames(PRdata)[colnames(PRdata) == 'nt_ch_wast'] <- "value"
    return(PRdata)
}



#'AN_ANEM_W_ANY
#'womananemia
#'nt_wm_any_anem "Any anemia - women"  NT_WM_NUT.do
#'Percentage of women aged 15-49 classified as having any anemia
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "AN_ANEM_W_ANY",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::nt_wm_any_anem)
#' }
#' @export
#'

nt_wm_any_anem <- function(Rdata){

  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  IRdata <- IRdata %>%
    mutate(nt_wm_any_anem =
             case_when(
               v042==1 & v457<4 ~ 1 ,
               v042==1 &  v455==0 ~ 0)) %>%
    set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_wm_any_anem = "Any anemia - women")

  colnames(IRdata)[colnames(IRdata) == 'nt_wm_any_anem'] <- "value"
  return(IRdata)
}

#'CN_BRFS_C_EXB
#' Children exclusively breastfed
#' NT_IYCF.do KR "Exclusively breastfed - last-born under 6 months"
#' Children exclusively breastfed (Prevalence of exclusive breastfeeding of children under six months of age)
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CN_BRFS_C_EXB",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::nt_ebf)
#' }
#' @export
#'

nt_ebf<- function(Rdata){

  KRdata=Rdata
  # Calculate age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(KRdata))))
    KRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(KRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    KRdata <- KRdata %>%
      mutate(age = b19)
  } else {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  }

  KRiycf <- KRdata %>%
    subset(age < 24 & b9==0) %>% # children under 24 months living at home
    arrange(caseid, bidx) %>% # make sure the data is sorted
    subset(is.na(lag(caseid)) | caseid!=lag(caseid)) # select just the youngest

  KRiycf <- KRiycf %>%
    mutate(wt = v005/1000000)

  # *** Breastfeeding and complemenatry feeding ***
  #
  # //currently breastfed
  KRiycf <- KRiycf %>%
    mutate(nt_bf_curr =
             case_when(
               m4==95  ~ 1 ,
               m4 %in% c(93,94,98,99) ~ 0)) %>%
    set_value_labels(nt_bf_curr = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_bf_curr = "Currently breastfeeding - last-born under 2 years")

  # //breastfeeding status
  KRiycf <- KRiycf %>%
    mutate(water  = case_when(v409==1  ~ 1 , v409!=1 ~ 0)) %>%
    mutate(liquids= case_when(v409a==1 | v410==1 | v410a==1 | v412c==1 | v413==1 | v413a==1 | v413b==1 | v413c==1 | v413d==1  ~ 1 ,
                              v409a!=1 | v410!=1 | v410a!=1 | v412c!=1 | v413!=1 | v413a!=1 | v413b!=1 | v413c!=1 | v413d!=1 ~ 0)) %>%
    mutate(milk   = case_when(v411==1 | v411a==1 ~ 1 , v411!=1 | v411a!=1 ~ 0)) %>%
    mutate(solids = case_when(v414a==1 | v414b==1 | v414c==1 | v414d==1 | v414e==1 | v414f==1 | v414g==1 | v414h==1 | v414i==1 |
                                v414j==1 | v414k==1 | v414l==1 | v414m==1 | v414n==1 | v414o==1 | v414p==1 | v414q==1 | v414r==1 |
                                v414s==1 | v414t==1 | v414u==1 | v414v==1 | v414w==1 | v412a==1 | v412b==1 | m39a==1 ~ 1 ,
                              v414a!=1 | v414b!=1 | v414c!=1 | v414d!=1 | v414e!=1 | v414f!=1 | v414g!=1 | v414h!=1 | v414i!=1 |
                                v414j!=1 | v414k!=1 | v414l!=1 | v414m!=1 | v414n!=1 | v414o!=1 | v414p!=1 | v414q!=1 | v414r!=1 |
                                v414s!=1 | v414t!=1 | v414u!=1 | v414v!=1 | v414w!=1 | v412a!=1 | v412b!=1 | m39a!=1~ 0) ) %>%
    mutate(nt_bf_status = case_when(nt_bf_curr==0 ~ 0, solids==1 ~ 5, milk==1 ~ 4, liquids==1 ~3, water==1 ~2, TRUE~1 )) %>%
    set_value_labels(nt_bf_status = c("not bf"=0, "exclusively bf"=1, "bf & plain water"=2, "bf & non-milk liquids"=3, "bf & other milk"=4, "bf & complemenatry foods"=5 )) %>%
    set_variable_labels(nt_bf_status = "Breastfeeding status for last-born child under 2 years")
  # //exclusively breastfed
  KRiycf <- KRiycf %>%
    mutate(nt_ebf =
             case_when(
               age<6 & nt_bf_status==1  ~ 1 ,
               age<6 & nt_bf_status!=1 ~ 0)) %>%
    set_value_labels(nt_ebf = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_ebf = "Exclusively breastfed - last-born under 6 months")
colnames(KRiycf)[colnames(KRiycf) == 'nt_ebf'] <- "value"
  return(KRiycf)
}


#'CN_ANMC_C_ANY
#'Children with any anemia
#'"Any anemia - child 6-59 months" PR NT_CH_NUT.do
#' Children under five with any anemia
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CN_ANMC_C_ANY",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::nt_ch_any_anem)
#' }
#' @export


nt_ch_any_anem<-function(Rdata){

  PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)

PRdata <- PRdata %>%
  mutate(nt_ch_any_anem =
           case_when(
             hv103==1 & hc1>5 & hc1<60 & hc56<110 ~ 1 ,
             hv103==1 & hc1>5 & hc1<60 & hc56>=110 ~ 0)) %>%
  set_value_labels(nt_ch_any_anem = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_any_anem = "Any anemia - child 6-59 months")
colnames(PRdata)[colnames(PRdata) == 'nt_ch_any_anem'] <- "value"
return(PRdata)
}

#' AN_NUTS_W_THN
#' Women who are thin according to BMI (<18.5)
#' NT_WM_NUT.do "Thin BMI - women" IR !!!!!!!!
#' Underweight (Prevalence of underweight (BMI < 18.5) women of reproductive age)
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "AN_NUTS_W_THN",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::nt_wm_thin)
#' }
#' @export
#'

nt_wm_thin<-function(Rdata){

  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)
  # * age of most recent child
  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata$b19_01)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }

  # //Thin
  IRdata <- IRdata %>%
    mutate(nt_wm_thin =
             case_when(
               v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
               v445>= 1850   ~ 0,
               v445>=1200 & v445<1850  ~ 1 )) %>%
    replace_with_na(replace = list(nt_wm_thin = c(99))) %>%
    set_value_labels(nt_wm_thin = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_wm_thin = "Thin BMI - women")

colnames(IRdata)[colnames(IRdata) == 'nt_wm_thin'] <- "value"
return(IRdata)
}

################
##Malaria  ch12
################

#'ML_ITNA_P_ACC
#'Households with at least one insecticide-treated mosquito net (ITN) for every two persons who stayed in the household the previous night
#'Persons with access to an insecticide-treated mosquito net (ITN)
#' ML_NETS_HH.do  HR
#'Households with >1 ITN per 2 household members
#'Percentage of households with at least one ITN for every 2 persons who stayed in the household last night
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ML_ITNA_P_ACC",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ml_hhaccess)
#' }
#' @export
#'

ml_hhaccess<-function(Rdata){
    # Number of ITNs per household
  HRdata <- Rdata %>%
    mutate(itnhh_01 = case_when(hml10_1==1 ~ 1,TRUE ~ 0)) %>%
    mutate(itnhh_02 = case_when(hml10_2==1 ~ 1,TRUE ~ 0)) %>%
    mutate(itnhh_03 = case_when(hml10_3==1 ~ 1,TRUE ~ 0)) %>%
    mutate(itnhh_04 = case_when(hml10_4==1 ~ 1,TRUE ~ 0)) %>%
    mutate(itnhh_05 = case_when(hml10_5==1 ~ 1,TRUE ~ 0)) %>%
    mutate(itnhh_06 = case_when(hml10_6==1 ~ 1,TRUE ~ 0)) %>%
    mutate(itnhh_07 = case_when(hml10_7==1 ~ 1,TRUE ~ 0)) %>%
    mutate(ml_numitnhh = itnhh_01 + itnhh_02 + itnhh_03 + itnhh_04 + itnhh_05 + itnhh_06 + itnhh_07,
           ml_numitnhh = set_label(ml_numitnhh, label = "Number of ITNs per household"))

  # Households with > 1 ITN per 2 members
  HRdata <- HRdata %>%
    mutate(ml_potuse = ml_numitnhh*2,
           ml_potuse = set_label(ml_potuse, label = "Potential ITN users in household"))

  HRdata <- HRdata %>%
    mutate(ml_hhaccess0 =ml_potuse/hv013) %>%
    mutate(ml_hhaccess = case_when(
      hv013==0 ~ 99,
      ml_hhaccess0 >= 1   ~ 1,
      TRUE   ~ 0),
      ml_hhaccess = set_label(ml_hhaccess, label = "Households with >1 ITN per 2 household members"))%>%
    replace_with_na(replace = list(ml_hhaccess = c(99)))

  colnames(HRdata)[colnames(HRdata) == 'ml_hhaccess'] <- "value"
  return(HRdata)
}

#Percentage of children age 6-59 months classified as having malaria according to a rapid diagnostic test (RDT).
#ML_PMAL_C_RDT
#Malaria prevalence according to RDT
#ML_BIOMARKERS.do  PR   Tested for Parasitemia (via RDT) in children 6-59 months
 ml_test_rdtmal<-function(Rdata){
   # Tested for Parasitemia via RDT
   PRdata <- Rdata %>%
     mutate(ml_test_rdtmal = case_when(
       hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & !(hml35==0 | hml35==1)  ~ 0,
       hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml35==0 | hml35==1)  ~ 1),
       ml_test_rdtmal = set_label(ml_test_rdtmal, label = "Tested for Parasitemia (via RDT) in children 6-59 months"))

   colnames(PRdata)[colnames(PRdata) == 'ml_test_rdtmal'] <- "value"
   return(PRdata)
 }

################
##HIV Prevlence ch 14
################

#HIV prevalence among women
#
 # HV_PREV.do IR+AR

#'HA_HIVP_W_HIV
#'hv_hiv_pos
#'"HIV positive test result"
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "HA_HIVP_W_HIV",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::hv_hiv_pos)
#' }
#' @export


hv_hiv_pos<-function(Rdata){
    IRdata <- Rdata$`Individual Recode`
    MRdata <- Rdata$`Men's Recode`
    ARdata<- Rdata$`HIV Test Results Recode`

    # A merge of the IR and MR files with the AR file is needed to produce the Total HIV prevalence and present them by background variables present in the IR and MR files
    # The following merge sequence will produce an IRMRARmerge file for the survey of interest

    # merge AR file to IR file
    temp <- ARdata

    temp[["v001"]] <- temp[["hivclust"]]
    temp[["v002"]] <- temp[["hivnumb"]]
    temp[["v003"]] <- temp[["hivline"]]


    # merge
    IRARtemp <- merge(IRdata, temp, by = c("v001", "v002", "v003"), all = FALSE)
    IRARtemp <- IRARtemp %>% mutate(sex = 2)

    # merge AR file to MR file
    temp <- ARdata

    temp[["mv001"]] <- temp[["hivclust"]]
    temp[["mv002"]] <- temp[["hivnumb"]]
    temp[["mv003"]] <- temp[["hivline"]]

    # merge
    MRARtemp <- merge(MRdata, temp, by = c("mv001", "mv002", "mv003"), all = FALSE)
    MRARtemp <- MRARtemp %>% mutate(sex = 1)

    # append IRARtemp and MRARtemp

    # IMPORTANT! we are renaming all mv* variables to v* variables.
    names(MRARtemp) <- stringr::str_replace_all(names(MRARtemp),"mv","v")

    IRMRARmerge <- suppressWarnings(bind_rows(IRARtemp,MRARtemp))

    # limiting to age 15-49, you can comment this out if you want all men
    IRMRARmerge <- IRMRARmerge %>%
      filter(!v012>49)



   IRMRARmerge <- IRMRARmerge %>%
     mutate(hv_hiv_pos = case_when(
       hiv03==1  ~ 1,
       TRUE ~ 0),
       hv_hiv_pos = add_labels(hv_hiv_pos, labels = c("No"=0, "Yes"=1)),
       hv_hiv_pos = set_label(hv_hiv_pos, label = "HIV positive test result"))

   colnames(IRMRARmerge)[colnames(IRMRARmerge) == 'hv_hiv_pos'] <- "value"
   return(IRMRARmerge)
 }
#hv_hiv1_pos
#hv_hiv2_pos
#hv_hiv1or2_pos



################
# Water, Sanitation and Hygiene ch16 DHS8 ch2 github
################

 sanitation_adj<-function(WASHdata){

   # Generate type of sanitation facility

   #   NOTE: this cycles through ALL country specific coding and ends around line 1495.
   #   Surveys are specified through their country code [hv000] and year [hv007] or month [hv006] when   necessary.



   # create a variable for sanitation type, this var will be overwritten if country-specific coding is needed
   WASHdata <- WASHdata %>% mutate(ph_sani_type = hv205)


   # 	recode country-specific responses to standard codes ------------------------

   if (WASHdata$hv000[1]=="AF7")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==43 ~ 23,
       hv205==44 ~ 51,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="AM4" & WASHdata$hv007[1]==2000) {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="AM4" & WASHdata$hv007[1]==2004) {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==41 ~ 42,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="AM6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==44 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="AO7")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 14,
       hv205==14 ~ 11,
       hv205==15 ~ 12,
       hv205==16 ~ 14,
       hv205==17 ~ 11,
       hv205==18 ~ 12,
       hv205==19 ~ 14,
       hv205==22 ~ 21,
       hv205==24 ~ 21,
       hv205==25 ~ 21,
       hv205==26 ~ 23,
       hv205==27 ~ 21,
       hv205==28 ~ 21,
       hv205==29 ~ 23,
       hv205==42 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for 4 surveys: three that all have hv000=BD3 (BDHR31, BDHR3A, and BDHR41) and one that is BD4
   if (WASHdata$hv000[1] %in% c("BD3", "BD4")) {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 12,
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       hv205==24 ~ 43,
       TRUE ~ hv205
     )) }
   # same recode for 3 surveys: BFHR21, BFHR31, and BFHR43. BFHR31 and BFHR43 do not have a 41 category for hv205 and BFHR43 does not have a 12 category.
   if (WASHdata$hv000[1] %in% c("BF2", "BF3", "BF4")){
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BJ3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BJ4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BJ5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==15 ~ 14,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]<95)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==13 ~ 12,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]<98)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BO4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BR2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BR3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 14,
       hv205==13 ~ 14,
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="BU6" & WASHdata$hv007[1] %in% c(2012,2013))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==23 ~ 22,
       hv205==24 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CD5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==23 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CF3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CG5" & WASHdata$hv007[1]==2005)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CG5" & WASHdata$hv007[1]==2009)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CI3" & WASHdata$hv007[1]==94)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==23 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CI3" & WASHdata$hv007[1]>97)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CI5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CM2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 42,
       hv205==22 ~ 23,
       hv205==32 ~ 31,
       hv205==33 ~ 31,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CM3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CM4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CO2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CO3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 13,
       hv205==13 ~ 14,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]==2000)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 13,
       hv205==13 ~ 14,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]==2004)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 14,
       hv205==21 ~ 23,
       hv205==22 ~ 31,
       TRUE ~ hv205
     )) }
   # same recode for two surveys COHR61 and COHR72
   if (WASHdata$hv000[1]=="CO5" | (WASHdata$hv000[1]=="CO7"  & WASHdata$hv007[1]>=2015))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 14,
       hv205==21 ~ 23,
       hv205==22 ~ 31,
       TRUE ~ hv205
     )) }

   if (WASHdata$hv000[1]=="DR2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==96)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       hv205==23 ~ 22,
       hv205==24 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==99)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for 3 surveys: DRHR4B, DRHR52, and DRHR5A
   if (WASHdata$hv000[1] %in% c("DR4", "DR5"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       hv205==23 ~ 22,
       hv205==24 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="EG2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 15,
       hv205==14 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for 4 surveys: EGHR33, EGHR42, EGHR4A, and EGHR51. Only EGHR51 has category 32 for hv205
   if (WASHdata$hv000[1] %in% c("EG3", "EG4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="EG5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="EG6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==16 ~ 12,
       hv205==17 ~ 11,
       hv205==18 ~ 11,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1992)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1997)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 41,
       hv205==25 ~ 42,
       hv205==26 ~ 43,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GA3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for three surveys: GHHR31, GHHR41, and GHHR4B. Only GHHR4B has category 32 for hv205 and only GHHR41 has category 23.
   if (WASHdata$hv000[1] %in% c("GH2", "GH3", "GH4")) {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GH7" & WASHdata$hv007[1]==2019)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==16 ~ 51,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GN3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==31 ~ 23,
       hv205==41 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GN4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==23 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]==95)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]>98)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 11,
       hv205==13 ~ 12,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GU6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 14,
       hv205==14 ~ 15,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="GY4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==61 ~ 43,
       TRUE ~ hv205
     )) }
   # same recode for two surveys HNHR52 and HNHR62
   if (WASHdata$hv000[1] %in% c("HN5", "HN6"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 15,
       hv205==21 ~ 51,
       hv205==22 ~ 41,
       hv205==24 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="HT3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==23 ~ 21,
       hv205==24 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="HT4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="HT5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==51 ~ 42,
       hv205==61 ~ 43,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="HT6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==44 ~ 43,
       hv205==45 ~ 51,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: IAHR23 and IAHR42. Only IAHR23 has category 41 for hv205
   if (WASHdata$hv000[1] %in% c("IA2", "IA3"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: IAHR52 (2005-2006), IAHR74 (2015-16), and IAHR7E (2019-21)
   if (WASHdata$hv000[1] %in% c("IA5", "IA6", "IA7"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==44 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ID4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 12,
       hv205==12 ~ 14,
       hv205==21 ~ 15,
       hv205==41 ~ 23,
       hv205==51 ~ 31,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: IDHR51 and IDHR63. Only IDHR51 has categories 33 to 36 for hv205
   if (WASHdata$hv000[1] %in% c("ID5", "ID6"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 12,
       hv205==12 ~ 14,
       hv205==13 ~ 15,
       hv205==21 ~ 23,
       hv205==32 ~ 31,
       hv205==33 ~ 31,
       hv205==34 ~ 31,
       hv205==35 ~ 31,
       hv205==36 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ID7" & WASHdata$hv007[1]==2017)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==16 ~ 14,
       hv205==17 ~ 15,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: JOHR31 and JOHR42. Only JOHR42 has category 22 for hv205
   if (WASHdata$hv000[1] %in% c("JO3","JO4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: JOHR51 and JOHR61 both are hv000=JO5
   if (WASHdata$hv000[1]=="JO5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==12 ~ 13,
       hv205==13 ~ 14,
       TRUE ~ hv205
     )) }
   # same recode for three surveys: KEHR33, KEHR3A, and KEHR42. Only KEHR33 has category 41 for hv205 and there is no category 12 in KEHR42
   if (WASHdata$hv000[1] %in% c("KE2", "KE3", "KE4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="KH4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 12,
       hv205==12 ~ 14,
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for two surveys KKHR31 and KKHR42, both are hv000=KK3. Only KKHR31 has categories 12 and 22 for hv205
   if (WASHdata$hv000[1]=="KK3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="KM3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="KY3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="LS4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MA2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MA4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==32 ~ 42,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MB4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==31 ~ 41,
       hv205==41 ~ 42,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: MDHR21 and MDHR31. Only MDHR21 has categories 12, 23, and 41 for hv205
   if (WASHdata$hv000[1] %in% c("MD2", "MD3"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==23 ~ 22,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MD4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MD5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: MDHR61 and MDHR6A both are hv000=MD6
   if (WASHdata$hv000[1]=="MD6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==23 ~ 22,
       hv205==24 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ML3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: MLHR41 and MLHR53
   if (WASHdata$hv000[1]=="ML4" | (WASHdata$hv000[1]=="ML5" & WASHdata$hv007[1]==2006))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for 3 surveys: MWHR22, MWHR41, and MWHR4E. Only MWHR22 has categories 12 and 41 for hv205
   if (WASHdata$hv000[1] %in% c("MW2", "MW4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MW5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MZ3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MZ4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 31,
       hv205==30 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MZ6" & WASHdata$hv007[1]==2011)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==16 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="MZ6" & WASHdata$hv007[1]==2015)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: NCHR31 and NCHR41. NCHR31 does not have category 32 for hv205 and NCHR41 does not have category 24 for hv205
   if (WASHdata$hv000[1] %in% c("NC3", "NC4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==15 ~ 14,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==24 ~ 43,
       hv205==30 ~ 31,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: NGHR41 and NGHR4B. NGHR41 does not have category 32 for hv205 and NGHR4B does not have categories 12 and 23
   if (WASHdata$hv000[1] %in% c("NG3", "NG4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==23 ~ 42,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NI2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NI3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NI5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NM2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==23 ~ 42,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NM4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 22,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==23 ~ 42,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NP3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==31 ~ 42,
       hv205==32 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NP4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="NP8")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==44 ~ 42,
       hv205==45 ~ 51,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PE2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==23 ~ 22,
       hv205==24 ~ 22,
       hv205==25 ~ 23,
       hv205==26 ~ 23,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PE3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==14 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==32 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PE4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==13 ~ 15,
       hv205==14 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==30 ~ 31,
       hv205==41 ~ 31,
       TRUE ~ hv205
     )) }
   # same recode for five surveys: PEHR51, PEHR5I, PEHR61, PEHR61, and PEHR6I
   if (WASHdata$hv000[1] %in% c("PE5", "PE6"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==22 ~ 12,
       hv205==24 ~ 31,
       hv205==32 ~ 31,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PH2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 23,
       hv205==24 ~ 43,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: PHHR3B and PHHR41. Only PHHR3B has category 30 for hv205 and only PHHR41 has category 32
   if (WASHdata$hv000[1] %in% c("PH3", "PH4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 13,
       hv205==22 ~ 23,
       hv205==30 ~ 31,
       hv205==31 ~ 43,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PH6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==51 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PH7")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==71 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PK2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 15,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="PK5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 14,
       hv205==14 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="RW2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 15,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   # same recode for three surveys: RWHR41, RWHR53, and RWHR5A
   if (WASHdata$hv000[1] %in% c("RW4", "RW5"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="SL5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==71 ~ 31,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: SNHR21 and SNHR32 both are hv000=SN2 . Only survey SNHR32 has category 41 for hv205
   if (WASHdata$hv000[1]=="SN2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="SN4")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 14,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for 6 surveys: SNHR61, SNHR6D, SNHR6R, SNHR7H, SNHR7I, and SNHRG0. All are hv000=SN6.
   if (WASHdata$hv000[1]=="SN6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 22,
       hv205==26 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="SZ5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: TDHR31 and TDHR41
   if (WASHdata$hv000[1] %in% c("TD3", "TD4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="TD6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 12,
       hv205==12 ~ 13,
       hv205==13 ~ 14,
       hv205==14 ~ 15,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="TG3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==21 ~ 22,
       hv205==22 ~ 23,
       hv205==23 ~ 12,
       hv205==24 ~ 22,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="TR2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 13,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: TRHR41 and TRHR4A. Only survey TRHR41 has category 12 for hv205
   if (WASHdata$hv000[1] %in% c("TR3", "TR4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 13,
       TRUE ~ hv205
     )) }
   # same recode for 5 surveys: TZHR21, TZHR3A, TZHR41, TZHR4A, and TZHR4I. Only surveys TZHR21 and TZHR3A have category 12 for hv205
   if (WASHdata$hv000[1] %in% c("TZ2", "TZ3", "TZ4") | (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2003, 2004)))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2007,2008))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 23,
       TRUE ~ hv205
     )) }
   # same recode for three surveys: TZHR6A, TZHR7B, and TZHR7I
   if (WASHdata$hv000[1] %in% c("TZ6", "TZ7"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 22,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: UGHR33 and UGHR41. Only UGHR33 has category 12 for hv205
   if (WASHdata$hv000[1] %in% c("UG3", "UG4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: UGHR52 and UGHR5A.
   if (WASHdata$hv000[1]=="UG5") {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==22 ~ 23,
       hv205==23 ~ 22,
       hv205==24 ~ 23,
       hv205==25 ~ 22,
       TRUE ~ hv205
     )) }
   # There are multiple surveys with country code UG6 in the year 2011 (UGHR6A and UGHR61), so need to specify maximum month of interview. UGHR61 had interviews up until hv006 (month) = 12. UGHR6A has no survey specific coding necessary.
   if (WASHdata$hv000[1]=="UG6" & WASHdata$hv007[1]==2011 & max(WASHdata$hv006==12))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==22 ~ 23,
       hv205==23 ~ 22,
       hv205==24 ~ 23,
       hv205==25 ~ 22,
       hv205==44 ~ 51,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="UG6" & WASHdata$hv007[1] %in% c(2014,2015))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 22,
       hv205==25 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="UZ3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   # same recode for two surveys VNHR31 and VNHR41. Both are hv000=VNT
   if (WASHdata$hv000[1]=="VNT")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="VN5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="YE2")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==13 ~ 11,
       hv205==14 ~ 15,
       hv205==24 ~ 14,
       hv205==25 ~ 23,
       hv205==26 ~ 15,
       hv205==31 ~ 42,
       hv205==32 ~ 31,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="YE6")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==24 ~ 23,
       hv205==25 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ZA3")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 42,
       hv205==22 ~ 23,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ZA7" & WASHdata$hv007[1]==2016)  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==23 ~ 21,
       hv205==44 ~ 51,
       TRUE ~ hv205
     )) }
   # same recode for three surveys: ZMHR21, ZMHR31, and ZMHR42. Only survey ZMHR21 has category 41 for hv025 and survey ZMHR42 does not have categories 41 or 12.
   if (WASHdata$hv000[1] %in% c("ZM2", "ZM3", "ZM4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       hv205==41 ~ 96,
       TRUE ~ hv205
     )) }
   # same recode for two surveys: ZWHR31 and ZWHR42. Only survey ZWHR31 has category 12 for hv205.
   if (WASHdata$hv000[1] %in% c("ZW3", "ZW4"))  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==11 ~ 15,
       hv205==12 ~ 15,
       hv205==21 ~ 23,
       hv205==22 ~ 21,
       TRUE ~ hv205
     )) }
   if (WASHdata$hv000[1]=="ZW5")  {
     WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
       hv205==91 ~ 41,
       hv205==92 ~ 42,
       TRUE ~ hv205
     )) }



   # End of country specific codes
   return(WASHdata)

 }

#'WS_TLET_H_IMP
#'Percentage of households using an improved sanitation facility
#'PH_SANI.do  PR
#'ph_sani_improve "Access to improved sanitation" country-specific
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "WS_TLET_H_IMP",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ph_sani_improve)
#' }
#' @export
#'

 ph_sani_improve<-function(Rdata){
   WASHdata <- Rdata #same code can be used for PR or HR files, but must be specified here

   WASHdata<-sanitation_adj(WASHdata)

   # Tested for Parasitemia via RDT
   WASHdata <- WASHdata %>% mutate(ph_sani_type = case_when(
     is.na(ph_sani_type) ~ 99,
     TRUE ~ ph_sani_type)) %>%
     set_value_labels(ph_sani_type =
                        c("flush - to piped sewer system" = 11,
                          "flush - to septic tank"	= 12,
                          "flush - to pit latrine"	= 13,
                          "flush - to somewhere else" = 14,
                          "flush - don't know where/unspecified" = 15,
                          "pit latrine - ventilated improved pit (vip)" = 21,
                          "pit latrine - with slab" = 22,
                          "pit latrine - without slab / open pit" = 23,
                          "no facility/bush/field/river/sea/lake" = 31,
                          "composting toilet" = 41,
                          "bucket toilet" = 42,
                          "hanging toilet/latrine" = 43,
                          "other improved" = 51,
                          "other" = 96,
                          "missing" = 99)) %>%
     set_variable_labels(ph_sani_type = "Type of sanitation")


   # create improved sanitation indicator
   WASHdata <- WASHdata %>% mutate(ph_sani_improve = case_when(
     ph_sani_type %in% c(11, 12, 13, 15, 21, 22, 41, 51) ~ 1,
     ph_sani_type %in% c(14, 23, 42, 43, 96) ~ 2,
     ph_sani_type ==31 ~ 3,
     ph_sani_type ==99 ~ NA)) %>%
     set_value_labels(ph_sani_improve =
                        c("improved sanitation" = 1,
                          "unimproved sanitation" = 2,
                          "open defecation" = 3)) %>%
     set_variable_labels(ph_sani_improve = "Improved sanitation")


   WASHdata$ph_sani_improve= ifelse(  WASHdata$ph_sani_improve == 1, 1, 0)


   colnames(WASHdata)[colnames(WASHdata) == 'ph_sani_improve'] <- "value"
   return(WASHdata)
 }


#'WS_TLET_P_BAS
#'Population with access to a basic sanitation service WS_TLET_P_BAS in DHS API
#'PH_SANI.do  PR
#'ph_sani_basic "Basic sanitation facility"
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "WS_TLET_P_BAS",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ph_sani_basic)
#' }
#' @export
#'

 ph_sani_basic<-function(Rdata){
   WASHdata <- Rdata #same code can be used for PR or HR files, but must be specified here

   WASHdata<-sanitation_adj(WASHdata)

   # Tested for Parasitemia via RDT
   WASHdata <- WASHdata %>% mutate(ph_sani_type = case_when(
     is.na(ph_sani_type) ~ 99,
     TRUE ~ ph_sani_type)) %>%
     set_value_labels(ph_sani_type =
                        c("flush - to piped sewer system" = 11,
                          "flush - to septic tank"	= 12,
                          "flush - to pit latrine"	= 13,
                          "flush - to somewhere else" = 14,
                          "flush - don't know where/unspecified" = 15,
                          "pit latrine - ventilated improved pit (vip)" = 21,
                          "pit latrine - with slab" = 22,
                          "pit latrine - without slab / open pit" = 23,
                          "no facility/bush/field/river/sea/lake" = 31,
                          "composting toilet" = 41,
                          "bucket toilet" = 42,
                          "hanging toilet/latrine" = 43,
                          "other improved" = 51,
                          "other" = 96,
                          "missing" = 99)) %>%
     set_variable_labels(ph_sani_type = "Type of sanitation")

   # create improved sanitation indicator
   WASHdata <- WASHdata %>% mutate(ph_sani_improve = case_when(
     ph_sani_type %in% c(11, 12, 13, 15, 21, 22, 41, 51) ~ 1,
     ph_sani_type %in% c(14, 23, 42, 43, 96) ~ 2,
     ph_sani_type ==31 ~ 3,
     ph_sani_type ==99 ~ NA)) %>%
     set_value_labels(ph_sani_improve =
                        c("improved sanitation" = 1,
                          "unimproved sanitation" = 2,
                          "open defecation" = 3)) %>%
     set_variable_labels(ph_sani_improve = "Improved sanitation")

   # NOTE: an older definition of improved sanitation checked to see if there was a shared toilet [hv225==1]


   # create basic or limited sanitation services indicator - hv225 may not exist
   WASHdata <- WASHdata %>%
     mutate(ph_sani_basic = case_when(
       ph_sani_improve==1 & hv225==0 ~ 1,
       ph_sani_improve==1 & hv225==1 ~ 2,
       ph_sani_improve==2 ~ 3,
       ph_sani_improve==3 ~ 4)) %>%
     set_value_labels(ph_sani_basic =
                        c("basic sanitation" = 1,
                          "limited sanitation" = 2,
                          "unimproved sanitation" = 3,
                          "open defecation" = 4)) %>%
     set_variable_labels(ph_sani_basic = "Basic or limited sanitation")



   WASHdata$ph_sani_basic= ifelse(  WASHdata$ph_sani_basic == 1, 1, 0)


   colnames(WASHdata)[colnames(WASHdata) == 'ph_sani_basic'] <- "value"
   return(WASHdata)
 }


#'WS_SRCE_P_BAS
#'Population using a basic water source
#' PH_WATER.do
#'ph_wtr_basic "Basic water service" PR
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "WS_SRCE_P_BAS",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::watersource_adj)
#' }
#' @export
#'

 watersource_adj<-function(Rdata){

   WASHdata=Rdata

   # generate water source indicator ----------------------------------------------

   # create a variable for water source, this var will be overwritten if country-specific coding is needed
   WASHdata <- WASHdata %>% mutate(ph_sani_type = hv205)


   # country-specific coding ------------------------------------------------------
   if (WASHdata$hv000[1]=="AF7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="AL7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="AM4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="AM4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==32 ~ 41,
       hv201==41 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="AM6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 11,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="AM7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="AO7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==33 ~ 21,
       hv201==63 ~ 62,
       TRUE ~ hv201
     )) }
   # same recode for 3 surveys that all have hv000=BD3 (BDHR31, BDHR3A, and BDHR41). BDHR41 does not have category 41 for hv201
   if (WASHdata$hv000[1]=="BD3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==31 ~ 43,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BD4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==23 ~ 31,
       hv201==24 ~ 32,
       hv201==41 ~ 43,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BD7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BF2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BF3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 11,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==51 ~ 71,
       hv201==61 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BF4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BF7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BJ3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==31 ~ 41,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==42 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BJ4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==52 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BJ5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==52 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BJ7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]<95)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==13 ~ 14,
       hv201==21 ~ 30,
       hv201==32 ~ 43,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BO3" & WASHdata$hv007[1]==98)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 15,
       hv201==21 ~ 30,
       hv201==31 ~ 43,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BO4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 15,
       hv201==22 ~ 32,
       hv201==42 ~ 43,
       hv201==45 ~ 14,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BO5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BR2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BR3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="BU7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CD5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==34 ~ 32,
       hv201==35 ~ 32,
       hv201==36 ~ 32,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CF3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CG5" & WASHdata$hv007[1]==2005)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: CIHR35 and CIHR3A both are hv000=CI3. Only survey CIHR35 has categories 51 and 61 for hv201
   if (WASHdata$hv000[1]=="CI3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     ))}
   if (WASHdata$hv000[1]=="CI5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==42 ~ 40,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CM2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==22 ~ 32,
       hv201==31 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 96,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CM3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==22 ~ 32,
       hv201==31 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CM4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 15,
       hv201==22 ~ 32,
       hv201==31 ~ 32,
       hv201==41 ~ 43,
       hv201==42 ~ 41,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CM7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==92 ~ 73,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: COHR22 and COHR31. Only survey COHR22 has category 71 for hv201
   if (WASHdata$hv000[1] %in% c("CO2", "CO3"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 11,
       hv201==13 ~ 15,
       hv201==14 ~ 13,
       hv201==21 ~ 30,
       hv201==31 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]==2000)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 11,
       hv201==21 ~ 30,
       hv201==41 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys COHR53, COHR61, COHR72
   if (WASHdata$hv000[1]=="CO4" & WASHdata$hv007[1]>=2004 | (WASHdata$hv000[1] %in% c("CO5", "CO7")))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 11,
       hv201==22 ~ 32,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: DRHR21 and DRHR32. Only survey DRHR21 has category 71 for hv201
   if (WASHdata$hv000[1]=="DR2" | (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==96))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==31 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="DR3" & WASHdata$hv007[1]==99 )  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==25 ~ 31,
       hv201==26 ~ 31,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="DR4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==41 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: EGHR21 and EGHR33. Only survey EGHR21 has category 71 for hv201
   if (WASHdata$hv000[1] %in% c("EG2", "EG3"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 43,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: EGHR42 and EGHR4A. Both surveys are hv000=EG4. Only survey EGHR42 has category 72 for hv201
   if (WASHdata$hv000[1]=="EG4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 30,
       hv201==31 ~ 30,
       hv201==32 ~ 30,
       hv201==33 ~ 30,
       hv201==41 ~ 43,
       hv201==72 ~ 65,
       TRUE ~ hv201
     )) }
   # this is survey EGHR51 which is also hv000=EG4 as the previous two surveys. Use hv007=2005 to specify
   if (WASHdata$hv000[1]=="EG4" & WASHdata$hv007[1]==2005)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 42,
       hv201==31 ~ 21,
       hv201==32 ~ 31,
       hv201==33 ~ 41,
       hv201==41 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1992)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 15,
       hv201==21 ~ 32,
       hv201==22 ~ 42,
       hv201==23 ~ 31,
       hv201==24 ~ 41,
       hv201==31 ~ 43,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ET4" & WASHdata$hv007[1]==1997)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 15,
       hv201==21 ~ 32,
       hv201==22 ~ 42,
       hv201==31 ~ 21,
       hv201==32 ~ 31,
       hv201==33 ~ 41,
       hv201==41 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for ETHR71 and ETHR81
   if (WASHdata$hv000[1]=="ET7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GA3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 31,
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==24 ~ 32,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GA6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==32 ~ 31,
       hv201==33 ~ 32,
       hv201==34 ~ 32,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: GHHR31 and GHHR41. Only survey GHHR41 has category 61 for hv201
   if (WASHdata$hv000[1] %in% c("GH2", "GH3"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==35 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GH4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       hv201==81 ~ 73,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: GHHR5A and GHHR72
   if (WASHdata$hv000[1] %in% c("GH5", "GH6"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: GHHR7B and GHHR82. Both are hv000=GH7
   if (WASHdata$hv000[1]=="GH7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GM7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GN3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 41,
       hv201==32 ~ 42,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==35 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GN4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==34 ~ 21,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for GNHR71 and GNHR81. Both are hv000==GN7
   if (WASHdata$hv000[1]=="GN7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]==95)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 13,
       hv201==12 ~ 13,
       hv201==13 ~ 15,
       hv201==22 ~ 30,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GU3" & WASHdata$hv007[1]==98)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 13,
       hv201==12 ~ 13,
       hv201==13 ~ 15,
       hv201==14 ~ 13,
       hv201==21 ~ 30,
       hv201==31 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GU6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 15,
       hv201==31 ~ 13,
       hv201==32 ~ 30,
       hv201==41 ~ 43,
       hv201==42 ~ 43,
       hv201==43 ~ 41,
       hv201==44 ~ 42,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="GY4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==81 ~ 43,
       hv201==91 ~ 62,
       hv201==92 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HN5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 11,
       hv201==14 ~ 11,
       hv201==21 ~ 32,
       hv201==31 ~ 30,
       hv201==32 ~ 21,
       hv201==41 ~ 43,
       hv201==62 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HN6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 11,
       hv201==14 ~ 11,
       hv201==31 ~ 30,
       hv201==44 ~ 13,
       hv201==45 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HT3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==35 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==52 ~ 65,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HT4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 30,
       hv201==32 ~ 30,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       hv201==81 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HT5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==63 ~ 43,
       hv201==64 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HT6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==32 ~ 31,
       hv201==33 ~ 32,
       hv201==34 ~ 32,
       hv201==72 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="HT7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="IA2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==24 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="IA3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==22 ~ 21,
       hv201==23 ~ 30,
       hv201==24 ~ 32,
       hv201==25 ~ 31,
       hv201==26 ~ 32,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="IA7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==92 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ID2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: IDHR31 (1994) and IDHR3A (1997). Both are hv000=ID3
   if (WASHdata$hv000[1]=="ID3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==31 ~ 41,
       hv201==32 ~ 42,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ID4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: IDHR51 (2002) and IDHR63 (2007). Only IDHR63 has category 81 for hv201
   if (WASHdata$hv000[1] %in% c("ID5", "ID6"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==33 ~ 32,
       hv201==34 ~ 32,
       hv201==35 ~ 32,
       hv201==36 ~ 31,
       hv201==37 ~ 31,
       hv201==38 ~ 31,
       hv201==44 ~ 40,
       hv201==45 ~ 43,
       hv201==46 ~ 43,
       hv201==47 ~ 43,
       hv201==81 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ID7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="JO3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="JO4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==41 ~ 40,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KE2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==22 ~ 32,
       hv201==31 ~ 43,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KE3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 43,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KE4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201    )) }
   if (WASHdata$hv000[1]=="KE6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KE7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KH4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 21,
       hv201==34 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KH8")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KK3" & WASHdata$hv007[1]==95)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KK3" & WASHdata$hv007[1]==99)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==24 ~ 43,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KM3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       hv201==42 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KY3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="LB7" & WASHdata$hv007[1]==2016)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="LB7" & WASHdata$hv007[1] >=2019)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==92 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="LS4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==34 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="LS5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MA2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MA4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MB4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==63 ~ 62,
       hv201==81 ~ 41,
       hv201==82 ~ 42,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MD2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MD3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 30,
       hv201==23 ~ 32,
       hv201==24 ~ 21,
       hv201==25 ~ 30,
       hv201==26 ~ 32,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MD4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: MDHR7- (2016) and MDHR8- (2021), both have hv000==MD7
   if (WASHdata$hv000[1]=="MD7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ML3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ML4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: MLHR7- (2018) and MDHR8- (2021), both have hv000==ML7
   if (WASHdata$hv000[1]=="ML7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MM7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MR7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MV5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==52 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MV7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       hv201==52 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MW2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==13 ~ 12,
       hv201==22 ~ 30,
       hv201==23 ~ 31,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MW4" & WASHdata$hv007[1]==2000)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==32 ~ 21,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MW4" & WASHdata$hv007[1] %in% c(2004,2005))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: MWHR7A (2015) and MWHR7I (2017). Both are hv000=MW7
   if (WASHdata$hv000[1]=="MW7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MZ3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 14,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="MZ4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 14,
       hv201==21 ~ 12,
       hv201==22 ~ 14,
       hv201==23 ~ 32,
       hv201==41 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: MZHR62 (2011) and MZHR71 (2015). Both are hv000=MZ6
   if (WASHdata$hv000[1]=="MZ6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==33 ~ 21,
       TRUE ~ hv201    )) }
   if (WASHdata$hv000[1]=="MZ7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NC3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 43,
       hv201==32 ~ 40,
       hv201==41 ~ 51,
       hv201==61 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NC4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==31 ~ 30,
       hv201==32 ~ 30,
       hv201==41 ~ 43,
       hv201==42 ~ 40,
       hv201==61 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NG3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==52 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 21,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NG4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       hv201==62 ~ 65,
       TRUE ~ hv201
     )) }
   # same recode for three surveys: NGHR61 (2010), NGHR6A (2013), and NGHR71 (2015). All are hv000=NG6
   if (WASHdata$hv000[1]=="NG6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NG7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==92 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NG8")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NI2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NI3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==24 ~ 32,
       hv201==25 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 62,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NI5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==41 ~ 40,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NI6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==63 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NI7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NM2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==35 ~ 21,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NM4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 42,
       hv201==31 ~ 21,
       hv201==32 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NP3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==24 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==34 ~ 41,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NP4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 21,
       hv201==32 ~ 21,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==43 ~ 41,
       TRUE ~ hv201
     )) }
   #	same recode for two surveys: NPHR51 (2006) and NPHR61 (2011).
   if (WASHdata$hv000[1] %in% c("NP5", "NP6"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==44 ~ 41,
       TRUE ~ hv201
     )) }
   #	same recode for two surveys: NPHR7H (2016) and NPHR81 (2022).
   if (WASHdata$hv000[1] %in% c("NP7", "NP8"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="NP8")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PE2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==13 ~ 12,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PE3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 11,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   # same recode for six surveys: PEHR41,51,5I,61,6A,and 6I. The last three surveys all are hv000=PE6. Only survey PEHR41 has category 42 for hv201
   if (WASHdata$hv000[1] %in% c("PE4", "PE5", "PE6"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PG7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PH2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 11,
       hv201==22 ~ 12,
       hv201==23 ~ 30,
       hv201==24 ~ 30,
       hv201==31 ~ 32,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PH3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 31,
       hv201==22 ~ 32,
       hv201==31 ~ 41,
       hv201==32 ~ 42,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==35 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PH4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       FTRUE ~ hv201
     )) }
   # same recode for two surveys: PHHR52 (2008) and PHHR61 (2013). Only survey PHHR52 has categories 72 and 73 fpr hv201
   if (WASHdata$hv000[1] %in% c("PH5", "PH6"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==33 ~ 32,
       hv201==72 ~ 14,
       hv201==73 ~ 14,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: PHHR71 (2017) and PHHR81 (2022)
   if (WASHdata$hv000[1] %in% c("PH7", "PH8"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PK2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==13 ~ 12,
       hv201==23 ~ 21,
       hv201==24 ~ 32,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: PKHR52 (2006) and PKHR61 (2012). Only survey PKHR61 has category 63 for hv201
   if (WASHdata$hv000[1] %in% c("PK5", "PK6"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 21,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="PK7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==63 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="RW2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==13 ~ 12,
       hv201==23 ~ 21,
       hv201==24 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   # same recode for three surveys: RWHR41, RWHR53, and RWHR5A. Survey RWHR41 does not have category 21 for hv201
   if (WASHdata$hv000[1] %in% c("RW4", "RW5"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: RWHR7- (2017) and RWHR8- (2019-20), both have hv000=="RW7"
   if (WASHdata$hv000[1]=="RW7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: SLHR73 (2016) and SLHR7A- (2019), both have hv000=="SL7"
   if (WASHdata$hv000[1]=="SL7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: SNHR21 (1992-93) and SNHR32 (1997). Both are hv000=SN2. Only survey SNHR32 has categories 34, 41, and 61 for variable hv201
   if (WASHdata$hv000[1]=="SN2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: SNHR4A (2005) and SNHR51 (2006).
   if (WASHdata$hv000[1] %in% c("SN4", "SN5"))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode for four surveys: SNHR7Z (2017), SNHR80 (2018), SNHR8B (2019), SNHR8I (2020-21), all are hv000==SN7
   if (WASHdata$hv000[1]=="SN7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TD3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 32,
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==24 ~ 31,
       hv201==31 ~ 43,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TD4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==44 ~ 43,
       hv201==52 ~ 65,
       hv201==53 ~ 65,
       hv201==54 ~ 65,
       hv201==55 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TG3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 14,
       hv201==22 ~ 31,
       hv201==23 ~ 32,
       hv201==31 ~ 41,
       hv201==32 ~ 43,
       hv201==41 ~ 51,
       hv201==42 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TG6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TG7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TJ7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TL7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TR2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==42 ~ 43,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TR3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 41,
       hv201==32 ~ 40,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       hv201==71 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TR4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==21 ~ 30,
       hv201==31 ~ 30,
       hv201==42 ~ 40,
       hv201==81 ~ 72,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TR7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   # same recode for two surveys: TZHR21 (1991-92) and TZHR3A (1996). Only survey TZHR21 has categories 51 and 71 for hv201
   if (WASHdata$hv000[1]=="TZ2" | (WASHdata$hv000[1]=="TZ3" & WASHdata$hv007[1]==96))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==35 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TZ3" & WASHdata$hv007[1]==99)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 31,
       hv201==23 ~ 21,
       hv201==31 ~ 41,
       hv201==32 ~ 42,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2003,2004))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==32 ~ 21,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TZ4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==24 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==34 ~ 21,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       hv201==62 ~ 65,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2007,2008))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==24 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==34 ~ 21,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       hv201==62 ~ 65,
       hv201==91 ~ 62,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="TZ5" & WASHdata$hv007[1] %in% c(2009, 2010))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==24 ~ 32,
       hv201==25 ~ 32,
       hv201==33 ~ 31,
       hv201==34 ~ 31,
       hv201==35 ~ 31,
       hv201==36 ~ 21,
       hv201==45 ~ 40,
       TRUE ~ hv201
     )) }
   #	same recode for two surveys: TZHR7B (2015-16) and TZHR7I (2017), both are hv000=TZ7
   if (WASHdata$hv000[1]=="TZ7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 41,
       hv201==41 ~ 51,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 21,
       hv201==34 ~ 21,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       hv201==81 ~ 41,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG5" & WASHdata$hv007[1]==2006)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 21,
       hv201==23 ~ 21,
       hv201==33 ~ 31,
       hv201==34 ~ 31,
       hv201==35 ~ 32,
       hv201==36 ~ 32,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       hv201==46 ~ 43,
       hv201==91 ~ 41,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG5" & WASHdata$hv007[1] %in% c(2009,2010))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==33 ~ 31,
       hv201==34 ~ 31,
       hv201==35 ~ 21,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       hv201==46 ~ 43,
       TRUE ~ hv201
     )) }
   # same recode can be used for two surveys: UGHR61 and UGHR6A. Only survey UGHR61 has categories 22,23,33,34,35,36,44,45 and 46 and only survey UGHR6A has category 81 for hv201. Both surveys are hv000=UG6 and both are also hv007=2011
   if (WASHdata$hv000[1]=="UG6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 21,
       hv201==23 ~ 21,
       hv201==33 ~ 31,
       hv201==34 ~ 31,
       hv201==35 ~ 32,
       hv201==36 ~ 32,
       hv201==44 ~ 43,
       hv201==45 ~ 43,
       hv201==46 ~ 43,
       hv201==81 ~ 41,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG6" & WASHdata$hv007[1] %in% c(2014,2015))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 21,
       hv201==44 ~ 41,
       hv201==63 ~ 62,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG7" & WASHdata$hv007[1]==2016)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==63 ~ 62,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UG7" & WASHdata$hv007[1] %in% c(2018,2019))  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       hv201==63 ~ 62,
       hv201==72 ~ 73,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="UZ3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   # same recode for two surveys VNHR31 and VNHR41. Both are hv000=VNT. Only survey VNHR31 has category 61 for hv201
   if (WASHdata$hv000[1]=="VNT")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="VNT")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==34 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="VN5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==31 ~ 30,
       hv201==32 ~ 30,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       hv201==44 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="YE2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 11,
       hv201==14 ~ 12,
       hv201==23 ~ 21,
       hv201==24 ~ 32,
       hv201==32 ~ 43,
       hv201==35 ~ 51,
       hv201==36 ~ 43,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="YE6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==14 ~ 72,
       hv201==15 ~ 72,
       hv201==32 ~ 30,
       hv201==43 ~ 40,
       hv201==44 ~ 41,
       hv201==45 ~ 43,
       hv201==72 ~ 62,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZA3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==31 ~ 43,
       hv201==41 ~ 51,
       hv201==51 ~ 61,
       hv201==61 ~ 71,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZA7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZM2")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 30,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==51 ~ 61,
       hv201==71 ~ 96,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZM3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 30,
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==24 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZM4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==22 ~ 32,
       hv201==23 ~ 32,
       hv201==24 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZM7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZW3")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==12 ~ 13,
       hv201==21 ~ 31,
       hv201==22 ~ 32,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZW4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==21 ~ 31,
       hv201==22 ~ 32,
       hv201==23 ~ 21,
       hv201==31 ~ 40,
       hv201==32 ~ 43,
       hv201==33 ~ 43,
       hv201==41 ~ 51,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZW5")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==71 ~ 62,
       hv201==81 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="ZW7")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
       hv201==13 ~ 14,
       hv201==14 ~ 13,
       TRUE ~ hv201
     )) }


   # special code for Cambodia ----------------------------------------------------

   # NOTE: Cambodia collects data on water source for both the wet season and dry season. Below, an indicator is created for the dry season and water source and a wet	season water source. For all following indicators that use water source, only	the water source that corresponds to the season of interview (hv006 = month	of interview) is used.

   # 	e.g. If the interview took place during the dry season, then the dry season	water source is used for standard indicators in this code.


   if (WASHdata$hv000[1]=="KH4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 21,
       hv201==34 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }

   if (WASHdata$hv000[1]=="KH4")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source_dry= case_when(
       hv201==11 ~ 12,
       hv201==12 ~ 13,
       hv201==21 ~ 32,
       hv201==22 ~ 32,
       hv201==32 ~ 31,
       hv201==33 ~ 21,
       hv201==34 ~ 31,
       hv201==41 ~ 40,
       hv201==42 ~ 43,
       TRUE ~ hv201
     )) }
   if (WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]<=2006)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet = hv201w)
   }
   if (WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]<=2006)  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source_dry = hv201d)
   }
   # KHHR61 and KHHR73 used the same variables for wet and dry season
   if ((WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]>=2010) | WASHdata$hv000[1]=="KH6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet = sh104b)
   }
   if ((WASHdata$hv000[1]=="KH5" & WASHdata$hv007[1]>=2010) | WASHdata$hv000[1]=="KH6")  {
     WASHdata <- WASHdata %>% mutate(ph_wtr_source_wet = sh102)
   }


   # check if interview took place in dry season or wet season
   if (WASHdata$hv000[1] %in% c("KH4", "KH5", "KH6")) {
     WASHdata <- WASHdata %>% mutate(interview_season = case_when(
       hv006 %in% c(2, 3, 4, 11, 12) ~ 1,
       hv006 %in% c(5, 6, 7, 8, 9, 10) ~ 2)) %>%
       set_value_labels(interview_season = c(
         "dry season" = 1,
         "wet season" = 0)) %>%
       set_variable_labels(interview_season = "Interview in dry or rainy season") %>%
       # now replace water_source variable with the variable that matches the interview season
       mutate(ph_water_source = case_when(
         interview_season==1 ~ ph_wtr_source_dry,
         interview_season==2 ~ ph_wtr_source_wet))
   }


   return(WASHdata)

 }


 ph_wtr_basic<-function(Rdata){

   WASHdata <- watersource_adj(Rdata)
   # time to obtain drinking water (round trip)
   WASHdata <- WASHdata %>%
     mutate(ph_wtr_time = case_when(
     hv204 %in% c(0, 996) ~ 0,
     between(hv204, 1, 30) ~ 1,
     between(hv204, 31,900) ~ 2,
     hv204>=998 ~ 3)) %>%
     set_value_labels(ph_wtr_time =
                        c("water on premises" = 0,
                          "30 minutes or less" = 1,
                          "More than 30 minutes" = 2,
                          "don't know" = 3)) %>%
     set_variable_labels(ph_wtr_time = "Round trip time to obtain water")

   WASHdata <- WASHdata %>% mutate(ph_wtr_source = case_when(
     is.na(ph_wtr_source) ~ 99,
     TRUE ~ ph_wtr_source)) %>%
     set_value_labels(ph_wtr_source =
                        c("piped into dwelling" = 11,
                          "piped to yard/plot" = 12,
                          "public tap/standpipe" = 13,
                          "piped to neighbor" = 14,
                          "piped outside of yard/lot" = 15,
                          "tube well or borehole" = 21,
                          "well - protection unspecified" = 30,
                          "protected well" = 31,
                          "unprotected well" = 32,
                          "spring - protection unspecified" = 40,
                          "protected spring" = 41,
                          "unprotected spring" = 42,
                          "surface water (river/dam/lake/pond/stream/canal/irrigation channel)" = 43,
                          "rainwater" = 51,
                          "tanker truck" = 61,
                          "cart with small tank, cistern, drums/cans" = 62,
                          "purchased water" = 65,
                          "bottled water" = 71,
                          "purified water, filtration plant" = 72,
                          "satchet water" = 73,
                          "other" = 96,
                          "missing" = 99)) %>%
     set_variable_labels(ph_wtr_source = "Source of drinking water")

   # improved water source
   WASHdata <- WASHdata %>% mutate(ph_wtr_improve = case_when(
     ph_wtr_source %in% c(11, 12, 13, 14, 15, 21, 31, 41, 51, 61, 62, 65, 71, 72, 73) ~ 1,
     ph_wtr_source %in% c(30, 32, 40, 42, 43, 96) ~ 0,
     ph_wtr_source==99 ~ 99)) %>%
     set_value_labels(ph_wtr_improve = c(
       "improved" = 1,
       "unimproved/surface water" = 0,
       "missing" = 99)) %>%
     set_variable_labels(ph_wtr_improve = "Improved Water Source")

    WASHdata <- WASHdata %>% mutate(ph_wtr_basic = case_when(
     ph_wtr_improve==1 & ph_wtr_time<=1 ~ 1,
     ph_wtr_improve==1 & ph_wtr_time>1 ~ 2,
     ph_wtr_improve==0 ~ 3)) %>%
     set_value_labels(ph_wtr_basic =
                        c("basic water services" = 1,
                          "limited water services" = 2,
                          "unimproved water source" = 3)) %>%
     set_variable_labels(ph_wtr_basic = "Basic or limited water services")


   WASHdata$ph_wtr_basic= ifelse(  WASHdata$ph_wtr_basic == 1, 1, 0)
   colnames(WASHdata)[colnames(WASHdata) == 'ph_wtr_basic'] <- "value"
   return(WASHdata)
 }


