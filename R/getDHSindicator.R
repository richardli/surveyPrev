#' Process DHS data
#'
#' This function processes DHS data from getDHSdata function.
#'
#' @param Rdata Result from getDHSdata function, the raw DHS survry data from get_datasets.
#' @param indicator Indicator of interests.
#'
#' @return The function returns processed survey data that contains the indicator of interests.
#' \itemize{
#'   \item dat.tem
#' }
#'
#' @importFrom naniar replace_with_na
#' @import dplyr
#' @import tidyverse
#' @import labelled
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export
getDHSindicator <- function(Rdata, indicator) {
  Rdata<-Rdata
  if (indicator == "womananemia")
  {IRdata <- Rdata %>%
      mutate(wt = v005/1000000)
    #
    # *** Anemia indicators ***
    #
    # //Any anemia
    IRdata <- IRdata %>%
      mutate(nt_wm_any_anem =
               case_when(
                 v042==1 & v457<4 ~ 1 ,
                 v042==1 &  v455==0 ~ 0)) %>%
      set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
      set_variable_labels(nt_wm_any_anem = "Any anemia - women")

     raw.dat.tmp<-IRdata
      colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'nt_wm_any_anem'] <- "value"

  }else if(indicator =="ancvisit4+")
  {
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

    # //4+ ANC visits
    IRdata <- IRdata %>%
      mutate(rh_anc_4vs =
               case_when(
                 rh_anc_numvs==3 ~ 1,
                 rh_anc_numvs %in% c(0,1,2,9)   ~ 0 )) %>%
      set_value_labels(rh_anc_4vs = c("Yes" = 1, "No"=0)) %>%
      set_variable_labels(rh_anc_4vs = "Attended 4+ ANC visits")
    raw.dat.tmp<-IRdata
    colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'rh_anc_4vs'] <- "value"


  }else if(indicator == "wasting")
  {
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

    raw.dat.tmp<-PRdata
    colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'nt_ch_wast'] <- "value"

  }else if(indicator == "stunting")
  {
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

    raw.dat.tmp<-PRdata
    colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'nt_ch_stunt'] <- "value"

  }else if(indicator == "DPT3"){
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

      raw.dat.tmp<-KRvac
      colnames(raw.dat.tmp)[colnames(raw.dat.tmp) == 'ch_pent3_either'] <- "value"


  }







  if (indicator == "ancvisit4+")
  {
    strat <- attr(raw.dat.tmp$v025,which='labels')
    names(strat) <- tolower(names(strat))
    raw.dat.tmp$v025 <- ifelse(raw.dat.tmp$v025 == strat["urban"][[1]],'urban','rural')
    raw.dat.tmp$v025 <- factor(raw.dat.tmp$v025, levels = c('urban','rural'))
    raw.dat.tmp$v024 <- factor(labelled::unlabelled(raw.dat.tmp$v024))

    dat.tmp<-  raw.dat.tmp %>%
     filter( age<60)%>%#for indiviudal file
    #filter( b19<=60)%>% #for birth file
    dplyr::  select(c(cluster="v001", householdID= "v002",region="v024", weight="v005", strata="v025", value="value"))
  }
  else if(indicator %in%  c("wasting","stunting"))  {
    strat <- attr(raw.dat.tmp$hv025,which='labels')
    names(strat) <- tolower(names(strat))
    raw.dat.tmp$hv025 <- ifelse(raw.dat.tmp$hv025 == strat["urban"][[1]],'urban','rural')
    raw.dat.tmp$hv025 <- factor(raw.dat.tmp$hv025, levels = c('urban','rural'))
    raw.dat.tmp$hv024 <- factor(unlabelled(raw.dat.tmp$hv024))

    dat.tmp<-  raw.dat.tmp %>%
      dplyr::  select(c(cluster="hv001", householdID= "hv002",region="hv024", weight="hv005", strata="hv025",
                        value="value"))

  }else{
    strat <- attr(raw.dat.tmp$v025,which='labels')
    names(strat) <- tolower(names(strat))
    raw.dat.tmp$v025 <- ifelse(raw.dat.tmp$v025 == strat["urban"][[1]],'urban','rural')
    raw.dat.tmp$v025 <- factor(raw.dat.tmp$v025, levels = c('urban','rural'))
    raw.dat.tmp$v024 <- factor(labelled::unlabelled(raw.dat.tmp$v024))

    dat.tmp<-  raw.dat.tmp %>%
      # filter( age<60)%>%#for indiviudal file
      #filter( b19<=60)%>% #for birth file
      dplyr::  select(c(cluster="v001", householdID= "v002",region="v024", weight="v005", strata="v025", value="value"))

  }



  return(dat.tmp)

}
