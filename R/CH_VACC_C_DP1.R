#'CH_VACC_C_DP1
#'KRdata
#'Pentavalent 1st dose vaccination
#'Percentage of children (age 12-23) with 1st dose of Pentavalent vaccine
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACC_C_DP1",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::CH_VACC_C_DP1)
#' }
#' @export
#'
CH_VACC_C_DP1<-function(Rdata){

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

