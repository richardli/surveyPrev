#'CH_VACC_C_NON
#'KRdata
#'Children with no vaccinations (age 12-23)
#'
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CH_VACC_C_NON",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::CH_VACC_C_NON)
#' }
#' @export
#'
CH_VACC_C_NON<-function(Rdata){


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
