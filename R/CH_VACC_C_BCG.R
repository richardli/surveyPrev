##' CH_VACC_C_BCG  Percentage of children 12-23 months who had received BCG vaccination
##' ms_afm_15 in github
##' KR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CH_VACC_C_BCG", year = 2018)
#' }
#'
#' @export
CH_VACC_C_BCG<- function(Rdata){


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
    mutate(CH_VACC_C_BCG =
             case_when(h2%in%c(1,2,3) ~ 1, h2%in%c(0,8)   ~ 0  )) %>%
    set_value_labels(CH_VACC_C_BCG = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(CH_VACC_C_BCG = "BCG vaccination according to either source")

  colnames(KRvac)[colnames(KRvac) == 'CH_VACC_C_BCG'] <- 'value'

  return(KRvac)
}
