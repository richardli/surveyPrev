#'ML_NETP_H_IT2
#'HRdata
#'Households with access to an insecticide-treated mosquito net (ITN)
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
#'                                  indicator = "ML_NETP_H_IT2",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ML_NETP_H_IT2)
#' }
#' @export
#'
ML_NETP_H_IT2<-function(Rdata){
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
