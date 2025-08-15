
##'HC_FLRM_H_CER Percentage of households with ceramic tile floors
##' ph_floor in github
##' HR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
HC_FLRM_H_CER<- function(Rdata){



  # weight variable
  HRdata <- Rdata %>%
    mutate(wt = hv005/1000000)

  # //Flooring material
  HRdata <- HRdata %>%
    mutate(ph_floor = hv213) %>%
    set_variable_labels(y = "Flooring material")


  # Percentage of households with ceramic tile floors
  HRdata <- HRdata %>% mutate(HC_FLRM_H_CER = case_when(
    ph_floor %in% c(33) ~ 1,
    ph_floor %in% c(10,11,12,20,21,22,30,31,32,34,35,96) ~ 0,
    ph_floor==99 ~ 99)) %>%
    set_value_labels(HC_FLRM_H_CER = c(
      "households with ceramic tile floors" = 1,
      "other" = 0,
      "missing" = 99)) %>%
    set_variable_labels(HC_FLRM_H_CER = "Percentage of households with ceramic tile floors")
  colnames(HRdata)[colnames(HRdata) == 'HC_FLRM_H_CER'] <- 'value'


  return(HRdata)
}
