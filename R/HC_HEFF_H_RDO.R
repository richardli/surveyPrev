
##'HC_HEFF_H_RDO Percentage of households possessing a radio
##' ph_radio in github
##' HR
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
HC_HEFF_H_RDO<- function(Rdata){



  # weight variable
  HRdata <- Rdata %>%
    mutate(wt = hv005/1000000)
  # //Radio
  HRdata <- HRdata %>%
    mutate(HC_HEFF_H_RDO =
             case_when(
               hv207==0 | is.na(hv207) ~ 0,
               hv207==1 ~ 1)) %>%
    set_value_labels(HC_HEFF_H_RDO = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(HC_HEFF_H_RDO = "Owns a radio")

  colnames(HRdata)[colnames(HRdata) == 'HC_HEFF_H_RDO'] <- 'value'

  return(HRdata)
}
