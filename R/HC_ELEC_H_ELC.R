
##'HC_ELEC_H_ELC Percentage of households with electricity
##' ph_electric in github
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
HC_ELEC_H_ELC<- function(Rdata){



  # weight variable
  HRdata <- Rdata %>%
    mutate(wt = hv005/1000000)

  # //Have electricity
  HRdata <- HRdata %>%
    mutate(HC_ELEC_H_ELC = hv206) %>%
    set_value_labels(HC_ELEC_H_ELC = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(HC_ELEC_H_ELC = "Have electricity")
   colnames(HRdata)[colnames(HRdata) == 'HC_ELEC_H_ELC'] <- 'value'


  return(HRdata)
}
