##' ##' MA_MBAG_W_B18  Percentage of  women first married by exact age 18
##' ms_afm_18 in github
##' IR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
MA_MBAG_W_B18<- function(Rdata){

  # Create yes and no category labels
  yesno <- c("Yes" = 1, "No" = 0)
  # First marriage by age 18
  IRdata <- Rdata %>%
    filter(v013 %in% c(3,4,5,6,7) )%>%
    mutate(MA_MBAG_W_B18 = case_when(v511>=0 & v511<18 ~ 1, TRUE ~ 0)) %>%
    set_value_labels(MA_MBAG_W_B18 = yesno) %>%
    set_variable_labels(MA_MBAG_W_B18 = "First marriage by age 18")

  colnames(IRdata)[colnames(IRdata) == 'MA_MBAG_W_B18'] <- 'value'

  return(IRdata)
}
