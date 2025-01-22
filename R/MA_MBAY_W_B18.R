##' MA_MBAY_W_B18  Percentage of Young women age 20-24 first married by exact age 18
##' ms_afm_18 in github
##' IR
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
MA_MBAY_W_B18<- function(Rdata){

  # Create yes and no category labels
  yesno <- c("Yes" = 1, "No" = 0)
  # First marriage by age 18
  IRdata <- Rdata %>%
   filter(v013 %in% c(2) )%>%
    mutate(MA_MBAY_W_B18 = case_when(v511>=0 & v511<18 ~ 1, TRUE ~ 0)) %>%
    set_value_labels(MA_MBAY_W_B18 = yesno) %>%
    set_variable_labels(MA_MBAY_W_B18 = "First marriage by age 18")

  colnames(IRdata)[colnames(IRdata) == 'MA_MBAY_W_B18'] <- 'value'

  return(IRdata)
}
