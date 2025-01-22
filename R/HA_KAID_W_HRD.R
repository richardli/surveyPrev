
##' HA_KAID_W_HRD Percentage of women who have heard of HIV or AIDS
##' hk_ever_heard in github
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
HA_KAID_W_HRD<- function(Rdata){


  # Create yes and no category labels
  yesno <- c("Yes" = 1, "No" = 0)



  # HIV RELATED KNOWLEDGE (WOMEN) ------------------------------------------------

  # // Ever heard of HIV/AIDS
  IRdata <- Rdata %>% mutate(HA_KAID_W_HRD = v751)  %>%
    set_value_labels(HA_KAID_W_HRD = yesno) %>%
    set_variable_labels(HA_KAID_W_HRD = "Have ever heard of HIV or AIDS")

  colnames(IRdata)[colnames(IRdata) == 'HA_KAID_W_HRD'] <- 'value'

  return(IRdata)
}
