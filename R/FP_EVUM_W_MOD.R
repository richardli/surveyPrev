##'FP_EVUM_W_MOD
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
FP_EVUM_W_MOD<- function(Rdata){
#note:
#manually added by Qianyu
#Ever use of any modern method of contraception (married women)
#No api results to compare with

  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  ##married
  IRdata=IRdata[IRdata$v501 %in% c(1,2),]
  IRdata <- IRdata %>%
    mutate(FP_EVUM_W_MOD =
             ifelse(v313 == 3, 1, 0)) %>%
    set_value_labels(FP_EVUM_W_MOD = c(yes = 1, no = 0)) %>%
    set_variable_labels(FP_EVUM_W_MOD ="Currently used any modern method")
  colnames(IRdata)[colnames(IRdata) == 'FP_EVUM_W_MOD'] <- "value"
  return(IRdata)
}
