##' MA_MBAG_W_B15  Women first married by exact age 15
##' ms_afm_15 in github
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
MA_MBAG_W_B15<- function(Rdata){


  # Create yes and no category labels
  yesno <- c("Yes" = 1, "No" = 0)

  # First marriage by age 18
  IRdata <- Rdata %>%
    filter(v013 %in% c(3,4,5,6,7) )%>%
    mutate(MA_MBAG_W_B15 = case_when(v511>=0 & v511<15 ~ 1, TRUE ~ 0)) %>%
    set_value_labels(MA_MBAG_W_B15 = yesno) %>%
    set_variable_labels(MA_MBAG_W_B15 = "First marriage by age 15")

  colnames(IRdata)[colnames(IRdata) == 'MA_MBAG_W_B15'] <- 'value'

  return(IRdata)
}
