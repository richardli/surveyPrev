##' DV_EXSV_W_EVR  Percentage of women who ever experienced sexual violence
##' dv_sex in github
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
DV_EXSV_W_EVR<- function(Rdata){


  Rdata$v005=Rdata$d005

  ## **EXPERIENCED SEXUAL VIOLENCE ##

  # //Ever
  IRdata <- Rdata %>%
    mutate(DV_EXSV_W_EVR =
             case_when(
               d105h>0 | d105i>0 | d105k>0  ~ 1, # violence by current partner
               d130b>0  ~ 1,  # violence by former partner
               d124==1  ~ 1, # violence by anyone other than partner
               d125==1 ~ 1, # forced to perform unwanted acts
               v044==1  ~ 0 )) %>%
    set_value_labels(DV_EXSV_W_EVR = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(DV_EXSV_W_EVR = "Ever experienced sexual violence")

  colnames(IRdata)[colnames(IRdata) == 'DV_EXSV_W_EVR'] <- 'value'

  return(IRdata)
}
