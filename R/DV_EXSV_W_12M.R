##' DV_EXSV_W_12M  Percentage of women who ever experienced sexual violence
##' dv_sex_12m in github
##' IR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "DV_EXSV_W_12M", year = 2018)
#' }
#'
#' @export
DV_EXSV_W_12M<- function(Rdata){


  Rdata$v005=Rdata$d005

  ## **EXPERIENCED SEXUAL VIOLENCE ##

  # //In the last 12 months
  IRdata <- Rdata %>%
    mutate(DV_EXSV_W_12M =
             case_when(
               d105h %in% c(1,2) | d105i %in% c(1,2) | d105k %in% c(1,2)  ~ 1,
               d130b==1 | d124==1  ~ 1,
               v044==1  ~ 0 )) %>%
    set_value_labels(DV_EXSV_W_12M = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(DV_EXSV_W_12M = "Experienced sexual violence in past 12 mos")

  colnames(IRdata)[colnames(IRdata) == 'DV_EXSV_W_12M'] <- 'value'

  return(IRdata)
}
