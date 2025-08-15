#'FG_PFCC_W_WCC
#'IRdata
#'Percentage of women circumcised (women who experienced female genital cutting (FGM))
#'
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "FP_CUSA_W_MOD",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::fp_cruse_mod)
#' }
#' @export
FG_PFCC_W_WCC <- function(Rdata){


  IRdata <- Rdata %>%
    mutate(FG_PFCC_W_WCC =
             case_when(
               g102==1 ~ 1 ,
               g102==0 | g100==0 | g100==1 ~ 0)) %>%
    set_value_labels(FG_PFCC_W_WCC = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(FG_PFCC_W_WCC = "Circumcised among women age 15-49")


  colnames(IRdata)[colnames(IRdata) == 'FG_PFCC_W_WCC'] <- "value"
  return(IRdata)


}




