#'CN_ANMC_C_ANY
#'PRdata
#'Children with any anemia
#'Children under five with any anemia
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CN_ANMC_C_ANY",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::CN_ANMC_C_ANY)
#' }
#' @export
CN_ANMC_C_ANY<-function(Rdata){

  PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)

  PRdata <- PRdata %>%
    mutate(nt_ch_any_anem =
             case_when(
               hv103==1 & hc1>5 & hc1<60 & hc56<110 ~ 1 ,
               hv103==1 & hc1>5 & hc1<60 & hc56>=110 ~ 0)) %>%
    set_value_labels(nt_ch_any_anem = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_ch_any_anem = "Any anemia - child 6-59 months")
  colnames(PRdata)[colnames(PRdata) == 'nt_ch_any_anem'] <- "value"
  return(PRdata)
}
