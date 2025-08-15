#'HC_WIXQ_P_2ND
#'PRdata
#'Population in the second wealth quintile
#'
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
#'
HC_WIXQ_P_2ND <- function(Rdata){
  PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)
  PRdata$hv270=ifelse(PRdata$hv270 == 2, 1, 0)
  colnames(PRdata)[colnames(PRdata) == 'hv270'] <- "value"
  return(PRdata)
}
