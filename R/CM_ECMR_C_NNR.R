#'CM_ECMR_C_NNR
#'BRdata
#'Neonatal mortality rate
#'
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#' @param nmr.year This is an argument specifically for NMR calculation. It specifies births how many years do we include prior to the date of survey. Default to be 10, i.e., NMR in the last 10 years prior to survey.
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "CM_ECMR_C_NNR",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::CM_ECMR_C_NNR)
#' }
#' @export
#'
CM_ECMR_C_NNR<- function(Rdata, nmr.year=10){

  BRdata <- Rdata%>%
    mutate(wt = v005/1000000)%>%
    mutate(bo10 = Rdata$v008-12*nmr.year-b3)%>%
    filter(bo10<0)

  BRdata$value<- ifelse(BRdata$b7==0, 1, 0)
  BRdata$value[is.na( BRdata$value)] <- 0

  return(BRdata)
}
