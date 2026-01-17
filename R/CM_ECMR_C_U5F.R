##' CM_ECMR_C_U5F u5mr 5 years prior to survey.
##'
##' BR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#' @param mort.cut  age group cutoffs for hazard calculation. Constant hazards are assumed to be within the specified cutoffs.
#' @param mort.year The number of years preceding survey. Set to be 10.
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CM_ECMR_C_U5F", year = 2018)
#' }
#'
#' @export
#'
CM_ECMR_C_U5F<- function(Rdata, mort.cut = c(1, 2, 6, 12, 24, 36, 48, 60), mort.year = 5){
    
    Rdata <- CM_ECMR_C_U5M(Rdata, mort.cut = mort.cut, mort.year = 5)

  return(Rdata)
}