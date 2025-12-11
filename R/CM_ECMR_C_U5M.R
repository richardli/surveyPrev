##' CM_ECMR_C_U5M u5mr 10 years prior to survey.
##'
##' BR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CM_ECMR_C_U5M", year = 2018)
#' }
#'
#' @export
#'
CM_ECMR_C_U5M<- function(Rdata){
  nmr.year=10
  BRdata=Rdata
  # convert v5 to factor using libraries already imported in surveyPrev
  BRdata$b5 <- labelled::to_factor(BRdata$b5)
  # Using the same way as surveyPrev in defining 10 year cutoff by month
  BRdata <- subset(BRdata, v008-12*nmr.year-b3 < 0)
  BRdata$strata <- NA
  # Get birth coded as person-months
  # year.cut is specified for a wide range to avoid SUMMER's rule of dropping partial year observations for now
  BRdata <- SUMMER::getBirths(data = BRdata,  strata = c("strata"),
                                   year.cut = c(0, 30000))
  BRdata$value <- BRdata$died


  return(BRdata)
}
