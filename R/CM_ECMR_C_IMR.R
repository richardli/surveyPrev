##' CM_ECMR_C_IMR imr 10 years prior to survey.
##'
##' BR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#' @param mort.cut  age group cutoffs for hazard calculation. Constant hazards are assumed to be within the specified cutoffs.
#' @param mort.year The number of years preceding survey. Set to be 10.
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
CM_ECMR_C_IMR<- function(Rdata, mort.cut = c(1, 2, 6, 12, 24, 36, 48, 60), mort.year = 10){


  Rdata$b5 <- labelled::to_factor(Rdata$b5)

  # restrict births to IMR exposure window
  Rdata <- subset(Rdata, v008 - 12*(mort.year + 1) - b3 < 0)

  Rdata$strata <- NA

  raw.dat.tmp <- SUMMER::getBirths(
    data = Rdata,
    strata = c("strata"),
    year.cut = c(1900,2100),
    month.cut = mort.cut
  )

  Rdata$id.new <- 1:nrow(Rdata)

  raw.dat.tmp1 <- dplyr::left_join(raw.dat.tmp, Rdata[, c("id.new","v008")])

  raw.dat.tmp1 <- subset(raw.dat.tmp1, v008 - mort.year*12 < obsStart)

  raw.dat.tmp <- raw.dat.tmp1[, -which(colnames(raw.dat.tmp1) == "v008")]
  raw.dat.tmp$value <- raw.dat.tmp$died

  return(raw.dat.tmp)
}
