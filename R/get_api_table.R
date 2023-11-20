
#'Function to obtain subnational estimates from DHS API
#' @param country keys at: https://api.dhsprogram.com/rest/dhs/countries?returnFields=CountryName,DHS_CountryCode&f=html
#' @param survey keys at: https://api.dhsprogram.com/rest/dhs/surveys?returnFields=SurveyId,SurveyYearLabel,SurveyType,CountryName&f=html
#' @param indicator keys at: https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
#' @param simplify if TRUE only the value and region index is returned.
#'
get_api_table <- function(coutry, survey,indicator, simplify = TRUE){
  call <- paste0("https://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&indicatorIds=",
                 indicator,
                 "&countryIds=",
                 country,
                 "&surveyIds=",
                 survey,
                 "&lang=en&f=csv")
  tab <- read.csv(call)
  if(simplify){
    tab <- tab[, c("Value", "CharacteristicLabel", "ByVariableLabel")]
  }

  return(tab)

}
