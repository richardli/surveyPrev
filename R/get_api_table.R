#' Function to obtain subnational estimates from DHS API
#'
#' @param country A character string of keys at: https://api.dhsprogram.com/rest/dhs/countries?returnFields=CountryName,DHS_CountryCode&f=html
#' @param survey A character string of keys at: https://api.dhsprogram.com/rest/dhs/surveys?returnFields=SurveyId,SurveyYearLabel,SurveyType,CountryName&f=html
#' @param indicator A character string of keys at: https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
#' @param simplify if TRUE only the value and region index is returned.
#'
#' @importFrom utils read.csv
#' @return a data frame of the DHS indicator estimates
#' @export
#' @examples
#' \dontrun{
#' # country:  Zambia
#' # survey: 2018 DHS
#' # indicator: Percentage of children stunted
#' #             (below -2 SD of height for age
#' #              according to the WHO standard)
#' dhs_table <- get_api_table(country = "ZM",
#'                            survey = "ZM2018DHS",
#'                            indicator = "CN_NUTS_C_HA2",
#'                            simplify = TRUE)
#' dhs_table
#' }
#'
get_api_table <- function(country,survey,indicator, simplify = TRUE, admin=1){

  if(admin==1){
    call <- paste0("https://api.dhsprogram.com/rest/dhs/data?breakdown=subnational&indicatorIds=",
                   indicator,
                   "&countryIds=",
                   country,
                   "&surveyIds=",
                   survey,
                   "&lang=en&f=csv")
    tab <- read.csv(call)
  }else if(admin==0){

    call <- paste0("https://api.dhsprogram.com/rest/dhs/data?indicatorIds=",
                   indicator,
                   "&countryIds=",
                   country,
                   "&surveyIds=",
                   survey,
                   "&lang=en&f=csv")
    options(warn = -1)
    tab <- read.csv(call)
  }






  if(simplify){
    tab <- tab[, c("Value", "CharacteristicLabel", "ByVariableLabel")]
    if(sum(!is.na(tab$ByVariableLabel)) == 0){
      tab <- tab[, c("Value", "CharacteristicLabel")]
    }
  }

  return(tab)

}
