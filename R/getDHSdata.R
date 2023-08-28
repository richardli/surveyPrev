#' Download DHS survey data
#'
#' This function downloads DHS data for a particular country and survey.
#'
#' @param country Country name.
#' @param indicator Indicator of interests. Current list of supported indicators include: "womananemia", "ancvisit4+", "stunting", "wasting", "DPT3".
#' @param year Year the survey conducted.
#'
#' @return This function returns the survey dataset that contains the indicator:
#' \itemize{
#'   \item Rdata
#' }
#' @importFrom rdhs get_datasets
#' @importFrom stringr str_to_title
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export
getDHSdata <- function(country, indicator, year) {


  indicator<-indicator
  if (indicator %in% c("womananemia", "ancvisit4+")) {
    Type <- c("Individual Recode")
  } else if (indicator %in% c("stunting", "wasting")) {
    Type <- c("Household Member Recode")
  } else if (indicator %in% c("DPT3")) {
    Type <- c("Children's Recode")
  }
  message(paste(Type, "is used.\n\n"))

  CountryName<-stringr::str_to_title(country)
  countryId <-rdhs::dhs_countries()[rdhs::dhs_countries()$CountryName==CountryName,]
  potential_surveys <- rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYear = year)%>%
    dplyr::filter( FileFormat=='Stata dataset (.dta)')
 
  surveys <- potential_surveys %>% dplyr::filter(FileType ==c(Type))
  data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==year,]$FileName, clear_cache = T)
  Rdata<-readRDS(paste0(data.paths.tmp))
  return(Rdata)
}

