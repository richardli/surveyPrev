#' Download DHS geo data
#'
#' This function downloads cluster's coordinate data for country and survey.
#'
#' @param country Country name.
#' @param year Year the survey conducted.
#'
#' @return The function returns a spatial point dataset with coordinates for each cluster based on the chosen survey and year.
#' @importFrom rdhs get_datasets
#' @importFrom stringr str_to_title
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' }
#'
#' @export
getDHSgeo <- function(country, year) {
  CountryName <- country
  # CountryName<-stringr::str_to_title(country)
  # indicator<-indicator
  countryId <-rdhs::dhs_countries()[rdhs::dhs_countries()$CountryName==CountryName,]
  surveys <- rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYear = year)%>%
    dplyr::filter( FileType == 'Geographic Data')
  data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==year,]$FileName, clear_cache = T)
  geo<-readRDS(paste0(data.paths.tmp))
  return(geo)
}

