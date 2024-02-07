#' Download DHS survey data
#'
#' This function downloads DHS data for a particular country and survey.
#'
#' @param country Country name.
#' @param indicator Indicator of interests. Current list of supported indicators include: "womananemia", "ancvisit4+", "stunting", "wasting", "DPT3".
#' @param year Year the survey conducted.
#'
#' @return This function returns the survey dataset that contains the indicator.
#' @importFrom rdhs get_datasets
#' @importFrom stringr str_to_title
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' # When indicator is known, download only the relevant file
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#'
#' # When indicator is NULL or not recognized, download all files
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = NULL,
#'                                  year = 2018)
#' names(dhsData)
#' }
#'
#' @export
getDHSdata <- function(country, indicator = NULL, year) {


  indicator<-indicator
  if(is.null(indicator)){
    Type <- NULL
  }else if (indicator %in% c("womananemia", "ancvisit4+", "unmet_family")) {
    Type <- c("Individual Recode")
  } else if (indicator %in% c("stunting", "wasting", "sanitation")) {
    Type <- c("Household Member Recode")
  } else if (indicator %in% c("DPT3")) {
    Type <- c("Children's Recode")
  }else if (indicator %in% c("nmr")) {
    Type <- c("Births Recode")
  }else{
    Type <- NULL
  }
  if(!is.null(Type)){
    message(paste(Type, "is used.\n\n"))
  }else{
    message("All DHS files are downloaded.\n\n")
  }

  CountryName<-stringr::str_to_title(country)
  countryId <-rdhs::dhs_countries()[rdhs::dhs_countries()$CountryName==CountryName,]
  potential_surveys <- rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYear = year)%>%
    dplyr::filter( FileFormat=='Stata dataset (.dta)')

  # single file download
  if(!is.null(Type)){
    surveys <- potential_surveys %>% dplyr::filter(FileType ==c(Type))
    data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==year,]$FileName, clear_cache = T)
    Rdata<-readRDS(paste0(data.paths.tmp))
    return(Rdata)

  # if null then download all and process later
  }else{
    all <- NULL
    # remove HIV data for now. TODO: use try catch to download all with permission
    list <- c("Men's Recode"     , "Household Member Recode",
              "Children's Recode", "Births Recode",
              "Couples' Recode"  , "Household Recode",
              "Individual Recode")
    listname <- c("MRdata"     , "PRdata",
              "KRdata", "BRdata",
              "CRdata"  , "HRdata",
              "IRdata")
    for(i in 1:length(list)){
      Type <- list[i]
      surveys <- potential_surveys %>% dplyr::filter(FileType ==c(Type))

      if( dim(surveys)[1]==0){}
      else{

        data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear ==
                                                 year, ]$FileName, clear_cache = T)
        Rdata <- readRDS(paste0(data.paths.tmp))
        all[[listname[i]]] <- Rdata
      }


    }
    return(all)
  }
}

