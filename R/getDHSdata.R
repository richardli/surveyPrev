#' Download DHS survey data
#'
#' This function downloads DHS data for a particular country and survey.
#'
#' @param country Country name.
#' @param indicator Indicator of interests. Current list of supported indicators include: "womananemia", "ancvisit4+", "stunting", "wasting", "DPT3".
#' @param Recode Types of dhs Recode
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
getDHSdata <- function(country, indicator = NULL, Recode= NULL, year) {

  # data(match_all_result)


  if(indicator %in% match_all_result$indicator_ID_DHS ){

  dataset=match_all_result[match_all_result$indicator_ID_DHS==indicator, "updated_recode_name"]#"IRdata"

  tb= data.frame(list=c("Men's Recode"     , "Household Member Recode",
                           "Children's Recode", "Births Recode",
                           "Couples' Recode"  , "Household Recode",
                           "Individual Recode","Pregnancy and Postnatal Care Recode"
  ),
  listname= c("MRdata"     , "PRdata",
                "KRdata", "BRdata",
                "CRdata"  , "HRdata",
                "IRdata","NRdata"))

  Type <- tb[tb$listname==dataset,1]

  CountryName <- country
  #CountryName<-stringr::str_to_title(country)
  countryId <-rdhs::dhs_countries()[rdhs::dhs_countries()$CountryName==CountryName,]
  potential_surveys <- rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYear = year)%>%
    dplyr::filter( FileFormat=='Stata dataset (.dta)')

  surveys <- potential_surveys %>% dplyr::filter(FileType ==c(Type))
  data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==year,]$FileName, clear_cache = T)
  Rdata<-readRDS(paste0(data.paths.tmp))
  return(Rdata)
  }else{





  IR_Individual<-c("ancvisit4+"
                     ,"RH_ANCN_W_N4P"
                     ,"womananemia"
                     ,"AN_ANEM_W_ANY"
                     ,"unmet_family"
                     ,"FP_NADA_W_UNT"
                     ,"FP_CUSA_W_MOD"
                     ,"AN_NUTS_W_THN")
 PR_Household_Member<-c(
   "CN_ANMC_C_ANY"
   ,"CN_NUTS_C_WH2"
   ,"wasting"
   ,"CN_NUTS_C_HA2"
   ,"stunting"
   ,"ML_PMAL_C_RDT"
   ,"WS_TLET_H_IMP"
   ,"WS_TLET_P_BAS"
   ,"WS_SRCE_P_BAS"
 )
 KR_Children<-c(
    "CH_DIAT_C_ORT"
   ,"DPT3"
   ,"CH_VACC_C_DP3"
   ,"CH_VACC_C_DP1"
   ,"CH_VACC_C_BAS"
   ,"CH_VACC_C_NON"
   ,"CN_BRFS_C_EXB"
   ,"CH_VACC_C_MSL"
   ,"PCV3"
   ,"RotaC1"
 )
 BRdata_Birth<-c(
   "RH_DELA_C_SKP"
   ,"CM_ECMR_C_NNR"#"nmr"
   ,"nmr"
 )

 HRdata_Household<-c(
   "ML_NETP_H_IT2"
 )

 HIV<-c(
   "HA_HIVP_B_HIV"
 )

  indicator<-indicator
  if(is.null(indicator) & is.null(Recode)){
    Type <- NULL
  }else if (!is.null(Recode)){
    Type= Recode
  }else if (indicator %in% IR_Individual) {
    Type <- c("Individual Recode")
  } else if (indicator %in% PR_Household_Member) {
    Type <- c("Household Member Recode")
  } else if (indicator %in% KR_Children) {
    Type <- c("Children's Recode")
  }else if (indicator %in% BRdata_Birth) {
    Type <- c("Births Recode")
  }else if (indicator %in% HRdata_Household) {
  Type <- c("Household Recode")
  }else if (indicator %in% HIV) {
    Type <- c("Individual Recode", "Men's Recode","HIV Test Results Recode")
  }else{
    Type <- NULL
  }


  if(!is.null(Type)){
    message(paste(Type, "is used.\n\n"))
  }else{
    message("All DHS files are downloaded.\n\n")
  }
  CountryName <- country
  #CountryName<-stringr::str_to_title(country)
  countryId <-rdhs::dhs_countries()[rdhs::dhs_countries()$CountryName==CountryName,]
  potential_surveys <- rdhs::dhs_datasets(countryIds = countryId$DHS_CountryCode, surveyYear = year)%>%
    dplyr::filter( FileFormat=='Stata dataset (.dta)')

  # file download

  if(length(Type)==1){
    #if(!is.null(Type)){
    surveys <- potential_surveys %>% dplyr::filter(FileType ==c(Type))
    data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==year,]$FileName, clear_cache = T)
    Rdata<-readRDS(paste0(data.paths.tmp))
    return(Rdata)
  }else if(length(Type)>1){
    surveys <- potential_surveys %>% dplyr::filter(FileType %in% c(Type))
    data.paths.tmp <- get_datasets(surveys[surveys$SurveyYear==year,]$FileName, clear_cache = T)
    all=list()
    listname=surveys$FileType
    for (i in 1:length(Type)){
      all[[listname[i]]] <-  readRDS(paste0(data.paths.tmp[i]))
    }
    return(all)

  }else if (is.null(Type)){
    # if null then download all and process later




    all <- NULL
    # remove HIV data for now. TODO: use try catch to download all with permission
    list <- c("Men's Recode"     , "Household Member Recode",
              "Children's Recode", "Births Recode",
              "Couples' Recode"  , "Household Recode",
              "Individual Recode","Pregnancy and Postnatal Care Recode"
              )
    listname <- c("MRdata"     , "PRdata",
              "KRdata", "BRdata",
              "CRdata"  , "HRdata",
              "IRdata","NRdata")
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
}

