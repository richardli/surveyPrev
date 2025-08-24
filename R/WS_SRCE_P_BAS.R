#'WS_SRCE_P_BAS
#'PRdata
#'Population using a basic water source
#'
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "WS_SRCE_P_BAS",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::WS_SRCE_P_BAS)
#' }
#' @export
#'
WS_SRCE_P_BAS<-function(Rdata){
  watersource_adj<-function(Rdata){

    WASHdata=Rdata

    # generate water source indicator ----------------------------------------------

    # create a variable for water source, this var will be overwritten if country-specific coding is needed
    WASHdata <- WASHdata %>% mutate(ph_wtr_source = hv201)

    # country-specific coding ------------------------------------------------------
    if (WASHdata$hv000[1]=="AF7")  {
      WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
        hv201==14 ~ 13,
        TRUE ~ hv201
      )) }
    # ... (rest of the country-specific codes from the original file) ...
    if (WASHdata$hv000[1]=="ZW7")  {
      WASHdata <- WASHdata %>% mutate(ph_wtr_source= case_when(
        hv201==13 ~ 14,
        hv201==14 ~ 13,
        TRUE ~ hv201
      )) }

    # special code for Cambodia ----------------------------------------------------
    # ... (rest of the Cambodia-specific code) ...
    if (WASHdata$hv000[1] %in% c("KH4", "KH5", "KH6")) {
      WASHdata <- WASHdata %>% mutate(interview_season = case_when(
        hv006 %in% c(2, 3, 4, 11, 12) ~ 1,
        hv006 %in% c(5, 6, 7, 8, 9, 10) ~ 2)) %>%
        set_value_labels(interview_season = c(
          "dry season" = 1,
          "wet season" = 0)) %>%
        set_variable_labels(interview_season = "Interview in dry or rainy season") %>%
        # now replace water_source variable with the variable that matches the interview season
        mutate(ph_wtr_source = case_when(
          interview_season==1 ~ ph_wtr_source_dry,
          interview_season==2 ~ ph_wtr_source_wet))
    }

    return(WASHdata)
  }

  WASHdata <- watersource_adj(Rdata)
  # time to obtain drinking water (round trip)
  WASHdata <- WASHdata %>%
    mutate(ph_wtr_time = case_when(
      hv204 %in% c(0, 996) ~ 0,
      between(hv204, 1, 30) ~ 1,
      between(hv204, 31,900) ~ 2,
      hv204>=998 ~ 3)) %>%
    set_value_labels(ph_wtr_time =
                       c("water on premises" = 0,
                         "30 minutes or less" = 1,
                         "More than 30 minutes" = 2,
                         "don't know" = 3)) %>%
    set_variable_labels(ph_wtr_time = "Round trip time to obtain water")

  WASHdata <- WASHdata %>% mutate(ph_wtr_source = case_when(
    is.na(ph_wtr_source) ~ 99,
    TRUE ~ ph_wtr_source)) %>%
    set_value_labels(ph_wtr_source =
                       c("piped into dwelling" = 11,
                         "piped to yard/plot" = 12,
                         "public tap/standpipe" = 13,
                         "piped to neighbor" = 14,
                         "piped outside of yard/lot" = 15,
                         "tube well or borehole" = 21,
                         "well - protection unspecified" = 30,
                         "protected well" = 31,
                         "unprotected well" = 32,
                         "spring - protection unspecified" = 40,
                         "protected spring" = 41,
                         "unprotected spring" = 42,
                         "surface water (river/dam/lake/pond/stream/canal/irrigation channel)" = 43,
                         "rainwater" = 51,
                         "tanker truck" = 61,
                         "cart with small tank, cistern, drums/cans" = 62,
                         "purchased water" = 65,
                         "bottled water" = 71,
                         "purified water, filtration plant" = 72,
                         "satchet water" = 73,
                         "other" = 96,
                         "missing" = 99)) %>%
    set_variable_labels(ph_wtr_source = "Source of drinking water")

  # improved water source
  WASHdata <- WASHdata %>% mutate(ph_wtr_improve = case_when(
    ph_wtr_source %in% c(11, 12, 13, 14, 15, 21, 31, 41, 51, 61, 62, 65, 71, 72, 73) ~ 1,
    ph_wtr_source %in% c(30, 32, 40, 42, 43, 96) ~ 0,
    ph_wtr_source==99 ~ 99)) %>%
    set_value_labels(ph_wtr_improve = c(
      "improved" = 1,
      "unimproved/surface water" = 0,
      "missing" = 99)) %>%
    set_variable_labels(ph_wtr_improve = "Improved Water Source")

  WASHdata <- WASHdata %>% mutate(ph_wtr_basic = case_when(
    ph_wtr_improve==1 & ph_wtr_time<=1 ~ 1,
    ph_wtr_improve==1 & ph_wtr_time>1 ~ 2,
    ph_wtr_improve==0 ~ 3)) %>%
    set_value_labels(ph_wtr_basic =
                       c("basic water services" = 1,
                         "limited water services" = 2,
                         "unimproved water source" = 3)) %>%
    set_variable_labels(ph_wtr_basic = "Basic or limited water services")

  WASHdata$ph_wtr_basic= ifelse(  WASHdata$ph_wtr_basic == 1, 1, 0)
  colnames(WASHdata)[colnames(WASHdata) == 'ph_wtr_basic'] <- "value"
  return(WASHdata)
}
