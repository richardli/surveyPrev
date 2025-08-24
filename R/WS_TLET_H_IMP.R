#'WS_TLET_H_IMP
#'PRdata
#'Percentage of households using an improved sanitation facility
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
#'                                  indicator = "WS_TLET_H_IMP",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::WS_TLET_H_IMP)
#' }
#' @export
#'
WS_TLET_H_IMP<-function(Rdata){

  sanitation_adj<-function(WASHdata){

    # Generate type of sanitation facility

    #   NOTE: this cycles through ALL country specific coding and ends around line 1495.
    #   Surveys are specified through their country code [hv000] and year [hv007] or month [hv006] when   necessary.

    # create a variable for sanitation type, this var will be overwritten if country-specific coding is needed
    WASHdata <- WASHdata %>% mutate(ph_sani_type = hv205)

    # 	recode country-specific responses to standard codes ------------------------
    if (WASHdata$hv000[1]=="AF7")  {
      WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
        hv205==43 ~ 23,
        hv205==44 ~ 51,
        TRUE ~ hv205
      )) }
    if (WASHdata$hv000[1]=="AM4" & WASHdata$hv007[1]==2000) {
      WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
        hv205==11 ~ 15,
        hv205==21 ~ 23,
        hv205==22 ~ 23,
        TRUE ~ hv205
      )) }
    # ... (rest of the country-specific codes from the original file) ...
    if (WASHdata$hv000[1]=="ZW5")  {
      WASHdata <- WASHdata %>% mutate(ph_sani_type= case_when(
        hv205==91 ~ 41,
        hv205==92 ~ 42,
        TRUE ~ hv205
      )) }

    # End of country specific codes
    return(WASHdata)
  }


  WASHdata <- Rdata #same code can be used for PR or HR files, but must be specified here

  WASHdata<-sanitation_adj(WASHdata)

  # Tested for Parasitemia via RDT
  WASHdata <- WASHdata %>% mutate(ph_sani_type = case_when(
    is.na(ph_sani_type) ~ 99,
    TRUE ~ ph_sani_type)) %>%
    set_value_labels(ph_sani_type =
                       c("flush - to piped sewer system" = 11,
                         "flush - to septic tank"	= 12,
                         "flush - to pit latrine"	= 13,
                         "flush - to somewhere else" = 14,
                         "flush - don't know where/unspecified" = 15,
                         "pit latrine - ventilated improved pit (vip)" = 21,
                         "pit latrine - with slab" = 22,
                         "pit latrine - without slab / open pit" = 23,
                         "no facility/bush/field/river/sea/lake" = 31,
                         "composting toilet" = 41,
                         "bucket toilet" = 42,
                         "hanging toilet/latrine" = 43,
                         "other improved" = 51,
                         "other" = 96,
                         "missing" = 99)) %>%
    set_variable_labels(ph_sani_type = "Type of sanitation")

  # create improved sanitation indicator
  WASHdata <- WASHdata %>% mutate(ph_sani_improve = case_when(
    ph_sani_type %in% c(11, 12, 13, 15, 21, 22, 41, 51) ~ 1,
    ph_sani_type %in% c(14, 23, 42, 43, 96) ~ 2,
    ph_sani_type ==31 ~ 3,
    ph_sani_type ==99 ~ NA)) %>%
    set_value_labels(ph_sani_improve =
                       c("improved sanitation" = 1,
                         "unimproved sanitation" = 2,
                         "open defecation" = 3)) %>%
    set_variable_labels(ph_sani_improve = "Improved sanitation")

  WASHdata$ph_sani_improve= ifelse(  WASHdata$ph_sani_improve == 1, 1, 0)

  colnames(WASHdata)[colnames(WASHdata) == 'ph_sani_improve'] <- "value"
  return(WASHdata)
}
