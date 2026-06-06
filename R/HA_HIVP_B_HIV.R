

#'HA_HIVP_B_HIV
#'hv_hiv_pos
#'"HIV prevalence among general population"
#'
# HV_PREV.do IR+AR
#' @param Rdata  data.frame from surveyPrev::getDHSdata IR+AR+MR
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "HA_HIVP_B_HIV",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::HA_HIVP_B_HIV)
#' }
#' @export
#'
HA_HIVP_B_HIV<-function(Rdata){
  IRdata <- Rdata$`Individual Recode`
  MRdata <- Rdata$`Men's Recode`
  ARdata <- Rdata$`HIV Test Results Recode`

  # A merge of the IR and MR files with the AR file is needed to produce the Total HIV prevalence and present them by background variables present in the IR and MR files
  # The following merge sequence will produce an IRMRARmerge file for the survey of interest

  # merge AR file to IR file
  temp <- ARdata

  temp[["v001"]] <- temp[["hivclust"]]
  temp[["v002"]] <- temp[["hivnumb"]]
  temp[["v003"]] <- temp[["hivline"]]


  # merge
  IRARtemp <- merge(IRdata, temp, by = c("v001", "v002", "v003"), all = FALSE)
  IRARtemp <- IRARtemp %>% mutate(sex = 2)

  # merge AR file to MR file
  temp <- ARdata

  temp[["mv001"]] <- temp[["hivclust"]]
  temp[["mv002"]] <- temp[["hivnumb"]]
  temp[["mv003"]] <- temp[["hivline"]]

  # merge
  MRARtemp <- merge(MRdata, temp, by = c("mv001", "mv002", "mv003"), all = FALSE)
  MRARtemp <- MRARtemp %>% mutate(sex = 1)

  # append IRARtemp and MRARtemp

  # IMPORTANT! we are renaming all mv* variables to v* variables.
  names(MRARtemp) <- stringr::str_replace_all(names(MRARtemp),"mv","v")

  IRMRARmerge <- suppressWarnings(bind_rows(IRARtemp,MRARtemp))

  # limiting to age 15-49, you can comment this out if you want all men
  IRMRARmerge <- IRMRARmerge %>%
    filter(!v012>49)



  IRMRARmerge <- IRMRARmerge %>%
    mutate(hv_hiv_pos = case_when(
      hiv03==1  ~ 1,
      TRUE ~ 0),
      hv_hiv_pos = add_labels(hv_hiv_pos, labels = c("No"=0, "Yes"=1)),
      hv_hiv_pos = set_label(hv_hiv_pos, label = "HIV positive test result"))

  colnames(IRMRARmerge)[colnames(IRMRARmerge) == 'hv_hiv_pos'] <- "value"
  return(IRMRARmerge)

}
