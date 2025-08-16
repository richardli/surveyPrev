
##' HA_STIS_W_STI  Percentage of women reporting a sexually transmitted infection in the 12 months preceding the survey among women who ever had sexual intercourse
##' hk_sti in github
##' IR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "HA_STIS_W_STI", year = 2018)
#' }
#'
#' @export
HA_STIS_W_STI<- function(Rdata){


  # Create yes and no category labels
  yesno <- c("Yes" = 1, "No" = 0)



  # SELF REPORT STIS (WOMEN) -----------------------------------------------------
  # STI in the past 12 months
  IRdata <- Rdata %>%  mutate(HA_STIS_W_STI = case_when(
    v763a==1 ~ 1,
    v525==0 | v525==99 | is.na(v525) ~ NA_real_,
    TRUE ~ 0)) %>%
    set_value_labels(HA_STIS_W_STI = yesno) %>%
    set_variable_labels(HA_STIS_W_STI = "Had STI in the past 12 months")

  colnames(IRdata)[colnames(IRdata) == 'HA_STIS_W_STI'] <- 'value'

  return(IRdata)
}
