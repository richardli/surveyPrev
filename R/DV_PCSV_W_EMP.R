#' Sexual violence committed by employer/someone at work
#'
#' Percentage of women for whom sexual violence was committed by employer/someone at work
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_DV/DV_PCSV_W_EMP.R; validated on TZ_2022 (published 0.6, observed 0.0016, abs_error 0.5984).
#' @export
DV_PCSV_W_EMP <- function(IRdata) {
  
  # Identify all women who experienced sexual violence
  IRdata <- IRdata %>%
    mutate(dv_sex = case_when(
      d105h>0 | d105i>0 | d105k>0  ~ 1,
      d130b>0  ~ 1,
      d124==1  ~ 1,
      d125==1 ~ 1,
      v044==1  ~ 0
    ))
  
  # If d127k doesn't exist, add it as NA
  if (!"d127k" %in% names(IRdata)) {
    IRdata$d127k <- NA_real_
  }
  
  # Identify employer/work as perpetrator among SV victims
  IRdata <- IRdata %>%
    mutate(value = case_when(
      d127k==1 ~ 1,
      dv_sex==1 ~ 0
    ))
  
  # Select required survey design columns plus value
  IRdata <- IRdata %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(IRdata))
}
