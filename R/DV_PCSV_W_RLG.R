#' Sexual violence committed by priest/religious leader
#'
#' Percentage of women for whom sexual violence was committed by priest/religious leader
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_DV/DV_PCSV_W_RLG.R; validated on TZ_2022 (published 0, observed 1e-04, abs_error 1e-04).
#' @export
DV_PCSV_W_RLG <- function(IRdata) {
  
  # Identify all women who experienced sexual violence
  IRdata <- IRdata %>%
    mutate(dv_sex = case_when(
      d105h > 0 | d105i > 0 | d105k > 0 ~ 1,
      d130b > 0 ~ 1,
      d124 == 1 ~ 1,
      d125 == 1 ~ 1,
      v044 == 1 ~ 0
    ))
  
  # Add d127m if it doesn't exist
  if (!"d127m" %in% names(IRdata)) {
    IRdata$d127m <- NA_real_
  }
  
  # Identify priest/religious leader as perpetrator among SV victims
  IRdata <- IRdata %>%
    mutate(value = case_when(
      d127m == 1 ~ 1,
      dv_sex == 1 ~ 0,
      TRUE ~ NA_real_
    ))
  
  # Select required survey design columns plus value
  IRdata <- IRdata %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(IRdata))
}
