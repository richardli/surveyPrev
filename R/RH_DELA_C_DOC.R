#' Assistance during delivery: Doctor
#'
#' Percentage of live births (or stillbirths) in the two (or three/five) years preceding the survey assisted by a doctor
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DELA_C_DOC.R; validated on ZM_2018 (published 8.1, observed 7.7563, abs_error 0.3437).
#' @export
RH_DELA_C_DOC <- function(KRdata) {
  # RH_DELA_C_DOC: Percentage of live births in the X years preceding the survey 
  # assisted by a doctor
  
  # Calculate b19 if it doesn't exist (age of child in months)
  if (!"b19" %in% names(KRdata)) {
    KRdata$b19 <- KRdata$v008 - KRdata$b3
  }
  
  # Determine reference period based on b19 (age of child in months)
  max_b19 <- max(KRdata$b19, na.rm = TRUE)
  if (max_b19 >= 50) {
    period_months <- 60  # 5 years
  } else if (max_b19 >= 30) {
    period_months <- 36  # 3 years
  } else {
    period_months <- 24  # 2 years
  }
  
  # Filter to births in the reference period
  df <- KRdata %>%
    filter(b19 < period_months)
  
  # m3a = 1 indicates doctor assisted at delivery
  df <- df %>%
    mutate(
      value = case_when(
        m3a == 1 ~ 1,
        m3a == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Preserve survey design columns and return one row per birth
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(as.data.frame(result))
}
