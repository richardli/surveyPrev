#' Place of delivery: Private sector (NGO)
#'
#' Percentage of live births in the two (or three/five) years preceding the survey delivered in a private sector NGO facility
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DELP_C_PNG.R; validated on TZ_2022 (published 4.3, observed 4.3319, abs_error 0.0319).
#' @export
RH_DELP_C_PNG <- function(KRdata) {
  # RH_DELP_C_PNG: Place of delivery - Private sector (NGO)
  # Percentage of live births in the 2/3/5 years preceding the survey
  # delivered in a private sector NGO facility
  
  # Determine reference period based on b19 (child's age in months)
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
  
  # m15: Place of delivery
  # Private sector NGO includes codes 40-49:
  # 40 = NGO/mission/religious hospital
  # 41 = Mission/religious health center
  # etc.
  # Excludes 31-36 which are private non-NGO
  # Excludes 50+ which are home/other
  
  df <- df %>%
    mutate(
      value = case_when(
        m15 >= 40 & m15 < 50 ~ 1,
        !is.na(m15) & m15 < 90 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Return data frame with value and survey design columns
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
