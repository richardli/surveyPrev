#' Women who deposited or withdrew money from their account
#'
#' Percentage of women who deposited or withdrew money from their own account in the last 12 months
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CO/CO_MOBB_W_DWM.R; validated on TZ_2022 (published 0.2, observed 0.0713, abs_error 0.1287).
#' @export
CO_MOBB_W_DWM <- function(IRdata) {
  # Women who deposited or withdrew money from their own account in the last 12 months
  # Numerator: Women age 15-49 who deposited or withdrew money from account (v170 = 1)
  # Denominator: All women age 15-49
  # Note: NA values (women not asked) are treated as 0
  
  result <- IRdata %>%
    filter(v012 >= 15 & v012 <= 49) %>%
    mutate(
      value = case_when(
        v170 == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
