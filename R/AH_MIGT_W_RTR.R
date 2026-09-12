#' Type of migration: Rural to rural [Women]
#'
#' Percentage of women who moved from rural to rural
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGT_W_RTR.R; validated on TZ_2022 (published 32.1, observed 32.091, abs_error 0.009).
#' @export
AH_MIGT_W_RTR <- function(IRdata) {
  # Percentage of women who moved from rural to rural in the last 5 years
  # Denominator: Women age 15-49 born outside current place of residence (v104 ≠ 95:99)
  # Numerator: Women who moved to current residence in last 5 years, rural to rural
  #            (v104 in 0:4 AND v102=2 AND v105=3)
  
  result <- IRdata %>%
    filter(
      # Denominator: Born outside current place of residence
      # Exclude: 95=always lived here, 96=visitor, 97-99=missing/DK
      v104 %in% 0:4  # denominator fix: moved in last 5 years (iter4)
    ) %>%
    mutate(
      # Numerator: Rural to rural migration in last 5 years
      # v104 in 0:4 = lived here 0-4 years (moved in last 5 years)
      # v102 = 2 = current residence is rural
      # v105 = 3 = previous residence was rural (countryside)
      value = if_else(
        v104 %in% 0:4 & v102 == 2 & v105 == 3,
        1,
        0,
        missing = NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
