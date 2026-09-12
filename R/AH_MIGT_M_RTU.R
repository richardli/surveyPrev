#' Type of migration: Rural to urban [Men]
#'
#' Percentage of men who moved from rural to urban
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGT_M_RTU.R; validated on TZ_2022 (published 17.2, observed 17.1551, abs_error 0.0449).
#' @export
AH_MIGT_M_RTU <- function(MRdata) {
  # Percentage of men who moved from rural to urban
  # Denominator: Men age 15-49 born outside current place of residence (mv104 ≠ 95:99)
  # Numerator: Men who moved to current residence in last 5 years, rural to urban
  #            (mv104 in 0:4 AND mv102=1 AND mv105=3)
  
  result <- MRdata %>%
    filter(
      # Denominator: Born outside current place of residence
      # Exclude: 95=always lived here, 96=visitor, 97-99=missing/DK
      mv104 %in% 0:4  # denominator fix: moved in last 5 years (iter4)
    ) %>%
    mutate(
      # Numerator: Rural to urban migration in last 5 years
      # mv104 in 0:4 = lived here 0-4 years (moved in last 5 years)
      # mv102 = 1 = current residence is urban
      # mv105 = 3 = previous residence was rural (countryside)
      value = if_else(
        mv104 %in% 0:4 & mv102 == 1 & mv105 == 3,
        1,
        0,
        missing = NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
