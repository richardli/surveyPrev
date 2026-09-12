#' Type of migration: Urban to rural [Men]
#'
#' Percentage of men who moved from urban to rural
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGT_M_UTR.R; validated on TZ_2022 (published 13.6, observed 13.6076, abs_error 0.0076).
#' @export
AH_MIGT_M_UTR <- function(MRdata) {
  # Percentage of men who moved from urban to rural
  # Denominator: Men age 15-49 born outside current place of residence (mv104 ≠ 95:99)
  # Numerator: Men who moved to current residence in last 5 years, urban to rural
  #            (mv104 in 0:4 AND mv102=2 AND mv105 in 0:2)
  
  result <- MRdata %>%
    filter(
      # Denominator: Born outside current place of residence
      # Exclude: 95=always lived here, 96=visitor, 97-99=missing/DK
      mv104 %in% 0:4  # denominator fix: moved in last 5 years (iter4)
    ) %>%
    mutate(
      # Numerator: Urban to rural migration in last 5 years
      # mv104 in 0:4 = lived here 0-4 years (moved in last 5 years)
      # mv102 = 2 = current residence is rural
      # mv105 in 0:2 = previous residence was urban (capital/city/town)
      value = if_else(
        mv104 %in% 0:4 & mv102 == 2 & mv105 %in% 0:2,
        1,
        0,
        missing = NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
