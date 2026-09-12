#' Type of migration: Urban to urban [Men]
#'
#' Percentage of men who moved from urban to urban
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGT_M_UTU.R; validated on TZ_2022 (published 39, observed 39.04, abs_error 0.04).
#' @export
AH_MIGT_M_UTU <- function(MRdata) {
  # Percentage of men who moved from urban to urban among those who moved
  # to their current place of residence in the last 5 years.
  # Denominator: Men age 15-49 who moved to current residence in the last
  #              5 years (mv104 in 0:4). The four migration-type categories
  #              (RTR/RTU/UTR/UTU) partition this denominator and sum to 100%.
  # Numerator:   Recent migrants whose current residence is urban (mv102==1)
  #              and previous residence was urban (mv105 in 0:2).
  # @source LLM-authored | seed=AH_MIGT_M_UTU | iter=4 |
  #         fix=denominator changed from all lifetime migrants to
  #         recent (last-5-year) migrants; validated on TZ_2022.
  result <- MRdata %>%
    filter(
      # Denominator: moved to current residence in the last 5 years
      mv104 %in% 0:4
    ) %>%
    mutate(
      value = if_else(
        mv102 == 1 & mv105 %in% 0:2,
        1,
        0,
        missing = NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)

  return(as.data.frame(result))
}
