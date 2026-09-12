#' Type of migration: Urban to urban [Women]
#'
#' Percentage of women who moved from urban to urban
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGT_W_UTU.R; validated on TZ_2022 (published 28.3, observed 28.3092, abs_error 0.0092).
#' @export
AH_MIGT_W_UTU <- function(IRdata) {
  # Type of migration: Urban to urban [Women] among women who moved to current residence in the last 5 years.
  # Denominator: v104 in 0:4 (moved in last 5 years). The four types sum to 100%.
  # @source LLM-authored | seed=AH_MIGT_W_UTU | iter=4 | fix=denominator=recent migrants + corrected current/previous residence logic; validated on TZ_2022.
  result <- IRdata %>%
    filter(v104 %in% 0:4) %>%
    mutate(value = if_else(v102 == 1 & v105 %in% 0:2, 1, 0, missing = NA_real_)) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
