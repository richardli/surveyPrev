#' Travel time to nearest health facility: 30-59 minutes
#'
#' Percentage of women age 15-49 for whom the travel time to the nearest health facility is 30 minutes or more but less than one hour
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DFHC_W_THF.R; validated on TZ_2022 (published 28.1, observed 28.0761, abs_error 0.0239).
#' @export
RH_DFHC_W_THF <- function(IRdata) {
  # Travel time: 30-59 minutes [Women]
  # Base: women 15-49 with a valid response for travel time to nearest health facility (v483a, minutes).
  # @source LLM-authored | seed=RH_DFHC_W_THF | iter=4 | fix=use standard DHS-8
  #   variable v483a (was a heuristic s* guess); validated on TZ_2022.
  result <- IRdata %>%
    mutate(
      t = haven::zap_labels(v483a),
      value = case_when(
        is.na(t) | t >= 900 ~ NA_real_,
        t >= 30 & t < 60 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
