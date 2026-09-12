#' Travel time to nearest health facility: 60-119 minutes
#'
#' Percentage of women age 15-49 for whom the travel time to the nearest health facility is 1 hour or more but less than 2 hours
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DFHC_W_SXO.R; validated on TZ_2022 (published 15.6, observed 15.5584, abs_error 0.0416).
#' @export
RH_DFHC_W_SXO <- function(IRdata) {
  # Travel time: 60-119 minutes [Women]
  # Base: women 15-49 with a valid response for travel time to nearest health facility (v483a, minutes).
  # @source LLM-authored | seed=RH_DFHC_W_SXO | iter=4 | fix=use standard DHS-8
  #   variable v483a (was a heuristic s* guess); validated on TZ_2022.
  result <- IRdata %>%
    mutate(
      t = haven::zap_labels(v483a),
      value = case_when(
        is.na(t) | t >= 900 ~ NA_real_,
        t >= 60 & t < 120 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
