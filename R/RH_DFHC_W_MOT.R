#' Means of transport to nearest health facility: motorized
#'
#' Percentage of women age 15-49 who use motorized transport to the nearest health facility
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DFHC_W_MOT.R; validated on TZ_2022 (published 15.3, observed 15.2602, abs_error 0.0398).
#' @export
RH_DFHC_W_MOT <- function(IRdata) {
  # Means of transport: motorized [Women]
  # Base: women 15-49 with a valid response for mode of transport to nearest health facility (v483b).
  # @source LLM-authored | seed=RH_DFHC_W_MOT | iter=4 | fix=use standard DHS-8
  #   variable v483b (was a heuristic s* guess); validated on TZ_2022.
  result <- IRdata %>%
    mutate(
      m = haven::zap_labels(v483b),
      value = case_when(
        is.na(m) | m %in% c(98, 99) ~ NA_real_,
        m %in% 11:14 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
