#' Means of transport to nearest health facility: not motorized
#'
#' Percentage of women age 15-49 who use non-motorized transport to the nearest health facility
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DFHC_W_NMO.R; validated on TZ_2022 (published 84.7, observed 84.7333, abs_error 0.0333).
#' @export
RH_DFHC_W_NMO <- function(IRdata) {
  # Means of transport: not motorized [Women]
  # Base: women 15-49 with a valid response for mode of transport to nearest health facility (v483b).
  # @source LLM-authored | seed=RH_DFHC_W_NMO | iter=4 | fix=use standard DHS-8
  #   variable v483b (was a heuristic s* guess); validated on TZ_2022.
  result <- IRdata %>%
    mutate(
      m = haven::zap_labels(v483b),
      value = case_when(
        is.na(m) | m %in% c(98, 99) ~ NA_real_,
        m %in% 21:24 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
