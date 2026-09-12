#' Male population age 6 and over who attended primary education
#'
#' Percentage of the de facto male household population age 6 and over who attended primary education
#'
#' @param PRdata A DHS Household Member Recode (PR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_EDAT_M_PRI.R; validated on ZM_2018 (published 49.6, observed 50.0973, abs_error 0.4973).
#' @export
ED_EDAT_M_PRI <- function(PRdata) {
  # Male population age 6 and over: PRI educational attainment.
  # Denominator: de facto household males age 6+ (hv103==1, hv105>=6),
  #   known educational attainment (hv109 in 0:5).
  # @source LLM-authored | seed=ED_EDAT_M_PRI | iter=9 | fix=migrated to PR household
  #   population 6+ with hv109 for family consistency; validated on ZM_2018.
  result <- PRdata %>%
    mutate(hv103 = haven::zap_labels(hv103), hv105 = haven::zap_labels(hv105),
           hv104 = haven::zap_labels(hv104), e = haven::zap_labels(hv109)) %>%
    filter(hv103 == 1, hv105 >= 6, hv104 == 1) %>%
    mutate(value = case_when(
      e %in% 0:5 & (e %in% 1:2) ~ 1,
      e %in% 0:5 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  return(as.data.frame(result))
}
