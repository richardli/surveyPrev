#' Female population age 6 and over with some primary education
#'
#' Percentage of the de facto female household population age 6 and over with no education education
#'
#' @param PRdata A DHS Household Member Recode (PR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_EDAT_W_SPR.R; validated on ZM_2018 (published 44, observed 44.2095, abs_error 0.2095).
#' @export
ED_EDAT_W_SPR <- function(PRdata) {
  # Female population age 6 and over: SPR educational attainment.
  # Denominator: de facto household females age 6+ (hv103==1, hv105>=6),
  #   known educational attainment (hv109 in 0:5).
  # @source LLM-authored | seed=ED_EDAT_W_SPR | iter=3 | fix=migrated to PR household
  #   population 6+ with hv109 for family consistency; validated on ZM_2018.
  result <- PRdata %>%
    mutate(hv103 = haven::zap_labels(hv103), hv105 = haven::zap_labels(hv105),
           hv104 = haven::zap_labels(hv104), e = haven::zap_labels(hv109)) %>%
    filter(hv103 == 1, hv105 >= 6, hv104 == 2) %>%
    mutate(value = case_when(
      e %in% 0:5 & (e == 1) ~ 1,
      e %in% 0:5 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  return(as.data.frame(result))
}
