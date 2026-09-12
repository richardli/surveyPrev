#' Female population age 6 and over with completed primary education
#'
#' Percentage of the de facto female household population age 6 and over with some primary education
#'
#' @param PRdata A DHS Household Member Recode (PR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_EDAT_W_CPR.R; validated on ZM_2018 (published 10.6, observed 10.6286, abs_error 0.0286).
#' @export
ED_EDAT_W_CPR <- function(PRdata) {
  # Female population age 6 and over: CPR educational attainment.
  # Denominator: de facto household females age 6+ (hv103==1, hv105>=6) with
  #   known educational attainment (hv109 in 0:5).
  # hv109: 0=none,1=incomplete primary,2=complete primary,3=incomplete secondary,
  #   4=complete secondary,5=higher.
  # @source LLM-authored | seed=ED_EDAT_W_CPR | iter=7 | fix=compute on PR household
  #   population 6+ with hv109 (prior iter used wrong recode/denominator);
  #   validated on ZM_2018.
  result <- PRdata %>%
    mutate(hv103 = haven::zap_labels(hv103), hv105 = haven::zap_labels(hv105),
           hv104 = haven::zap_labels(hv104), e = haven::zap_labels(hv109)) %>%
    filter(hv103 == 1, hv105 >= 6, hv104 == 2) %>%
    mutate(value = case_when(
      e %in% 0:5 & (e == 2) ~ 1,
      e %in% 0:5 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  return(as.data.frame(result))
}
