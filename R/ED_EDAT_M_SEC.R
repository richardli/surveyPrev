#' Male population age 6 and over who attended secondary education
#'
#' Percentage of the de facto male household population age 6 and over who attended secondary education
#'
#' @param PRdata A DHS Household Member Recode (PR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_EDAT_M_SEC.R; validated on ZM_2018 (published 30.6, observed 30.929, abs_error 0.329).
#' @export
ED_EDAT_M_SEC <- function(PRdata) {
  # Male population age 6 and over: SEC educational attainment.
  # Denominator: de facto household males age 6+ (hv103==1, hv105>=6) with
  #   known educational attainment (hv109 in 0:5).
  # hv109: 0=none,1=incomplete primary,2=complete primary,3=incomplete secondary,
  #   4=complete secondary,5=higher.
  # @source LLM-authored | seed=ED_EDAT_M_SEC | iter=9 | fix=compute on PR household
  #   population 6+ with hv109 (prior iter used wrong recode/denominator);
  #   validated on ZM_2018.
  result <- PRdata %>%
    mutate(hv103 = haven::zap_labels(hv103), hv105 = haven::zap_labels(hv105),
           hv104 = haven::zap_labels(hv104), e = haven::zap_labels(hv109)) %>%
    filter(hv103 == 1, hv105 >= 6, hv104 == 1) %>%
    mutate(value = case_when(
      e %in% 0:5 & (e %in% 3:4) ~ 1,
      e %in% 0:5 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  return(as.data.frame(result))
}
