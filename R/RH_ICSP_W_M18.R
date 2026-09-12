#' Iron supplementation: 180+ days
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who took iron containing supplements for 180 days or more during the pregnancy of the most recent live birth (or stillbirth)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_ICSP_W_M18.R; validated on ZM_2018 (published 15.5, observed 15.5222, abs_error 0.0222).
#' @export
RH_ICSP_W_M18 <- function(IRdata) {
  # Iron supplementation during pregnancy: 180+ days.
  # Denominator: women 15-49 with a live birth in the 3 years preceding the
  #   survey (v008 - b3_01 < 36). Non-takers and don't-know/missing count as 0
  #   days (kept in the denominator). Numerator: days of iron (m46_1) in range.
  # @source LLM-authored | seed=RH_ICSP_W_M18 | iter=9 | fix=denominator = all
  #   recent-birth women (prior iter excluded non-takers, inflating the share);
  #   validated on ZM_2018.
  result <- IRdata %>%
    filter(!is.na(b3_01), (v008 - b3_01) < 36) %>%
    mutate(m46_1 = haven::zap_labels(m46_1),
           value = case_when(m46_1 >= 180 & m46_1 <= 365 ~ 1, TRUE ~ 0)) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
