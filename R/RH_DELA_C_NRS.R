#' Assistance during delivery: Nurse/midwife
#'
#' Percentage of live births (or stillbirths) in the two (or three/five) years preceding the survey assisted by a nurse or midwife
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DELA_C_NRS.R; validated on ZM_2018 (published 72, observed 71.3345, abs_error 0.6655).
#' @export
RH_DELA_C_NRS <- function(KRdata) {
  # Assistance during delivery: Nurse/midwife (highest-provider rule).
  # Denominator: live births in the 5 years preceding the survey.
  # DHS assigns each birth to its highest-ranked provider (doctor outranks
  # nurse/midwife), so nurse/midwife = m3b==1 AND not a doctor (m3a!=1).
  # @source LLM-authored | seed=RH_DELA_C_NRS | iter=8 | fix=apply doctor
  #   priority (prior iter double-counted births attended by both); validated ZM_2018.
  result <- KRdata %>%
    filter(b19 < 60) %>%
    mutate(value = case_when(
      m3b == 1 & m3a != 1 ~ 1,
      is.na(m3b) ~ NA_real_,
      TRUE ~ 0
    )) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
