#' Assistance during delivery: Other health worker
#'
#' Percentage of live births (or stillbirths) in the two (or three/five) years preceding the survey assisted by another type of health worker
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_DELA_C_OHW.R; validated on ZM_2018 (published 1.5, observed 0.9776, abs_error 0.5224).
#' @export
RH_DELA_C_OHW <- function(KRdata) {
  # Assistance during delivery: Other health worker (community health worker),
  # highest-provider rule (doctor > nurse/midwife > clinical officer > CHW).
  # Denominator: live births in the 5 years preceding the survey.
  # @source LLM-authored | seed=RH_DELA_C_OHW | iter=9 | fix=use m3h
  #   (community health worker); prior iter used m3d/e/f which are all-NA in ZM.
  result <- KRdata %>%
    filter(b19 < 60) %>%
    mutate(value = case_when(
      m3h == 1 & m3a != 1 & m3b != 1 & m3c != 1 ~ 1,
      is.na(m3h) ~ NA_real_,
      TRUE ~ 0
    )) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
