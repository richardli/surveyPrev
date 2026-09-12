#' Men who own a mobile phone
#'
#' Percentage of men who own a mobile phone
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CO/CO_MOBB_M_MOB.R; validated on ZM_2018 (published 66, observed 65.9917, abs_error 0.0083).
#' @export
CO_MOBB_M_MOB <- function(MRdata) {
  # Men who own a mobile phone.
  # @source LLM-authored | seed=CO_MOBB_M_MOB | iter=2 | owns a mobile phone (mv169a==1); denominator all respondents; validated on ZM_2018.
  result <- MRdata %>%
    mutate(value = case_when(mv169a == 1 ~ 1, mv169a == 0 ~ 0, TRUE ~ NA_real_)) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
