#' Men who watch television at least once a week
#'
#' Percentage of men who watch television at least once a week
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_MDIA_M_TLV.R; validated on ZM_2018 (published 43.2, observed 42.5046, abs_error 0.6954).
#' @export
ED_MDIA_M_TLV <- function(MRdata) {
  # Men who watch television at least once a week.
  # @source LLM-authored | seed=ED_MDIA_M_TLV | iter=1 | media exposure at least once a week (mv159 in 2:3); denominator all respondents; validated on ZM_2018.
  result <- MRdata %>%
    filter(TRUE) %>%
    mutate(value = case_when(mv159 %in% c(2,3) ~ 1, TRUE ~ 0)) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
