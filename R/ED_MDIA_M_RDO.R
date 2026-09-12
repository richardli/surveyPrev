#' Men who listen to the radio at least once a week
#'
#' Percentage of men who listen to the radio at least once a week
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_MDIA_M_RDO.R; validated on ZM_2018 (published 55.9, observed 56.2453, abs_error 0.3453).
#' @export
ED_MDIA_M_RDO <- function(MRdata) {
  # Men who listen to the radio at least once a week.
  # @source LLM-authored | seed=ED_MDIA_M_RDO | iter=1 | media exposure at least once a week (mv158 in 2:3); denominator all respondents; validated on ZM_2018.
  result <- MRdata %>%
    filter(TRUE) %>%
    mutate(value = case_when(mv158 %in% c(2,3) ~ 1, TRUE ~ 0)) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
