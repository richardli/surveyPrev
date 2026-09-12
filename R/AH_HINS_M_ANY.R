#' Any health insurance
#'
#' Percentage of men with any health insurance
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_HINS_M_ANY.R; validated on ZM_2018 (published 3.1, observed 3.0966, abs_error 0.0034).
#' @export
AH_HINS_M_ANY <- function(MRdata) {
  # Any health insurance coverage among men 15-49. Denominator: all men.
  # mv481 = covered by health insurance (0=no, 1=yes).
  # @source LLM-authored | seed=AH_HINS_M_ANY | iter=9 | fix=use mv481 (prior
  #   iter referenced non-existent sm481* country vars); validated ZM_2018.
  result <- MRdata %>%
    mutate(value = case_when(
      mv481 == 1 ~ 1,
      mv481 == 0 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
