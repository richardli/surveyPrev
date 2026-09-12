#' Men who are literate
#'
#' Percentage of men who are literate
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_LITR_M_LIT.R; validated on ZM_2018 (published 81.8, observed 81.8604, abs_error 0.0604).
#' @export
ED_LITR_M_LIT <- function(MRdata) {
  # Denominator: all men 15-49 (default MRdata frame)
  # Numerator: higher education (mv106==3) OR can read whole/part sentence (mv155 in 1,2)
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv106 == 3 | mv155 %in% c(1, 2) ~ 1,  # literate
        TRUE ~ 0  # not literate
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
