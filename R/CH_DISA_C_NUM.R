#' Number of children with diarrhea for whom advice or treatment was sought
#'
#' Number of children under age 5 with diarrhea in the 2 weeks preceding the survey for whom advice or treatment was sought
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CH/CH_DISA_C_NUM.R; validated on ZM_2018 (published 989, observed 989.4279, abs_error 0.4279).
#' @export
CH_DISA_C_NUM <- function(KRdata) {
  # Number of children with diarrhea for whom advice or treatment was sought
  # scale: count
  # Denominator: living children under age 5 with diarrhea in last 2 weeks
  # Numerator: sought advice or treatment from any source (h12a-h12x)
  
  result <- KRdata %>%
    filter(b5 == 1, b19 < 60, h11 == 2) %>%
    rowwise() %>%
    mutate(
      value = if_else(
        any(c_across(matches("^h12[a-x]$")) == 1, na.rm = TRUE),
        1, 0
      )
    ) %>%
    ungroup() %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
