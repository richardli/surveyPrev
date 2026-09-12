#' Women who have a smartphone
#'
#' Percentage of women who have a smartphone
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CO/CO_MOBB_W_OSP.R; validated on TZ_2022 (published 14.8, observed 14.838, abs_error 0.038).
#' @export
CO_MOBB_W_OSP <- function(IRdata) {
  # Women age 15-49 who own a smartphone.
  # Denominator: all women 15-49. Numerator: owns a phone that is a smartphone.
  # @source LLM-authored | seed=CO_MOBB_W_OSP | iter=3 | fix=use v169c
  #   (mobile phone is a smartphone); prior iter used v169a = owns any mobile.
  result <- IRdata %>%
    mutate(value = case_when(
      v169c == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
