#' Children tested for malaria with RDT
#'
#' Percentage of children age 6-59 months tested for malaria with RDT
#'
#' @param PRdata A DHS Household Member Recode (PR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ML/ML_CMLT_C_RDT.R; validated on TZ_2022 (published 96.9, observed 96.6604, abs_error 0.2396).
#' @export
ML_CMLT_C_RDT <- function(PRdata) {
  # Children age 6-59 months tested for malaria with an RDT.
  # Denominator: de facto children 6-59 months (hc1 in 6:59, hv103 == 1).
  # Numerator: children with an RDT result (hml35 in 0=negative / 1=positive).
  # @source LLM-authored | seed=ML_CMLT_C_RDT | iter=9 | fix=moved to PR
  #   recode; hml35 (result of malaria rapid test) is a PR biomarker variable
  #   absent from KR. Validated on TZ_2022.
  result <- PRdata %>%
    filter(hc1 >= 6, hc1 <= 59, hv103 == 1) %>%
    mutate(value = case_when(
      hml35 %in% c(0, 1) ~ 1,
      TRUE ~ 0
    )) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  return(as.data.frame(result))
}
