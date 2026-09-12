#' IPV 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received an IPV 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CH/CH_VACS_C_IP1.R; validated on TZ_2022 (published 71.8, observed 72.3562, abs_error 0.5562).
#' @export
CH_VACS_C_IP1 <- function(KRdata) {
  # IPV 1 vaccination received, children 12-23 months.
  # @source LLM-authored | seed=CH_VACS_C_IP1 | iter=6 | fix=use h60
  #   (received inactivated polio / IPV); prior iter used h61 = hepatitis B 1.
  result <- KRdata %>%
    filter(b5 == 1, b19 >= 12, b19 <= 23) %>%
    mutate(value = case_when(
      h60 %in% c(1, 2, 3) ~ 1,
      h60 == 0 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
