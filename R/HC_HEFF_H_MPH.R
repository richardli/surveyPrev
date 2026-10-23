#' Households possessing a mobile telephone
#'
#' Percentage of households possessing a mobile telephone
#'
#' @details Numerator: households with a mobile telephone (`hv243a == 1`).
#'   Denominator: all households.
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_HC/HC_HEFF_H_MPH.R; validated on ZM_2018 (published 73.7, observed 73.6758, abs_error 0.0242) and TZ_2022 (published 83.1, observed 83.1218, abs_error 0.0218).
#' @export
HC_HEFF_H_MPH <- function(HRdata) {
  # Households possessing a mobile telephone.
  # @source LLM-authored | seed=HC_HEFF_H_MPH | iter=1 | household has a mobile telephone (hv243a==1); denominator all households.
  result <- HRdata %>%
    mutate(value = case_when(hv243a == 1 ~ 1, TRUE ~ 0)) %>%
    select(hhid, hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  return(as.data.frame(result))
}
