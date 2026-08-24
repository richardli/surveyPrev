#' Households treating water by adding bleach/chlorine
#'
#' Percentage of households treating water by adding bleach or chlorine
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_WTRT_H_BLC <- function(HRdata) {
  # Households treating water by adding bleach/chlorine
  # Numerator: Households that add bleach/chlorine to drinking water
  # Denominator: All households
  # Expected: 23.7% for Zambia 2018
  
  HRdata <- HRdata %>%
    mutate(
      value = case_when(
        hv237b == 1 ~ 1,
        hv237b == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  
  return(HRdata)
}
