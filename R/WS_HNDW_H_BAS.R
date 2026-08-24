#' Households with a basic handwashing facility, with soap and water available
#'
#' Percentage of households with a basic handwashing facility, defined as a handwashing facility with soap and water available
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_HNDW_H_BAS <- function(HRdata) {
  # Denominator: all households
  # Numerator: fixed handwashing facility observed + soap + water present
  # hv230a: 1=fixed facility observed, 2=mobile, 3=no facility
  # hv230b: 1=water present, 0=no water
  # hv232: 1=soap present, 0=no soap
  
  result <- HRdata %>%
    mutate(
      value = case_when(
        # Basic: FIXED facility + soap + water
        hv230a == 1 & hv230b == 1 & hv232 == 1 ~ 1,
        # Missing data for key variables
        is.na(hv230a) | is.na(hv230b) | is.na(hv232) ~ NA_real_,
        # All other cases (mobile, no facility, or missing soap/water)
        TRUE ~ 0
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  
  return(as.data.frame(result))
}
