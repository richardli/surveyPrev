#' Households possessing a private car
#'
#' Percentage of households possessing a private car
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
HC_TRNS_H_CAR <- function(HRdata) {
  # HR recode -> hv* prefix. Weight is hv005/1e6.
  # Denominator: all households (no filter).
  # Numerator: hv212 == 1 (has car/truck).
  
  result <- HRdata %>%
    mutate(
      value = case_when(
        hv212 == 1 ~ 1,
        hv212 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  
  return(as.data.frame(result))
}
