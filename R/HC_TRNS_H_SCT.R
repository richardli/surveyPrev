#' Households possessing a motorcycle
#'
#' Percentage of households possessing a motorcycle
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
HC_TRNS_H_SCT <- function(HRdata) {
  # Percentage of households possessing a motorcycle/scooter
  # Variable: hv211 (Has motorcycle/scooter)
  # Binary indicator: 1 = yes, 0 = no (or NA)
  
  result <- HRdata %>%
    mutate(
      # hv211: Has motorcycle/scooter
      # 1 = yes, 0 = no, typically
      value = case_when(
        hv211 == 1 ~ 1,
        hv211 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      hhid,
      hv001, hv002, hv005, hv022, hv023, hv024, hv025,
      value
    )
  
  return(result)
}
