#' Households treating water by boiling
#'
#' Percentage of households treating water by boiling
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_WTRT_H_BOL <- function(HRdata) {
  # Percentage of households treating water by boiling
  
  HRdata <- HRdata %>%
    mutate(
      value = case_when(
        hv237a == 1 ~ 1,
        hv237a == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  
  return(HRdata)
}
