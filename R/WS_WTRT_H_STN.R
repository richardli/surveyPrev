#' Households treating water by straining through a cloth
#'
#' Percentage of households treating water by straining through a cloth
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_WTRT_H_STN <- function(HRdata) {
  # Households treating water by straining through a cloth
  
  result <- HRdata %>%
    mutate(
      value = case_when(
        hv237c == 1 ~ 1,
        hv237c == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      value,
      hv001, hv002, hv005, hv022, hv023, hv024, hv025
    )
  
  return(as.data.frame(result))
}
