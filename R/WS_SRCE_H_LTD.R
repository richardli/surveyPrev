#' Households with limited water service
#'
#' Percentage of households with limited water service, defined as an improved water source with round-trip collection time greater than 30 minutes.
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_SRCE_H_LTD <- function(HRdata) {
  # Define improved water sources based on DHS definition
  improved_sources <- c(11:14, 21, 31, 41, 51, 61, 62, 71)
  
  # Calculate limited service indicator
  # Limited = improved source with collection time > 30 minutes
  HRdata <- HRdata %>%
    mutate(
      value = case_when(
        is.na(hv201) ~ NA_real_,
        !(hv201 %in% improved_sources) ~ 0,
        hv201 %in% improved_sources & is.na(hv204) ~ NA_real_,
        hv201 %in% improved_sources & (data.table::inrange(hv204, 31, 900) | hv204 == 998) ~ 1,
        hv201 %in% improved_sources & (data.table::inrange(hv204, 0, 30) | hv204 == 996) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Return data frame with survey design variables and value
  result <- HRdata[, c("hv001", "hv002", "hv005", "hv022", "hv023", "hv024", "hv025", "value")]
  
  return(result)
}
