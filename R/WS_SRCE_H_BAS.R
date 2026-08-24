#' Households with basic water service
#'
#' Percentage of households with basic water service, defined as an improved water source with either water on the premises or round-trip collection time is 30 minutes or less.
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_SRCE_H_BAS <- function(HRdata) {
  # Improved water sources per DHS definition
  improved_sources <- c(11:14, 21, 31, 41, 51, 61, 62, 71)
  
  # Initialize value as NA
  HRdata$value <- NA_real_
  
  # Check for valid data
  # Exclude hv204 codes: 998 (don't know), 999 (missing)
  valid_hv201 <- !is.na(HRdata$hv201)
  valid_hv204 <- !is.na(HRdata$hv204) & !(HRdata$hv204 %in% c(998, 999))
  valid_cases <- valid_hv201 & valid_hv204
  
  # Basic water service: improved source AND (on premises OR ≤30 minutes)
  # hv204: 0-30 = ≤30 minutes, 996 = on premises
  HRdata$value[valid_cases] <- ifelse(
    HRdata$hv201[valid_cases] %in% improved_sources & 
    (HRdata$hv204[valid_cases] %in% 0:30 | HRdata$hv204[valid_cases] == 996),
    1,
    0
  )
  
  # Return with required survey design columns
  result <- HRdata[, c("hv001", "hv002", "hv005", "hv022", "hv023", "hv024", "hv025", "value")]
  
  return(result)
}
