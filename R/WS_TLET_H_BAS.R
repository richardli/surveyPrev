#' Households with basic sanitation service
#'
#' Percentage of households with basic sanitation service, defined as improved sanitation facilities that are not shared with other households
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_TLET_H_BAS <- function(HRdata) {
  # HR recode -> hv* prefix throughout. Weight is hv005/1e6.
  # Denominator: all households (no filter).
  # Numerator: improved sanitation facility (hv205 in 11, 12, 13, 15, 21, 22, 41)
  #            AND not shared (hv225 == 0).
  # Basic = improved AND not shared.
  
  IMPROVED <- c(11, 12, 13, 15, 21, 22, 41)
  
  result <- HRdata %>%
    mutate(
      value = case_when(
        hv205 %in% IMPROVED & !is.na(hv225) & hv225 == 0 ~ 1,
        hv205 %in% IMPROVED & !is.na(hv225) & hv225 == 1 ~ 0,
        !(hv205 %in% IMPROVED) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  
  return(as.data.frame(result))
}
