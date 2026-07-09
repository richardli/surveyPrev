#' Households with a place for handwashing was observed
#'
#' Percentage of households where a place for washing hands was observed
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_HNDW_H_OBS <- function(HRdata) {
  # Households with a place for handwashing observed (fixed or mobile)
  # hv230a: 1 = Fixed, 2 = Mobile
  # Numerator: hv230a in (1, 2)
  # Denominator: All households
  
  HRdata %>%
    mutate(
      value = case_when(
        hv230a %in% c(1, 2) ~ 1,
        !is.na(hv230a) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
}
