#' Households using an improved water source
#'
#' Percentage of households whose main source of drinking water is an improved source
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_SRCE_H_IMP <- function(HRdata) {
  # Improved water sources: hv201 in 11:14, 21, 31, 41, 51, 61, 62, 71
  # 11-12: piped into dwelling/yard, 13: public tap, 14: piped to neighbor
  # 21: tubewell/borehole, 31: protected well, 41: protected spring
  # 51: rainwater, 61-62: tanker truck/cart, 71: bottled water
  improved_sources <- c(11, 12, 13, 14, 21, 31, 41, 51, 61, 62, 71)
  
  HRdata %>%
    mutate(
      value = case_when(
        hv201 %in% improved_sources ~ 1,
        is.na(hv201) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(
      value,
      hv001, hv002, hv005, hv022, hv023, hv024, hv025
    )
}
