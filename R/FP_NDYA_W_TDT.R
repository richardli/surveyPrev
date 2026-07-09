#' Total demand for family planning, total (all young women)
#'
#' Total demand for family planning is the sum of unmet need and those currently using
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYA_W_TDT <- function(IRdata) {
  # Total demand for family planning, total (all young women)
  # Denominator: All young women aged 15-24
  # Numerator: Women with unmet need (spacing or limiting) OR currently using FP
  # v626a: 1=unmet spacing, 2=unmet limiting, 3=using spacing, 4=using limiting,
  #        5=no unmet need, 7=not in universe, 9=missing
  # Note: Missing values (code 9) are counted as "no demand" for this indicator
  
  result <- IRdata %>%
    filter(v012 %in% 15:24) %>%  # Young women aged 15-24
    mutate(
      value = case_when(
        v626a %in% c(1, 2, 3, 4) ~ 1,   # Has demand (unmet need or using)
        v626a %in% c(5, 7, 9) ~ 0,      # No demand / not in universe / missing
        is.na(v626a) ~ 0,                # Truly NA - count as no demand
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
