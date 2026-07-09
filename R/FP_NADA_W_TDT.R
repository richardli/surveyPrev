#' Total demand for family planning, total (all women)
#'
#' Total demand for family planning is the sum of unmet need and those currently using
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADA_W_TDT <- function(IRdata) {
  # Total demand for family planning (all women)
  # Numerator: women with unmet need (spacing or limiting) OR met need (spacing or limiting)
  # Denominator: all women (no filter)
  # v626a: 1=unmet spacing, 2=unmet limiting, 3=met spacing, 4=met limiting, 0=no unmet need
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        v626a %in% c(1, 2, 3, 4) ~ 1,  # Total demand = unmet + met need
        v626a == 0 ~ 0,                  # No unmet need
        v626a %in% c(97, 98, 99) ~ NA_real_,  # Don't know / missing
        is.na(v626a) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
