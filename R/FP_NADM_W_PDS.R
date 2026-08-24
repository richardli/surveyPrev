#' Demand for family planning satisfied
#'
#' Percentage of demand for family planning satisfied is calculated as the number of currently married women using family planning divided by the number of currently married women with demand for family planning (either with unmet need or currently using any family planning)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADM_W_PDS <- function(IRdata) {
  # Percentage of demand for family planning satisfied among currently married women
  # Numerator: Women using any contraceptive method (v626a in 3, 4)
  # Denominator: Women with total demand (v626a in 1, 2, 3, 4)
  
  IRdata %>%
    filter(
      v502 == 1,                    # Currently married women
      v626a %in% c(1, 2, 3, 4)      # Women with demand (unmet or met need)
    ) %>%
    mutate(
      value = if_else(v626a %in% c(3, 4), 1, 0)  # 1 if met need (using), 0 if unmet need
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
