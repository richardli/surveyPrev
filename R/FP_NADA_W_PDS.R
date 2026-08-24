#' Demand for family planning satisfied (all women)
#'
#' Percentage of demand for family planning satisfied is calculated as the number of all women using family planning divided by the number of all women with demand for family planning (either with unmet need or currently using any family planning)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADA_W_PDS <- function(IRdata) {
  # Percentage of demand for family planning satisfied (all women)
  # Numerator: Women using any contraceptive method (v626a in 3,4)
  # Denominator: All women with demand (v626a in 1,2,3,4)
  
  IRdata %>%
    filter(!is.na(v626a)) %>%
    mutate(
      # Total demand for family planning (unmet need or met need)
      demand = if_else(v626a %in% c(1, 2, 3, 4), 1, 0, missing = 0),
      # Demand satisfied (currently using any method)
      demand_satisfied = if_else(v626a %in% c(3, 4), 1, 0, missing = 0),
      # Calculate percentage
      value = if_else(demand == 1, demand_satisfied, NA_real_)
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
