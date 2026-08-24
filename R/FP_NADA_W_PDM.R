#' Demand for family planning satisfied by modern methods (all women)
#'
#' Percentage of demand for family planning satisfied by modern methods is calculated as the number of all women using modern methods of family planning divided by the number of all women with demand for family planning (either with unmet need or currently using any family planning)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADA_W_PDM <- function(IRdata) {
  # Percentage of demand for family planning satisfied by modern methods (all women)
  # Numerator: Women using modern methods (v313 = 3)
  # Denominator: All women with demand for FP (v626a in 1,2,3,4)
  
  IRdata %>%
    mutate(
      # Demand for family planning: met need or unmet need
      has_demand = case_when(
        v626a %in% c(1, 2, 3, 4) ~ 1,
        TRUE ~ 0
      ),
      # Using modern methods
      using_modern = case_when(
        v313 == 3 ~ 1,
        TRUE ~ 0
      ),
      # Demand satisfied by modern methods
      value = case_when(
        has_demand == 1 & using_modern == 1 ~ 1,
        has_demand == 1 & using_modern == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
}
