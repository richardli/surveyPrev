#' Demand for family planning satisfied by modern methods (married young women)
#'
#' Percentage of demand for family planning satisfied by modern methods is calculated as the number of currently married women using modern methods of family planning divided by the number of currently married women with demand for family planning (either with unmet need or currently using any family planning)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYM_W_PDM <- function(IRdata) {
  # Demand for family planning satisfied by modern methods (married young women)
  # Numerator: Currently married young women using modern methods
  # Denominator: Currently married young women with demand for FP (unmet need + currently using)
  
  IRdata %>%
    filter(
      v012 >= 15 & v012 <= 24,  # Young women aged 15-24
      v502 == 1                  # Currently married
    ) %>%
    mutate(
      # Modern method use (v313 codes 1-2 are traditional, 3 is modern)
      using_modern = case_when(
        v313 == 3 ~ 1,
        v313 %in% c(0, 1, 2) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Unmet need for family planning (v624 codes: 1=spacing, 2=limiting, 3=using for spacing, 4=using for limiting)
      # Demand = unmet need (codes 1,2) + currently using (codes 3,4 or v313>0)
      has_demand = case_when(
        v624 %in% c(1, 2) ~ 1,    # Unmet need for spacing or limiting
        v624 %in% c(3, 4) ~ 1,    # Using for spacing or limiting
        v313 > 0 ~ 1,              # Currently using any method
        v624 == 7 | v313 == 0 ~ 0, # No unmet need and not using
        TRUE ~ NA_real_
      ),
      
      # Percentage demand satisfied by modern methods
      value = case_when(
        has_demand == 1 & using_modern == 1 ~ 1,
        has_demand == 1 & using_modern == 0 ~ 0,
        has_demand == 0 ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
