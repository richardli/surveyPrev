#' Demand for family planning satisfied (married young women)
#'
#' Percentage of demand for family planning satisfied is calculated as the number of currently married women using family planning divided by the number of currently married women with demand for family planning (either with unmet need or currently using any family planning)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYM_W_PDS <- function(IRdata) {
  # FP_NDYM_W_PDS: Demand for family planning satisfied (married young women)
  # Percentage of demand satisfied = (currently using FP) / (demand for FP)
  # Demand = currently using + unmet need
  # Population: Currently married young women (15-24)
  
  IRdata %>%
    filter(
      v502 == 1,           # Currently married
      v012 >= 15,          # Age 15+
      v012 <= 24           # Age <= 24 (young women)
    ) %>%
    mutate(
      # v626a coding:
      # 1,2 = unmet need (spacing, limiting)
      # 3,4 = using for spacing/limiting
      # 7 = no unmet need
      # 9 = not sexually active/infecund
      
      # Has demand = using OR unmet need
      has_demand = case_when(
        v626a %in% c(1, 2, 3, 4) ~ 1,
        v626a %in% c(7, 9) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Currently using = met need
      using_fp = case_when(
        v626a %in% c(3, 4) ~ 1,
        v626a %in% c(1, 2) ~ 0,
        v626a %in% c(7, 9) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Value: among those with demand, is the woman using?
      value = case_when(
        has_demand == 1 & using_fp == 1 ~ 1,
        has_demand == 1 & using_fp == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(value)) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
