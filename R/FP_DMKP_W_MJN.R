#' Family planning decisionmaking jointly by wife and husband
#'
#' Percentage of currently married women age 15-49 for whom decisionmaking about whether to use or not use or family planning is made jointly by the wife and husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_DMKP_W_MJN <- function(IRdata) {
  # FP_DMKP_W_MJN
  # Family planning decisionmaking jointly by wife and husband
  # Percentage of currently married women age 15-49 for whom decisionmaking 
  # about whether to use or not use family planning is made jointly by 
  # the wife and husband
  
  # Denominator: Currently married women age 15-49 who were asked about FP decision-making
  # Numerator: Those who report decisions are made jointly
  # Variable: v632 (who decides about family planning)
  
  IRdata %>%
    filter(
      v012 >= 15 & v012 <= 49,  # Women age 15-49
      v502 == 1,                 # Currently married
      !is.na(v632) & v632 < 90   # Valid response to FP decision question
    ) %>%
    mutate(
      value = case_when(
        v632 == 3 ~ 1,                      # Joint decision
        v632 %in% c(1, 2, 4, 6) ~ 0,        # Other decision patterns
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value) %>%
    as.data.frame()
}
