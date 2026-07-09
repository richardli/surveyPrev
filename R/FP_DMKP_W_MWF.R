#' Family planning decisionmaking mainly by wife
#'
#' Percentage of currently married women age 15-49 for whom decisionmaking about whether to use or not use or family planning is made mainly by the wife
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_DMKP_W_MWF <- function(IRdata) {
  # FP_DMKP_W_MWF
  # Family planning decisionmaking mainly by wife
  # Percentage of currently married women age 15-49 for whom decisionmaking 
  # about whether to use or not use family planning is made mainly by the wife
  
  # Denominator: Currently married women age 15-49 who were asked about FP decision-making
  # Numerator: Those who report decisions are made mainly by the wife
  # Variable: v632 (who decides about family planning)
  
  IRdata %>%
    filter(
      v012 >= 15 & v012 <= 49,  # Women age 15-49
      v502 == 1,                 # Currently married
      !is.na(v632) & v632 < 90   # Valid response to FP decision question
    ) %>%
    mutate(
      value = case_when(
        v632 == 1 ~ 1,                    # Mainly wife
        v632 %in% c(2, 3, 4, 6) ~ 0,     # Husband/joint/other
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value) %>%
    as.data.frame()
}
