#' Family planning use decisionmaking mainly by others/don't know/missing
#'
#' Among currently married women using family planning, the percentage for whom the decision was made mainly by others or the response is don't know or missing
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_DMKF_W_UDK <- function(IRdata) {
  # Family planning use decisionmaking mainly by others/don't know/missing
  # Among currently married women using family planning, the percentage for whom 
  # the decision was made mainly by others or the response is don't know or missing
  
  # Filter to currently married women (v502 == 1)
  # Filter to those currently using FP (v312 != 0)
  # Decision maker variable is v632
  
  IRdata %>%
    filter(v502 == 1) %>%  # Currently married
    filter(!is.na(v312) & v312 != 0) %>%  # Currently using FP
    mutate(
      value = case_when(
        # v632: who decided to use family planning
        # 1 = mainly wife
        # 2 = mainly husband  
        # 3 = joint decision
        # 4 = other
        # 8 = don't know
        # 9 = missing
        v632 %in% c(4, 8, 9) | is.na(v632) ~ 1,
        v632 %in% c(1, 2, 3) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
