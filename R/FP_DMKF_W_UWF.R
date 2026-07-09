#' Family planning use decisionmaking mainly by wife
#'
#' Among currently married women using family planning, the percentage for whom the decision was made mainly by the wife
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_DMKF_W_UWF <- function(IRdata) {
  # FP_DMKF_W_UWF: Among currently married women using family planning,
  # the percentage for whom the decision was made mainly by the wife
  
  # Currently married women
  currently_married <- IRdata$v502 == 1
  
  # Using family planning (modern or traditional method)
  # v312 is current contraceptive method
  using_fp <- IRdata$v312 %in% 1:18
  
  # Decision-maker for family planning use
  # v632: who decided on using contraception
  # 1 = mainly respondent (wife)
  # 2 = mainly husband
  # 3 = joint decision
  # 4 = other
  # 6 = other (alternate code in some surveys)
  
  result <- IRdata %>%
    filter(currently_married & using_fp) %>%
    mutate(
      value = case_when(
        v632 == 1 ~ 1,  # mainly wife
        v632 %in% c(2, 3, 4, 6) ~ 0,  # other decision patterns
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
