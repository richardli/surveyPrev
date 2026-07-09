#' Decision maker about Own health care: Mainly wife [Women]
#'
#' Percentage of women for whom the decision maker for their own health care is mainly the respondent
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKH_W_WIF <- function(IRdata) {
  # Decision maker about Own health care: Mainly wife [Women]
  # Percentage of currently married women age 15-49 for whom the decision maker
  # for their own health care is mainly the respondent (wife)
  
  # Denominator: Currently married women age 15-49 (v502 = 1)
  # Numerator: Among those, women where v743a = 1 (respondent alone decides)
  
  df <- IRdata %>%
    filter(
      v012 >= 15 & v012 <= 49,  # Age 15-49
      v502 == 1                  # Currently married
    ) %>%
    mutate(
      # v743a: Person who usually makes decisions about respondent's health care
      # 1 = respondent alone
      # 2 = respondent and husband/partner
      # 3 = husband/partner alone
      # 4 = someone else
      # 5 = other
      # 6 = not applicable (not stated in questionnaire)
      # 9 = missing
      value = case_when(
        v743a == 1 ~ 1,           # Respondent alone (mainly wife)
        v743a %in% c(2, 3, 4, 5, 6) ~ 0,  # Other valid responses
        v743a == 9 | is.na(v743a) ~ NA_real_,  # Missing
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(df)
}
