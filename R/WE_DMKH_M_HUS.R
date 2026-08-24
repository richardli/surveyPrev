#' Decision maker about Own health care: Mainly husband [Men]
#'
#' Percentage of men for whom the decision maker for their own health care is mainly the husband
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKH_M_HUS <- function(MRdata) {
  # Decision maker about Own health care: Mainly husband [Men]
  # Percentage of men for whom the decision maker for their own health care is mainly the husband
  
  # Denominator: Currently married men age 15-49 (mv502 = 1)
  # Numerator: Those where mv743a indicates husband/respondent makes decision
  
  result <- MRdata %>%
    filter(
      mv502 == 1,  # Currently married
      mv012 >= 15 & mv012 <= 49  # Age 15-49
    ) %>%
    mutate(
      # mv743a: Person who usually makes decisions about respondent's health care
      # Based on standard DHS coding for men:
      # 1 = Respondent (himself)
      # 2 = Wife/partner
      # 3 = Respondent and wife/partner jointly
      # 4 = Someone else
      # 5 = Other
      # 6 = Not applicable/no one
      # 
      # For men, "mainly husband" means the respondent himself makes the decision
      value = case_when(
        mv743a == 1 ~ 1,  # Respondent (husband) makes decision
        mv743a %in% c(2, 3, 4, 5, 6) ~ 0,  # Other decision makers
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      mv001, mv002, mv005, mv022, mv023, mv024, mv025,
      value
    )
  
  return(result)
}
