#' Decision maker about Own health care: Wife and husband jointly [Women]
#'
#' Percentage of women for whom the decision maker for their own health care is mainly the respondent and her husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKH_W_JNT <- function(IRdata) {
  # Decision maker about Own health care: Wife and husband jointly [Women]
  # 
  # Denominator: Currently married women age 15-49 (v502 = 1)
  # Numerator: Women where decision about own health care is made jointly
  #            by respondent and husband (v743a = 2)
  # 
  # v502: Currently/formerly/never in union
  #   1 = Currently married
  # v743a: Person who usually makes decisions about respondent's health care
  #   1 = Respondent alone
  #   2 = Respondent and husband/partner jointly
  #   3 = Husband/partner alone
  #   4 = Someone else
  #   6 = Other
  #   (9 = Missing)
  
  out <- IRdata %>%
    filter(v502 == 1) %>%  # Currently married women
    mutate(
      value = case_when(
        v743a == 2 ~ 1,    # Joint decision = 1
        v743a %in% c(1, 3, 4, 6) ~ 0,  # Other response categories = 0
        TRUE ~ NA_real_    # Missing or invalid = NA
      )
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(out)
}
