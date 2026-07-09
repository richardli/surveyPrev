#' Decision maker about Own health care: Wife and husband jointly [Men]
#'
#' Percentage of men for whom the decision maker for their own health care is mainly the respondent and her husband
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKH_M_JNT <- function(MRdata) {
  # Denominator: Currently married men age 15-49 (mv502 == 1)
  # Numerator: Own health care decisions made jointly with wife
  # Variable: mv743a (person who usually makes decisions about own health care)
  # Joint decision = code 2 (respondent and wife jointly)
  
  result <- MRdata %>%
    filter(mv502 == 1) %>%  # Currently married
    mutate(
      value = case_when(
        mv743a == 2 ~ 1,  # Joint decision with wife
        mv743a %in% c(1, 3, 4, 5, 6) ~ 0,  # Other valid responses
        mv743a %in% c(8, 9) ~ NA_real_,  # Don't know/missing
        is.na(mv743a) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
