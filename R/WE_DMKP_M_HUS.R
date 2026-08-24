#' Decision maker about Major household purchases: Mainly husband [Men]
#'
#' Percentage of men for whom the decision maker for major household purchases is mainly the husband
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKP_M_HUS <- function(MRdata) {
  # Decision maker about Major household purchases: Mainly husband [Men]
  # Percentage of currently married men age 15-49 for whom the decision maker 
  # for major household purchases is mainly the husband
  
  # Filter: Currently married men (mv502 = 1)
  df <- MRdata %>%
    filter(mv502 == 1) %>%
    mutate(
      # mv743b: Person who usually makes decisions about large household purchases
      # Code 1 = respondent alone (husband alone)
      # Code 2 = respondent and wife/partner jointly
      # Code 3 = wife/partner alone
      # Code 4 = someone else
      # Code 6 = other
      value = case_when(
        mv743b == 1 ~ 1,  # Mainly husband (respondent alone)
        mv743b %in% c(2, 3, 4, 6) ~ 0,  # Other decision patterns
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      mcaseid,
      mv001, mv002, mv005, mv021, mv022, mv023, mv024, mv025,
      value
    )
  
  return(df)
}
