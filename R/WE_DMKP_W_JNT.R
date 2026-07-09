#' Decision maker about Major household purchases: Wife and husband jointly [Women]
#'
#' Percentage of women for whom the decision maker for major household purchases is mainly the respondent and her husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKP_W_JNT <- function(IRdata) {
  # Decision maker about Major household purchases: Wife and husband jointly [Women]
  # Percentage of currently married women age 15-49 for whom decisions about large 
  # household purchases are made jointly by the respondent and her husband
  
  # Filter to currently married women (v502 = 1)
  df <- IRdata %>%
    filter(v502 == 1) %>%
    mutate(
      # v743b: Person who usually makes decisions about large household purchases
      # Code 2 = respondent and husband/partner jointly
      value = case_when(
        v743b == 2 ~ 1,  # Jointly
        v743b %in% c(1, 3, 4, 5, 6) ~ 0,  # Other valid responses
        TRUE ~ NA_real_  # Missing/DK
      )
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(df)
}
