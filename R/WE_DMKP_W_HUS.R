#' Decision maker about Major household purchases: Mainly husband [Women]
#'
#' Percentage of women for whom the decision maker for major household purchases is mainly the husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKP_W_HUS <- function(IRdata) {
  # Denominator: Currently married women age 15-49 (v502 == 1)
  # Numerator: Decision maker for large household purchases is mainly husband (v743b == 4)
  # Weight: v005 / 1,000,000
  
  result <- IRdata %>%
    filter(v502 == 1) %>%  # Currently married women
    mutate(
      value = case_when(
        v743b == 4 ~ 1,  # Husband alone
        v743b %in% c(1, 2, 3, 5, 6) ~ 0,  # Other valid responses
        v743b %in% c(8, 9) | is.na(v743b) ~ NA_real_,  # Don't know/missing
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
