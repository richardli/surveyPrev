#' Women who used a mobile phone for financial transactions [all women]
#'
#' Percentage of all women who have used a mobile phone for financial transactions in the last 12 months
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_MOBB_W_SPT <- function(IRdata) {
  # Women who used a mobile phone for financial transactions
  # Numerator: Women age 15-49 who used a mobile phone for financial transactions in the last 12 months (v169b = 1)
  # Denominator: All women age 15-49
  # Note: NA values (women not asked, likely don't own mobile phone) are treated as 0
  
  result <- IRdata %>%
    filter(v012 >= 15 & v012 <= 49) %>%
    mutate(
      value = case_when(
        v169b == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
