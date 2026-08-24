#' Men who used a mobile phone for financial transactions [all men]
#'
#' Percentage of all men who have used a mobile phone for financial transactions in the last 12 months
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_MOBB_M_SPT <- function(MRdata) {
  # Men who used mobile phone for financial transactions in last 12 months
  # Denominator: all men (no filter)
  # Numerator: used mobile phone for financial transactions
  # Variable: mv169a (coded as 0=yes, 1=no based on diagnostic)
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv169a == 0 ~ 1,  # Yes, used mobile phone for financial transactions
        mv169a == 1 ~ 0,  # No
        mv169a %in% c(8, 9) ~ NA_real_,  # Don't know / missing
        is.na(mv169a) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
