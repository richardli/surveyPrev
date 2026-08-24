#' Men who use a mobile phone for financial transactions
#'
#' Percentage of men who use a mobile phone for financial transactions
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_MOBB_M_MBF <- function(MRdata) {
  # Men who use a mobile phone for financial transactions
  # Numerator: Men age 15-49 who used a mobile phone for financial transactions in the last 12 months (mv169b = 1)
  # Denominator: All men age 15-49
  
  result <- MRdata %>%
    filter(mv012 >= 15 & mv012 <= 49) %>%
    mutate(
      value = case_when(
        mv169b == 1 ~ 1,
        mv169b == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      mcaseid,
      mv001, mv002, mv005, mv022, mv023, mv024, mv025,
      value
    )
  
  return(result)
}
