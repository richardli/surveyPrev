#' Measles vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Measles vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_MSL <- function(KRdata) {
  # Denominator: living children age 12-23 months (b5 == 1 & b19 in 12:23)
  # Numerator: received measles vaccination (h9 in 1, 2, 3)
  
  result <- KRdata %>%
    filter(b5 == 1, b19 >= 12, b19 <= 23) %>%
    mutate(
      value = case_when(
        h9 %in% c(1, 2, 3) ~ 1,  # Vaccination from card or mother's report
        h9 == 0 ~ 0,              # No vaccination
        is.na(h9) | h9 %in% c(8, 9) ~ NA_real_,  # Missing/don't know
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
