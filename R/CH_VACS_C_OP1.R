#' Polio 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Polio 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_OP1 <- function(KRdata) {
  # Polio 1 vaccination received (h4) among children 12-23 months
  # Includes vaccination from card or mother's report
  
  # Filter to living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # Child is alive
      b19 >= 12,         # Age >= 12 months
      b19 <= 23          # Age <= 23 months
    ) %>%
    mutate(
      # h4 codes: 0=No, 1=Vaccination date on card, 2=Reported by mother, 3=Vaccination marked on card, 8=DK
      # Polio 1 received if h4 is 1, 2, or 3
      value = case_when(
        h4 %in% c(1, 2, 3) ~ 1,
        h4 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(df)
}
