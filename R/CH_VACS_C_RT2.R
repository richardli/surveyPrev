#' Rotavirus 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Rotavirus 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_RT2 <- function(KRdata) {
  # CH_VACS_C_RT2: Rotavirus 2 vaccination received
  # Percentage of children 12-23 (or 24-35) months who had received Rotavirus 2 vaccination
  # Numerator: children with rotavirus 2 vaccination (from card or mother's report)
  # Denominator: living children age 12-23 months
  
  # Filter to living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,                    # Child is alive
      b19 >= 12 & b19 <= 23       # Age 12-23 months
    ) %>%
    mutate(
      # Rotavirus 2 received (h58):
      # 0 = No, 1 = Vaccination date on card, 2 = Reported by mother, 3 = Vaccination marked on card
      # 8 = Don't know
      # Codes 1, 2, and 3 indicate vaccination received
      value = case_when(
        h58 %in% c(1, 2, 3) ~ 1,
        h58 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(df)
}
