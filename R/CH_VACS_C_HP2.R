#' Hepatitis 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Hepatitis 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_HP2 <- function(KRdata) {
  # Hepatitis 2 vaccination received
  # Percentage of children 12-23 (or 24-35) months who received Hepatitis B 2
  # According to card or mother's report
  
  # Filter to children alive and age 12-23 months (or 24-35 months depending on survey)
  df <- KRdata %>%
    filter(
      b5 == 1,  # Child is alive
      b19 >= 12 & b19 <= 23  # Age 12-23 months
    ) %>%
    mutate(
      # Hepatitis B 2 received - either from card (h62=1) or from mother's report (h62=2 or h62=3)
      # h62 codes: 0=No, 1=Vaccination date on card, 2=Reported by mother, 3=Vaccination marked on card
      # Missing/NA or 0 = not received
      hepatitis2 = case_when(
        h62 %in% c(1, 2, 3) ~ 1,  # Received (card or report)
        h62 == 0 ~ 0,              # Not received
        TRUE ~ NA_real_            # Missing/Don't know
      ),
      value = hepatitis2
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(df)
}
