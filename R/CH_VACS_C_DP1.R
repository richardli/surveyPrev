#' DPT 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received DPT 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_DP1 <- function(KRdata) {
  # DPT 1 vaccination received
  # Percentage of children 12-23 months who received DPT 1 vaccination
  # according to vaccination card or mother's report
  
  # Denominator: living children age 12-23 months
  KRdata_subset <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12,         # age >= 12 months
      b19 <= 23          # age <= 23 months
    )
  
  # Numerator: received DPT 1 vaccination
  # h3 = 1 (had DPT 1 from card)
  # h3 = 2 (had DPT 1 from mother's report)
  # h3 = 3 (vaccination date given)
  # h3 in (0, 8, 9) means no/don't know/missing
  
  KRdata_result <- KRdata_subset %>%
    mutate(
      value = case_when(
        h3 %in% c(1, 2, 3) ~ 1,  # received DPT 1 (card, report, or date given)
        h3 %in% c(0, 8) ~ 0,      # no or don't know
        TRUE ~ NA_real_            # missing
      )
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(KRdata_result)
}
