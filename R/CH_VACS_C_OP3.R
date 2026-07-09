#' Polio 3 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Polio 3 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_OP3 <- function(KRdata) {
  # Polio 3 vaccination received
  # Percentage of children 12-23 months who received Polio 3
  
  # Denominator: living children age 12-23 months
  KRdata_subset <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12 & b19 <= 23  # age 12-23 months
    )
  
  # Numerator: received Polio 3 (h8)
  # h8 coding: 0=No, 1=Vacc. date on card, 2=Reported by mother, 3=Vacc. marked on card, 8=DK
  # Values 1, 2, 3 indicate vaccine received (from card or mother's report)
  
  KRdata_subset <- KRdata_subset %>%
    mutate(
      value = case_when(
        h8 %in% c(1, 2, 3) ~ 1,   # received Polio 3
        h8 == 0 ~ 0,               # did not receive
        TRUE ~ NA_real_            # missing or DK
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(KRdata_subset)
}
