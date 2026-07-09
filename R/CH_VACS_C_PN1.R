#' Pneumococcal 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Pneumococcal 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_PN1 <- function(KRdata) {
  # Pneumococcal 1 vaccination received
  # Percentage of children 12-23 months who received Pneumococcal 1 vaccination
  # according to vaccination card or mother's report
  
  # Denominator: living children age 12-23 months
  data_subset <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12,         # age in months >= 12
      b19 <= 23          # age in months <= 23
    )
  
  # Numerator: received Pneumococcal 1 (h54)
  # h54 coding: 0=No, 1=Vacc date on card, 2=Reported by mother, 3=Vacc marked on card, 8=DK
  # Consider 1, 2, or 3 as "received"
  result <- data_subset %>%
    mutate(
      value = case_when(
        h54 %in% c(1, 2, 3) ~ 1,  # received (card date, reported, or marked)
        h54 == 0 ~ 0,              # not received
        TRUE ~ NA_real_            # missing or don't know
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
