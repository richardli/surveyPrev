#' Pneumococcal 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Pneumococcal 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_PN2 <- function(KRdata) {
  # Pneumococcal 2 vaccination received
  # Percentage of children 12-23 months who received Pneumococcal 2 vaccination
  # according to vaccination card or mother's report
  
  # Filter to living children age 12-23 months
  KRdata <- KRdata %>%
    filter(
      b5 == 1,           # Child is alive
      b19 >= 12 & b19 <= 23  # Age 12-23 months
    )
  
  # Determine if child received Pneumococcal 2
  # h55 = 1: received according to card
  # h55 = 2: received according to mother's report
  # h55 = 3: received (vaccination mark on card)
  # h55 = 0/8: not received / don't know
  KRdata <- KRdata %>%
    mutate(
      value = case_when(
        h55 %in% c(1, 2, 3) ~ 1,  # Received (card, report, or mark)
        h55 %in% c(0, 8) ~ 0,     # Not received or don't know
        TRUE ~ NA_real_
      )
    )
  
  # Select required columns
  result <- KRdata %>%
    select(
      caseid,
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
