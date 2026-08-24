#' Polio 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Polio 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_OP2 <- function(KRdata) {
  # Polio 2 vaccination received
  # Percentage of children 12-23 (or 24-35) months who received Polio 2
  
  # Keep only living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # Child is alive
      b19 >= 12,         # Age >= 12 months
      b19 <= 23          # Age <= 23 months
    ) %>%
    mutate(
      # h6 = Polio 2 vaccination status
      # 0 = no, 1 = vaccination date on card, 2 = reported by mother, 3 = vaccination marked on card, 8 = don't know
      # Value = 1 if received (any valid source: card or mother's report)
      value = case_when(
        h6 %in% c(1, 2, 3) ~ 1,  # Received via card date, mother's report, or card mark
        h6 == 0 ~ 0,              # Not received
        TRUE ~ NA_real_           # Missing or don't know
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(df)
}
