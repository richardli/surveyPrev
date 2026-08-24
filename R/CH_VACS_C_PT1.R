#' Pentavalent 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Pentavalent 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_PT1 <- function(KRdata) {
  # Denominator: children age 12-23 months
  # Numerator: received Pentavalent 1 vaccination
  # Variable: h51 (Pentavalent 1)
  #   1 = vaccination date on card
  #   2 = reported by mother
  #   3 = vaccination marked on card
  #   0 = no vaccination
  #   8 = don't know
  
  result <- KRdata %>%
    mutate(
      age_months = v008 - b3,
      value = case_when(
        # Received Pentavalent 1 (card, recall, or mark)
        h51 %in% c(1, 2, 3) ~ 1,
        # Not received
        h51 == 0 ~ 0,
        # Don't know or missing
        h51 %in% c(8, 9) | is.na(h51) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    # Filter to children age 12-23 months
    filter(age_months >= 12 & age_months <= 23) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
