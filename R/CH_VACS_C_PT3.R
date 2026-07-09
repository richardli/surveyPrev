#' Pentavalent 3 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Pentavalent 3 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_PT3 <- function(KRdata) {
  # Pentavalent 3 vaccination received
  # Denominator: living children age 12-23 months
  # Numerator: received Penta 3 (from card or mother's report)
  # Variable h7: DPT3/Pentavalent 3
  # Codes: 0=no, 1=date on card, 2=reported by mother, 3=marked on card, 8=don't know
  
  result <- KRdata %>%
    mutate(
      age_months = v008 - b3,
      value = case_when(
        b5 != 1 ~ NA_real_,  # child not alive
        age_months < 12 | age_months > 23 ~ NA_real_,
        h7 %in% c(1, 2, 3) ~ 1,
        h7 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
