#' Pentavalent 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Pentavalent 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_PT2 <- function(KRdata) {
  # Denominator: children aged 12-23 months
  # Numerator: received Pentavalent 2 vaccination
  # Variable: h52 (pentavalent 2: 0=no, 1=card, 2=recall, 3=marked, 8=don't know)
  
  result <- KRdata %>%
    mutate(
      age_months = v008 - b3,
      value = case_when(
        age_months < 12 | age_months >= 24 ~ NA_real_,
        h52 %in% c(1, 2, 3) ~ 1,
        h52 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
