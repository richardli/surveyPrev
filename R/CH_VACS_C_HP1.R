#' Hepatitis 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Hepatitis 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_HP1 <- function(KRdata) {
  # Hepatitis B 1 vaccination received among children 12-23 months
  # Per DHS Guide: h61 = received Hepatitis B 1
  # Numerator: card or mother's report (h61 in 1:3)
  # Denominator: living children 12-23 months (b5=1 & b19 in 12:23)
  
  KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12,         # age >= 12 months
      b19 <= 23          # age <= 23 months
    ) %>%
    mutate(
      value = case_when(
        h61 %in% c(1, 2, 3) ~ 1,  # vaccination date on card (1), reported by mother (2), vaccination marked on card (3)
        h61 == 0 ~ 0,              # no vaccination
        TRUE ~ NA_real_            # missing/don't know
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
