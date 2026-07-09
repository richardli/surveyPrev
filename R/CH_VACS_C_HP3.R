#' Hepatitis 3 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Hepatitis 3 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_HP3 <- function(KRdata) {
  # Hepatitis 3 vaccination received
  # Percentage of children 12-23 months who received Hepatitis B 3 vaccination
  # (either from card or mother's report)
  
  # Filter: living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12,         # age >= 12 months
      b19 <= 23          # age <= 23 months
    ) %>%
    mutate(
      # Hepatitis B 3 from card (h63 == 1) or mother's report (h63 == 2 or 3)
      # h63 coding: 0=no, 1=card, 2=reported by mother, 3=vaccination mark, 8=don't know
      hp3_vacc = case_when(
        h63 %in% c(1, 2, 3) ~ 1,  # received (card, report, or mark)
        h63 == 0 ~ 0,              # not received
        TRUE ~ NA_real_            # missing/don't know
      ),
      value = hp3_vacc
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(df)
}
