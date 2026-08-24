#' BCG vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received BCG vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_BCG <- function(KRdata) {
  # BCG vaccination received
  # Percentage of children 12-23 months who had received BCG vaccination
  # according to vaccination card or mother's report
  
  # Denominator: living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12,         # age >= 12 months
      b19 <= 23          # age <= 23 months
    ) %>%
    mutate(
      # BCG vaccination received either:
      # - from card (h1 == 1 & h2 in {1,2,3})
      # - from mother's report (h1 != 1 & h2 == 1)
      # h2 coding: 0=No, 1=Vacc date on card, 2=Reported by mother, 3=Vacc marked on card
      bcg_card = case_when(
        h1 == 1 & h2 %in% c(1, 2, 3) ~ 1,
        h1 == 1 & h2 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      bcg_report = case_when(
        h1 != 1 & h2 == 1 ~ 1,
        h1 != 1 & h2 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      # Combine: received if either source confirms
      value = case_when(
        bcg_card == 1 | bcg_report == 1 ~ 1,
        bcg_card == 0 | bcg_report == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(df)
}
