#' DPT 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received DPT 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_DP2 <- function(KRdata) {
  # Filter to living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12 & b19 <= 23  # age 12-23 months
    ) %>%
    mutate(
      # DPT 2 received if h5 indicates vaccination from card or mother's report
      # h5 = 1: from card, 2: reported by mother, 3: vaccination date marked
      value = case_when(
        h5 %in% c(1, 2, 3) ~ 1,  # received DPT 2
        h5 %in% c(0, 8) ~ 0,      # no or don't know
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(df)
}
