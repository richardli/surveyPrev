#' DPT 3 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received DPT 3 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_DP3 <- function(KRdata) {
  # Percentage of children 12-23 months who received DPT 3 vaccination
  # according to vaccination card or mother's report
  
  # Keep only living children aged 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # Child is alive
      b19 >= 12,         # At least 12 months old
      b19 <= 23          # At most 23 months old
    )
  
  # DPT3 can be recorded as:
  # - h7 = 1 (received DPT 3), or h7 = 2 (by card), or h7 = 3 (by recall)
  # - h53 = 1 (received Pentavalent 3, which includes DPT), or h53 = 2/3
  # Pentavalent vaccines typically contain DPT component
  
  df <- df %>%
    mutate(
      # Check if h7 (DPT 3) indicates vaccination
      dpt3_direct = case_when(
        h7 %in% c(1, 2, 3) ~ 1,
        h7 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      # Check if h53 (Pentavalent 3) indicates vaccination
      pent3 = case_when(
        h53 %in% c(1, 2, 3) ~ 1,
        h53 == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      # Child received DPT3 if either DPT3 directly or Pentavalent 3
      value = case_when(
        dpt3_direct == 1 | pent3 == 1 ~ 1,
        dpt3_direct == 0 & pent3 == 0 ~ 0,
        is.na(dpt3_direct) & pent3 == 1 ~ 1,
        dpt3_direct == 1 & is.na(pent3) ~ 1,
        dpt3_direct == 0 & is.na(pent3) ~ 0,
        is.na(dpt3_direct) & pent3 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Select required columns
  result <- df %>%
    select(
      caseid, v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
