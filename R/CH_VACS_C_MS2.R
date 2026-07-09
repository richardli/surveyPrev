#' Measles 2 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Measles 2 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_MS2 <- function(KRdata) {
  # CH_VACS_C_MS2: Measles 2 vaccination received
  # Percentage of children 24-35 months who received Measles 2 vaccination
  
  # Filter to living children age 24-35 months
  df <- KRdata %>%
    filter(
      b5 == 1,  # Child is alive
      b19 >= 24 & b19 <= 35  # Age 24-35 months
    ) %>%
    mutate(
      # Measles 2 vaccination
      # h9a codes: 0=No, 1=Card, 2=Reported by mother, 3=Vaccination mark, 8=DK
      measles2 = case_when(
        h9a %in% c(1, 2, 3) ~ 1,  # Received (any source)
        h9a == 0 ~ 0,              # Not received
        TRUE ~ NA_real_             # Missing/DK
      ),
      value = measles2
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(df)
}
