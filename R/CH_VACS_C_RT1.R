#' Rotavirus 1 vaccination received
#'
#' Percentage of children 12-23 (or 24-35) months who had received Rotavirus 1 vaccination
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_RT1 <- function(KRdata) {
  # Use b19 for age if available, otherwise calculate from v008 - b3
  if (!("b19" %in% names(KRdata)) || all(is.na(KRdata$b19))) {
    KRdata <- KRdata %>%
      mutate(age = v008 - b3)
  } else {
    KRdata <- KRdata %>%
      mutate(age = b19)
  }
  
  # Denominator: living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      age >= 12,         # age >= 12 months
      age <= 23          # age <= 23 months
    ) %>%
    mutate(
      # Rotavirus 1st dose received (h57)
      # 1 = date on card, 2 = reported by mother, 3 = marked on card
      # 0 = no, 8 = don't know
      value = case_when(
        h57 %in% c(1, 2, 3) ~ 1,
        h57 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(df))
}
