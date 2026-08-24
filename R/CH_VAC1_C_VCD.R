#' Percentage showing a vaccination card
#'
#' Percentage of children one to four years of age for whom a vaccination card was shown
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VAC1_C_VCD <- function(KRdata) {
  # Percentage of children one to four years of age for whom a vaccination card was shown
  
  # Age in months at interview
  KRdata$age_months <- KRdata$v008 - KRdata$b3
  
  # Restrict to children 12-47 months (one to four years of age)
  filtered_data <- KRdata %>%
    filter(age_months >= 12 & age_months <= 47)
  
  # h1: Whether vaccination card was seen
  # 1 = Yes, card seen
  # 0 = No, card not seen
  # 2 = No longer has card
  # 3 = No card
  
  result <- filtered_data %>%
    mutate(
      value = case_when(
        h1 == 1 ~ 1,   # Card was shown/seen
        h1 %in% c(0, 2, 3) ~ 0,  # No card shown (not seen, no longer has, never had)
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
