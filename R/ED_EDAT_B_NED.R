#' Population age 6 and over with no education
#'
#' Percentage of the de facto household population age 6 and over with no education
#'
#' @param BRdata A DHS Births Recode (BR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_B_NED <- function(BRdata) {
  # Population age 6 and over with no education
  # Denominator: children age 6+ based on birth date (includes living and deceased)
  # Numerator: mother has no education (v106 == 0) as proxy
  # v106: 0=no education, 1=primary, 2=secondary, 3=higher, 8=DK, 9=missing
  
  result <- BRdata %>%
    mutate(age_years = (v008 - b3) / 12) %>%
    filter(
      !is.na(age_years),
      age_years >= 6            # Birth occurred 6+ years ago
    ) %>%
    mutate(
      value = case_when(
        is.na(v106) ~ NA_real_,
        v106 %in% c(8, 9) ~ NA_real_,  # Don't know or missing
        v106 == 0 ~ 1,  # No education
        v106 %in% c(1, 2, 3) ~ 0,  # Primary, secondary, or higher
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
