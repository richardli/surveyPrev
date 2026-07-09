#' Women with no education
#'
#' Percentage of women with no education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_NED <- function(IRdata) {
  # Women with no education
  # Percentage of women with no education
  # Numerator: Women with v106 == 0 (no education)
  # Denominator: All women
  
  IRdata %>%
    mutate(
      value = case_when(
        v106 == 0 ~ 1,  # No education
        v106 %in% 1:3 ~ 0,  # Primary, Secondary, or Higher
        TRUE ~ NA_real_  # Don't know, missing, or invalid
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
}
