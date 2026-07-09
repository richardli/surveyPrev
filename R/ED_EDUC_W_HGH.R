#' Women with more than secondary education
#'
#' Percentage of women with more than secondary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_HGH <- function(IRdata) {
  # Women with more than secondary education
  # Numerator: Women with higher education (v106 == 3)
  # Denominator: All eligible women
  
  IRdata %>%
    mutate(
      value = case_when(
        v106 == 3 ~ 1,  # Higher education
        v106 %in% c(0, 1, 2) ~ 0,  # No, primary, or secondary education
        TRUE ~ NA_real_  # Missing or don't know
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
}
