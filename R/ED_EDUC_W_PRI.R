#' Women with primary education
#'
#' Percentage of women with primary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_PRI <- function(IRdata) {
  # Women with primary education
  # Numerator: Women whose highest educational level is primary (v106 == 1)
  # Denominator: All women
  
  IRdata %>%
    mutate(
      value = case_when(
        v106 == 1 ~ 1,   # Primary education
        v106 %in% c(0, 2, 3) ~ 0,  # No education, Secondary, Higher
        TRUE ~ NA_real_  # Missing or don't know
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
