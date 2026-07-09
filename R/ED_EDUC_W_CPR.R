#' Women with completed primary education
#'
#' Percentage of women with completed primary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_CPR <- function(IRdata) {
  IRdata %>%
    mutate(
      value = case_when(
        v106 == 1 & v107 == 7 ~ 1,              # Completed primary (7 years)
        v106 == 0 ~ 0,                           # No education
        v106 == 1 & v107 >= 0 & v107 < 7 ~ 0,  # Some primary
        v106 %in% c(2, 3) ~ 0,                   # Secondary or higher
        TRUE ~ NA_real_                          # Missing/invalid
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
}
