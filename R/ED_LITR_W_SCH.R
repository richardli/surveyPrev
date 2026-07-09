#' Women with secondary or higher education
#'
#' Percentage of women with secondary or higher education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_LITR_W_SCH <- function(IRdata) {
  # Denominator: All women 15-49 (IRdata default)
  # Numerator: Women with higher education (v106 = 3)
  # v106: 0=no education, 1=primary, 2=secondary, 3=higher
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        v106 %in% c(8, 9) | is.na(v106) ~ NA_real_,  # Missing/DK
        v106 == 3 ~ 1,  # Higher education
        v106 %in% c(0, 1, 2) ~ 0,  # No/primary/secondary education
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
