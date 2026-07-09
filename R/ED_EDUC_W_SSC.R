#' Women with some secondary education
#'
#' Percentage of women with some secondary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_SSC <- function(IRdata) {
  # Denominator: All women age 6+ (eligible for education questions)
  # Numerator: Women with some secondary education (incomplete secondary)
  # Uses v149 (educational attainment): 3 = incomplete secondary
  
  result <- IRdata %>%
    filter(v012 >= 6) %>%
    mutate(
      value = case_when(
        v149 == 3 ~ 1,
        v149 %in% c(0, 1, 2, 4, 5) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
