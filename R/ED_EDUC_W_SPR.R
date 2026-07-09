#' Women with some primary education
#'
#' Percentage of women with some primary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_SPR <- function(IRdata) {
  # Denominator: All women 15-49 (no filter - IR default)
  # Numerator: Women with some primary education (incomplete primary)
  # v149: Educational attainment
  #   0 = no education
  #   1 = incomplete primary (SOME PRIMARY - target)
  #   2 = complete primary
  #   3 = incomplete secondary
  #   4 = complete secondary
  #   5 = higher
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        is.na(v149) ~ NA_real_,
        v149 == 1 ~ 1,
        v149 %in% c(0, 2, 3, 4, 5) ~ 0,
        TRUE ~ NA_real_  # DK/missing
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
