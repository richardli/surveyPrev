#' Current marital status [Women]: Married or living in union
#'
#' Percentage of women married or living in union
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
MA_MSTA_W_UNI <- function(IRdata) {
  # MA_MSTA_W_UNI: Percentage of women married or living in union
  # Numerator: Women with v501 = 1 (married) or v501 = 2 (living together)
  # Denominator: All women age 15-49
  
  IRdata %>%
    mutate(
      # Create binary indicator for married or in union
      value = case_when(
        v501 %in% c(1, 2) ~ 1,  # Married or living together
        v501 %in% c(0, 3, 4, 5) ~ 0,  # Never married, widowed, divorced, separated
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v021, v022, v023, v024, v025, value)
}
