#' Women giving birth by age 18
#'
#' Percentage of women who gave birth by age 18
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FE_BBAG_W_A18 <- function(IRdata) {
  # Percentage of women aged 20-49 who gave birth by age 18
  # Denominator: women aged 20+
  # Numerator: women aged 20+ with first birth before age 18 (v212 < 18)
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        v012 < 20 ~ NA_real_,  # Denominator is women aged 20+
        v212 < 18 ~ 1,  # Had first birth before age 18
        v212 >= 18 | is.na(v212) ~ 0,  # First birth at 18+ or never gave birth
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
