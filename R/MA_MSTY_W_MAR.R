#' Current marital status [Young women]: Married
#'
#' Percentage of young women age 15-19, 20-24, or 15-24 married
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
MA_MSTY_W_MAR <- function(IRdata) {
  # Denominator: Young women age 15-19
  # Numerator: Currently married (v502 == 1)
  # Weight: v005/1e6
  
  result <- IRdata %>%
    filter(v012 >= 15 & v012 <= 19) %>%
    mutate(
      value = case_when(
        v502 == 1 ~ 1,
        v502 %in% c(0, 2, 3, 4, 5) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
