#' Unmet need for modern methods (married young women)
#'
#' Percentage of currently married or in union young women age 15-24 with an unmet need for modern methods
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYM_W_UMT <- function(IRdata) {
  # Unmet need for modern methods among married young women age 15-24
  # Denominator: All currently married young women age 15-24 (including infecund)
  # Numerator: Women with unmet need OR using traditional methods
  
  result <- IRdata %>%
    filter(
      v502 == 1,        # Currently married or in union
      v012 %in% 15:24   # Age 15-24 (young women)
    ) %>%
    mutate(
      value = case_when(
        # Numerator: unmet need for spacing/limiting OR using traditional methods
        v626a %in% c(1, 2) ~ 1,
        v313 %in% c(1, 2) ~ 1,
        
        # Denominator: using modern, met need, infecund, or not using
        v626a %in% c(3, 4, 7) | v313 %in% c(0, 3) ~ 0,
        
        # Missing
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
