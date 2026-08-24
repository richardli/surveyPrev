#' Unmet need for modern methods, total (all women)
#'
#' Percentage of women with an unmet need for modern methods
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADA_W_UMT <- function(IRdata) {
  # Unmet need for modern methods, total (all women)
  # Numerator: Women with standard unmet need OR using traditional/folkloric methods
  # Denominator: All fecund women 15-49 (excludes infecund)
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        # Exclude infecund women from calculation
        v624 == 7 ~ NA_real_,
        # Missing values
        is.na(v624) | v624 >= 98 | is.na(v313) | v313 >= 98 ~ NA_real_,
        # Standard unmet need for spacing or limiting
        v624 %in% c(1, 2) ~ 1,
        # Using traditional or folkloric method
        v313 %in% c(1, 2) ~ 1,
        # No unmet need for modern methods
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
