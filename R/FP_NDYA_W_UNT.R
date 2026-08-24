#' Unmet need for family planning, total (all young women)
#'
#' Percentage of young women age 15-24 with an unmet need for family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYA_W_UNT <- function(IRdata) {
  # Denominator: All young women age 15-24
  # Numerator: Women with unmet need for family planning, total (spacing or limiting)
  # v626a: unmet need (1=spacing, 2=limiting, 0=no unmet need, NA=not at risk)
  # Note: NAs are coded as 0 because women not at risk have no unmet need
  
  result <- IRdata %>%
    filter(v012 %in% 15:24) %>%
    mutate(
      value = case_when(
        v626a %in% c(1, 2) ~ 1,  # Unmet need for spacing or limiting
        TRUE ~ 0                  # No unmet need (includes v626a==0 and NAs)
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
