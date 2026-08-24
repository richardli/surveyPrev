#' Met need for family planning (currently using), total
#'
#' Percentage of currently married or in union women using family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADM_W_MNT <- function(IRdata) {
  # Denominator: All currently married women (v502 == 1)
  # Numerator: Women with met need for family planning (v626a in {3, 4})
  # All women without met need (including missing v626a, not sexually active,
  # infecund, etc.) are coded as 0
  
  result <- IRdata %>%
    filter(v502 == 1) %>%
    mutate(
      value = case_when(
        !is.na(v626a) & v626a %in% c(3, 4) ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
