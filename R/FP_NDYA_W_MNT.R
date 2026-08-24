#' Met need for family planning (currently using), total (all young women)
#'
#' Percentage of young women age 15-24 using family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYA_W_MNT <- function(IRdata) {
  # Met need for family planning (currently using), total (all young women)
  # Denominator: All young women age 15-24
  # Numerator: Currently using family planning (any method)
  # v313: Current contraceptive method (0=not using, >0=using)
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        # Not in denominator (not age 15-24)
        !(v012 %in% 15:24) ~ NA_real_,
        # Missing data for contraceptive use
        is.na(v313) | v313 >= 90 ~ NA_real_,
        # Currently using any method
        v313 > 0 ~ 1,
        # Not using
        v313 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
