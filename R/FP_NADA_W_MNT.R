#' Met need for family planning (currently using), total (all women)
#'
#' Percentage of women using family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADA_W_MNT <- function(IRdata) {
  # FP_NADA_W_MNT: Met need for family planning (currently using), total (all women)
  # Percentage of ALL women who are currently using family planning
  
  # Numerator: Women currently using any contraceptive method (v626a in 3,4)
  # Denominator: All women
  
  IRdata %>%
    mutate(
      # Currently using contraception (met need)
      value = case_when(
        v626a %in% c(3, 4) ~ 1,
        v626a %in% c(1, 2) ~ 0,  # Unmet need
        is.na(v626a) ~ 0,         # No need/demand
        TRUE ~ 0
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
