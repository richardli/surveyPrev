#' Total demand for family planning
#'
#' Total demand for family planning is the sum of unmet need for family planning and those currently using family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADM_W_TDT <- function(IRdata) {
  # FP_NADM_W_TDT: Total demand for family planning
  # Definition: Sum of unmet need + met need (v626a in 1,2,3,4)
  # Population: Currently married women (v502 = 1)
  
  design_cols <- c("caseid", "v001", "v002", "v005", "v021", "v022", "v023", "v024", "v025")
  keep_cols <- design_cols[design_cols %in% names(IRdata)]
  
  result <- IRdata %>%
    filter(v502 == 1) %>%  # Currently married women
    mutate(
      value = case_when(
        v626a %in% c(1, 2, 3, 4) ~ 1,
        v626a == 99 ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(all_of(keep_cols), value)
  
  return(result)
}
