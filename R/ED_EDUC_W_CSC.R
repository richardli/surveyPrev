#' Women with completed secondary education
#'
#' Percentage of women with completed secondary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDUC_W_CSC <- function(IRdata) {
  result <- IRdata %>%
    mutate(
      value = case_when(
        v106 == 2 & !is.na(v107) & v107 >= 5 ~ 1,
        !is.na(v106) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
