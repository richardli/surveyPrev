#' Men with secondary or higher education
#'
#' Percentage of men with secondary or higher education
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_LITR_M_SCH <- function(MRdata) {
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv106 == 3 ~ 1,  # Higher than secondary education
        mv106 %in% c(0, 1, 2) ~ 0,  # No education, Primary, or Secondary
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(result)
}
