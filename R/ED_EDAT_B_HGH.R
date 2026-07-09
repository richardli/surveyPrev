#' Population age 6 and over who attended higher education
#'
#' Percentage of the de facto household population age 6 and over who attended higher education
#'
#' @param BRdata A DHS Births Recode (BR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_B_HGH <- function(BRdata) {
  # Percentage of population age 6+ who attended higher education
  # v106: 0=no education, 1=primary, 2=secondary, 3=higher, 8=DK, 9=missing
  
  result <- BRdata %>%
    filter(!is.na(v012), v012 >= 6) %>%
    mutate(
      value = case_when(
        is.na(v106) ~ NA_real_,
        v106 %in% c(8, 9) ~ NA_real_,  # Don't know or missing
        v106 == 3 ~ 1,  # Higher education
        v106 %in% c(0, 1, 2) ~ 0,  # No education, primary, or secondary
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
