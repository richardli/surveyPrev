#' Population age 6 and over with completed secondary education
#'
#' Percentage of the de facto household population age 6 and over with some secondary education
#'
#' @param BRdata A DHS Births Recode (BR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_B_CSC <- function(BRdata) {
  # Percentage of population age 6+ with completed secondary education
  # BRdata: one row per birth; use child's age and mother's education as proxy
  # v149: 0=no, 1=incomplete primary, 2=complete primary, 3=incomplete secondary,
  #       4=complete secondary, 5=higher, 8=DK, 9=missing
  # "Completed secondary" includes those with higher education (who completed secondary first)
  
  result <- BRdata %>%
    mutate(age_years = (v008 - b3) / 12) %>%
    filter(
      !is.na(age_years),
      age_years >= 6            # Child is age 6 or older
    ) %>%
    mutate(
      value = case_when(
        is.na(v149) ~ NA_real_,
        v149 %in% c(8, 9) ~ NA_real_,  # Don't know or missing
        v149 %in% c(4, 5) ~ 1,  # Completed secondary or higher
        v149 %in% c(0, 1, 2, 3) ~ 0,  # Other education levels
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
