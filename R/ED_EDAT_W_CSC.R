#' Female population age 6 and over with completed secondary education
#'
#' Percentage of the de facto female household population age 6 and over with some secondary education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_W_CSC <- function(IRdata) {
  # Percentage of de facto female household population age 6 and over 
  # with completed secondary education
  
  # Filter: females age 6+
  df <- IRdata %>%
    filter(v012 >= 6)
  
  # Education level: v106
  # 0 = No education
  # 1 = Primary
  # 2 = Secondary
  # 3 = Higher
  # 8 = Don't know
  # 9 = Missing
  
  # Years at level: v107
  
  # Completed secondary means:
  # - Attended secondary (v106 == 2) AND completed all years, OR
  # - Attended higher education (v106 == 3)
  
  # Need to determine years required for secondary completion
  # This is country-specific but typically ranges from 4-7 years
  # Check the distribution of v107 for those with v106==2
  
  df <- df %>%
    mutate(
      # Completed secondary if:
      # 1. Attended higher education (v106 == 3), OR
      # 2. Attended secondary (v106 == 2) and completed maximum years
      # For most countries, secondary completion is 6-7 years
      # We'll use v107 >= 6 for secondary level as threshold for completion
      value = case_when(
        v106 == 3 ~ 1,  # Higher education = completed secondary
        v106 == 2 & v107 >= 6 ~ 1,  # Secondary with 6+ years = completed
        v106 %in% c(0, 1) ~ 0,  # No education or primary = not completed
        v106 == 2 & v107 < 6 ~ 0,  # Secondary but not enough years
        v106 %in% c(8, 9) ~ NA_real_,  # Don't know/missing
        TRUE ~ 0
      )
    )
  
  # Preserve survey design columns
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
