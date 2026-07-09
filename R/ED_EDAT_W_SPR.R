#' Female population age 6 and over with some primary education
#'
#' Percentage of the de facto female household population age 6 and over with no education education
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_W_SPR <- function(IRdata) {
  # Percentage of de facto female household population age 6 and over 
  # with some primary education (attended primary level)
  
  # Filter: females age 6+
  df <- IRdata %>%
    filter(v012 >= 6)
  
  # v106: Highest education level attended
  # 0 = No education
  # 1 = Primary
  # 2 = Secondary
  # 3 = Higher
  # 9 = Missing/DK
  
  # "Some primary" = v106==1 (attended primary level)
  df <- df %>%
    mutate(
      value = case_when(
        v106 == 1 ~ 1,                      # Attended primary
        v106 %in% c(0, 2, 3) ~ 0,           # Other valid education categories
        v106 == 9 | is.na(v106) ~ NA_real_, # Missing/DK
        TRUE ~ NA_real_
      )
    )
  
  # Keep survey design columns
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(as.data.frame(result))
}
