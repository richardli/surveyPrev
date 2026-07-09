#' Male population age 6 and over who attended primary education
#'
#' Percentage of the de facto male household population age 6 and over who attended primary education
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_M_PRI <- function(MRdata) {
  # Denominator: all men age 15+ in MRdata (satisfies age 6+ requirement)
  # Numerator: attended primary education = 1-7 years total education
  #   (Zambia primary cycle is 7 years)
  # Weight: mv005 / 1,000,000
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv133 >= 1 & mv133 <= 7 ~ 1,  # 1-7 years of education (primary level)
        mv133 == 0 | mv133 > 7 ~ 0,   # no education or beyond primary
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
