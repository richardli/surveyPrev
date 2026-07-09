#' Male population age 6 and over with some primary education
#'
#' Percentage of the de facto male household population age 6 and over with no education education
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_EDAT_M_SPR <- function(MRdata) {
  # Male population age 6 and over with some primary education
  # Denominator: all men in MRdata (age 15+, which satisfies age 6+)
  # Numerator: primary as highest level attended (mv106 == 1)
  # Weight: mv005 / 1,000,000
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv106 == 1 ~ 1,  # primary as highest level
        mv106 %in% c(0, 2, 3) ~ 0,  # no education, secondary, or higher
        TRUE ~ NA_real_  # missing/don't know
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
