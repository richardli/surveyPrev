#' Men who own a mobile phone
#'
#' Percentage of men who own a mobile phone
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_MOBB_M_MOB <- function(MRdata) {
  # Percentage of men age 15-49 who own a mobile phone
  # Numerator: Men who own a mobile phone (mv169a = 1)
  # Denominator: All men age 15-49
  
  result <- MRdata %>%
    filter(
      mv012 >= 15 & mv012 <= 49  # Men age 15-49
    ) %>%
    mutate(
      value = case_when(
        mv169a == 1 ~ 1,   # Owns a mobile phone
        mv169a == 0 ~ 0,   # Does not own a mobile phone
        TRUE ~ NA_real_    # Missing/don't know
      )
    ) %>%
    select(
      mv001,   # Cluster number
      mv002,   # Household number
      mv005,   # Sample weight
      mv022,   # Sample strata
      mv023,   # Stratification
      mv024,   # Region
      mv025,   # Urban/rural
      value
    )
  
  return(result)
}
