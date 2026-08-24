#' Any health insurance
#'
#' Percentage of women with any health insurance
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
AH_HINS_W_ANY <- function(IRdata) {
  # Percentage of women with any health insurance
  # Numerator: Women age 15-49 with any health insurance (v481=1 OR any v481a-z=1)
  # Denominator: All women age 15-49
  
  # Check if v481 (summary variable) exists
  if ("v481" %in% names(IRdata)) {
    result <- IRdata %>%
      mutate(
        value = case_when(
          v481 == 1 ~ 1,
          v481 == 0 ~ 0,
          TRUE ~ NA_real_
        )
      ) %>%
      select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
    
    return(result)
  }
  
  # Otherwise check for specific insurance type variables (v481a, v481b, etc.)
  insurance_vars <- grep("^v481[a-z]$", names(IRdata), value = TRUE)
  
  if (length(insurance_vars) > 0) {
    result <- IRdata %>%
      rowwise() %>%
      mutate(
        has_any = any(c_across(all_of(insurance_vars)) == 1, na.rm = TRUE),
        all_missing = all(is.na(c_across(all_of(insurance_vars)))),
        value = case_when(
          all_missing ~ NA_real_,
          has_any ~ 1,
          TRUE ~ 0
        )
      ) %>%
      ungroup() %>%
      select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
    
    return(result)
  }
  
  # If no health insurance variables exist, return all NA
  result <- IRdata %>%
    mutate(value = NA_real_) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(result)
}
