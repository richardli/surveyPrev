#' Wife beating justified for at least one specific reason [Women]
#'
#' Percentage of women who agree that a husband is justified in hitting or beating his wife for at least one specific reason
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_AWBT_W_AGR <- function(IRdata) {
  # Wife beating justified for at least one specific reason [Women]
  # Percentage of women age 15-49 who agree that a husband is justified 
  # in hitting or beating his wife for at least one of the specified reasons
  
  # Filter to women age 15-49
  df <- IRdata %>%
    filter(v012 >= 15 & v012 <= 49)
  
  # Create binary indicator for "at least one reason"
  # 1 if ANY reason = 1
  # 0 if ALL reasons are 0 (and at least one is non-missing)
  # NA only if ALL reasons are missing
  df <- df %>%
    mutate(
      # Count how many are 1, 0, or missing
      n_yes = rowSums(across(c(v744a, v744b, v744c, v744d, v744e), ~ . == 1), na.rm = TRUE),
      n_no = rowSums(across(c(v744a, v744b, v744c, v744d, v744e), ~ . == 0), na.rm = TRUE),
      n_missing = rowSums(is.na(across(c(v744a, v744b, v744c, v744d, v744e)))),
      
      value = case_when(
        n_yes >= 1 ~ 1,  # At least one yes
        n_missing == 5 ~ NA_real_,  # All missing
        n_no >= 1 ~ 0,  # At least one explicit no and no yes
        TRUE ~ NA_real_
      )
    )
  
  # Select relevant columns
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
