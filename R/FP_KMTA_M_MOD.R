#' Knowledge of any modern method of contraception (all men)
#'
#' Percentage of men who know any modern method of contraception
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_KMTA_M_MOD <- function(MRdata) {
  # FP_KMTA_M_MOD: Knowledge of any modern method of contraception (all men)
  # Denominator: All men age 15-49
  # Numerator: Men who know at least one modern contraceptive method
  # Modern = any method EXCEPT traditional methods 8, 9, 10
  
  # Filter to men age 15-49
  df <- MRdata %>%
    filter(mv012 >= 15 & mv012 <= 49)
  
  # Identify all mv304_* variables (knowledge of contraceptive methods)
  method_vars <- grep("^mv304_", names(df), value = TRUE)
  
  # Traditional method indices (to EXCLUDE)
  traditional_indices <- c(8, 9, 10)
  traditional_vars <- paste0("mv304_", traditional_indices)
  
  # Modern methods = all methods except traditional
  modern_vars <- setdiff(method_vars, traditional_vars)
  
  # Create indicator: knows any modern method if ANY modern method variable equals 1 or 2
  df <- df %>%
    mutate(
      knows_modern = if_else(
        rowSums(across(all_of(modern_vars), ~ .x %in% c(1, 2)), na.rm = TRUE) > 0,
        1,
        0
      )
    )
  
  # Return results with value column and survey design variables
  result <- df %>%
    transmute(
      mv001,
      mv002,
      mv005,
      mv022,
      mv023,
      mv024,
      mv025,
      value = knows_modern
    )
  
  return(as.data.frame(result))
}
