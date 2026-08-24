#' Knowledge of any method of contraception (all men)
#'
#' Percentage of men who know any method of contraception
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_KMTA_M_ANY <- function(MRdata) {
  # FP_KMTA_M_ANY: Knowledge of any method of contraception (all men)
  # Denominator: All men age 15-49
  # Numerator: Men who know at least one contraceptive method
  
  # Filter to men age 15-49
  df <- MRdata %>%
    filter(mv012 >= 15 & mv012 <= 49)
  
  # Identify all mv304_* variables (knowledge of contraceptive methods)
  # mv304_1 to mv304_20 where 1 = knows spontaneously, 2 = knows after probing
  method_vars <- grep("^mv304_", names(df), value = TRUE)
  
  # Create indicator: knows any method if ANY mv304_* variable equals 1 or 2
  df <- df %>%
    mutate(
      knows_any = if_else(
        rowSums(across(all_of(method_vars), ~ .x %in% c(1, 2)), na.rm = TRUE) > 0,
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
      value = knows_any
    )
  
  return(result)
}
