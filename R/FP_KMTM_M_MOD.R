#' Knowledge of any modern method of contraception (married men)
#'
#' Percentage of currently married or in union men who know any modern method of contraception
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_KMTM_M_MOD <- function(MRdata) {
  # Percentage of currently married men who know any modern method
  # Denominator: Currently married men (mv502 = 1)
  # Numerator: Know at least one modern method (any mv304_* except 8, 9, 10)
  # Traditional methods to exclude: 8=rhythm, 9=withdrawal, 10=other traditional
  
  # Filter to currently married men
  df <- MRdata %>%
    filter(mv502 == 1)
  
  # Identify all mv304_* variables in the data
  all_method_vars <- grep("^mv304_", names(df), value = TRUE)
  
  # Exclude traditional methods (8, 9, 10)
  traditional_vars <- c("mv304_8", "mv304_9", "mv304_10")
  modern_method_vars <- setdiff(all_method_vars, traditional_vars)
  
  # Create indicator: knows any modern method if any modern method variable equals 1 or 2
  df <- df %>%
    mutate(
      knows_modern = if_else(
        rowSums(across(all_of(modern_method_vars), ~ .x %in% c(1, 2)), na.rm = TRUE) > 0,
        1,
        0
      )
    )
  
  # Return result with survey design variables
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
