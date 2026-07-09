#' Knowledge of any method of contraception (married men)
#'
#' Percentage of currently married or in union men who know any method of contraception
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_KMTM_M_ANY <- function(MRdata) {
  # Percentage of currently married or in union men who know any method of contraception
  # Denominator: Currently married men (mv502 = 1)
  # Numerator: Know at least one method from mv304_1 to mv304_20 (value = 1 or 2)
  
  # Filter to currently married men
  df <- MRdata %>%
    filter(mv502 == 1) %>%
    select(
      mv001, mv002, mv005, mv022, mv023, mv024, mv025,
      starts_with("mv304_")
    )
  
  # Identify all mv304_* variables (contraceptive knowledge)
  mv304_vars <- names(df)[grepl("^mv304_", names(df))]
  
  # Create indicator: knows any method if any mv304_* variable equals 1 or 2
  df <- df %>%
    mutate(
      knows_any = if_else(
        rowSums(across(all_of(mv304_vars), ~ .x %in% c(1, 2)), na.rm = TRUE) > 0,
        1,
        0
      )
    )
  
  # Return result with value column and survey design variables
  result <- df %>%
    transmute(
      v001 = mv001,
      v002 = mv002,
      v005 = mv005,
      v022 = mv022,
      v023 = mv023,
      v024 = mv024,
      v025 = mv025,
      value = knows_any
    )
  
  return(as.data.frame(result))
}
