#' Knowledge of any method of contraception (married women)
#'
#' Percentage of currently married or in union women who know any method of contraception
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_KMTM_W_ANY <- function(IRdata) {
  # FP_KMTM_W_ANY: Knowledge of any method of contraception (married women)
  # Numerator: Currently married women who know at least one contraceptive method
  # Denominator: Currently married women (v502 = 1)
  
  # Filter to currently married women
  df <- IRdata %>%
    filter(v502 == 1)
  
  # Get all v304 variables present in the dataset
  v304_vars <- grep("^v304_", names(df), value = TRUE)
  
  # Create indicator: knows any method if ANY v304 variable = 1 or 2
  # Use apply with MARGIN=1 to check row-wise if any v304 variable is 1 or 2
  if (length(v304_vars) > 0) {
    v304_matrix <- as.matrix(df[, v304_vars, drop = FALSE])
    df$knows_any <- apply(v304_matrix, 1, function(row) {
      any(row %in% c(1, 2), na.rm = TRUE)
    })
  } else {
    df$knows_any <- NA
  }
  
  df$value <- as.numeric(df$knows_any)
  
  # Preserve survey design columns
  result <- df %>%
    select(
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
