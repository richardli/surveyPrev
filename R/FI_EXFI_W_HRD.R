#' Women who have ever heard of fistula symptoms
#'
#' Percentage of women who have ever heard of fistula symptoms
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FI_EXFI_W_HRD <- function(IRdata) {
  # Denominator: All women 15-49 (default IR frame)
  # Numerator: Ever heard of fistula (country-specific variable, typically s1102)
  
  var_name <- NULL
  
  # Try s1102 first (from seed code)
  if ("s1102" %in% colnames(IRdata)) {
    var_name <- "s1102"
  } else {
    # Search by label for "heard" and "fistula"
    var_labels <- sapply(IRdata, function(x) attr(x, "label"))
    heard_idx <- grepl("(heard|aware).*(fistula|vesicovaginal|rectovaginal)", 
                       var_labels, ignore.case = TRUE)
    if (any(heard_idx)) {
      var_name <- names(var_labels)[heard_idx][1]
    }
  }
  
  if (!is.null(var_name)) {
    result <- IRdata %>%
      mutate(value = case_when(
        is.na(!!sym(var_name)) ~ NA_real_,
        !!sym(var_name) %in% c(8, 9, 98, 99) ~ NA_real_,
        !!sym(var_name) == 1 ~ 1,
        TRUE ~ 0
      ))
  } else {
    result <- IRdata %>%
      mutate(value = NA_real_)
  }
  
  result <- result %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
