#' Children with fever for whom advice or treatment was sought
#'
#' Among children under age five with fever in the two weeks preceding the survey, the percentage for whom advice or treatment was sought
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ML_FEVT_C_ADV <- function(KRdata) {
  # Children with fever for whom advice or treatment was sought
  # Among children under age 5 with fever in the two weeks preceding the survey,
  # the percentage for whom advice or treatment was sought (excluding traditional practitioner)
  
  # Identify all h32* variables (advice/treatment sources)
  h32_vars <- grep("^h32[a-w]", names(KRdata), value = TRUE)
  # Explicitly exclude h32t (traditional practitioner) and h32x (other - may need inclusion)
  h32_vars <- setdiff(h32_vars, "h32t")
  
  df <- KRdata %>%
    filter(
      b5 == 1,      # Child is alive
      b19 < 60,     # Under age 5 (< 60 months)
      h22 == 1      # Had fever in last 2 weeks
    ) %>%
    mutate(
      # Check if advice/treatment was sought from any valid source
      sought_advice = if (length(h32_vars) > 0) {
        h32_data <- select(., all_of(h32_vars))
        # For each row, check if ANY h32 variable is explicitly coded as 1
        # Treat NA as "not sought from that source"
        apply(h32_data, 1, function(row) {
          if (all(is.na(row))) {
            return(NA_real_)
          }
          any(row == 1, na.rm = TRUE)
        })
      } else {
        NA_real_
      },
      value = as.numeric(sought_advice)
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(df)
}
