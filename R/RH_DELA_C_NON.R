#' Assistance during delivery: No one
#'
#' Percentage of live births (or stillbirths) in the two (or three/five) years preceding the survey assisted by no one
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_DELA_C_NON <- function(KRdata) {
  # RH_DELA_C_NON: Percentage of live births in the X years preceding the survey 
  # assisted by no one during delivery
  
  # Determine reference period based on b19 (age of child in months)
  max_b19 <- max(KRdata$b19, na.rm = TRUE)
  if (max_b19 >= 50) {
    period_months <- 60  # 5 years
  } else if (max_b19 >= 30) {
    period_months <- 36  # 3 years
  } else {
    period_months <- 24  # 2 years
  }
  
  # Filter to births in the reference period
  df <- KRdata %>%
    filter(b19 < period_months)
  
  # m3n = 1 indicates no one assisted at delivery
  if ("m3n" %in% names(df)) {
    df <- df %>%
      mutate(
        no_one = case_when(
          m3n == 1 ~ 1,
          m3n == 0 ~ 0,
          TRUE ~ NA_real_
        )
      )
  } else {
    # If m3n doesn't exist, check if all other m3 variables are 0
    m3_cols <- grep("^m3[a-z]$", names(df), value = TRUE)
    df <- df %>%
      mutate(
        any_assistance = rowSums(select(., all_of(m3_cols)) == 1, na.rm = TRUE),
        no_one = case_when(
          any_assistance == 0 ~ 1,
          any_assistance > 0 ~ 0,
          TRUE ~ NA_real_
        )
      )
  }
  
  # Calculate percentage (multiply by 100 to convert proportion to percent)
  df <- df %>%
    mutate(value = no_one * 100)
  
  # Preserve survey design columns and return one row per birth
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
