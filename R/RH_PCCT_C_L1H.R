#' Newborn's first postnatal checkup: Less than 1 hour
#'
#' Percentage of last births in the two years preceding the survey who had their first postnatal checkup less than 1 hour after birth
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCT_C_L1H <- function(KRdata) {
  # RH_PCCT_C_L1H
  # Percentage of last births in the two years preceding the survey 
  # who had their first postnatal checkup less than 1 hour after birth
  
  df <- KRdata %>%
    mutate(
      # Century month code of birth
      b3 = ifelse(b3 >= 9000, NA_real_, b3),
      
      # Months since birth at interview
      months_since_birth = v008 - b3,
      
      # Filter to births in last 2 years (< 24 months)
      in_last_2y = !is.na(months_since_birth) & months_since_birth < 24
    ) %>%
    filter(in_last_2y) %>%
    group_by(caseid) %>%
    filter(months_since_birth == min(months_since_birth, na.rm = TRUE)) %>%
    slice(1) %>%
    ungroup()
  
  # Apply PNC timing logic
  df <- df %>%
    mutate(
      # Convert to numeric, handling special codes
      m70_clean = ifelse(m70 %in% c(0, 1), m70, NA_real_),
      m71_clean = ifelse(m71 >= 100 & m71 <= 399, m71, NA_real_),
      m74_clean = ifelse(m74 %in% c(0, 1), m74, NA_real_),
      m75_clean = ifelse(m75 >= 100 & m75 <= 399, m75, NA_real_),
      
      # Less than 1 hour: m71/m75 = 100 (immediately/0 hours)
      m71_less_1h = !is.na(m71_clean) & m71_clean == 100,
      m75_less_1h = !is.na(m75_clean) & m75_clean == 100,
      
      # Denominator: all births in scope (already filtered)
      # Numerator: first check < 1 hour
      value = case_when(
        m71_less_1h | m75_less_1h ~ 1,
        !is.na(m71_clean) | !is.na(m75_clean) ~ 0,
        (m70_clean == 1 | m74_clean == 1) ~ 0,
        m70_clean == 0 & (is.na(m74_clean) | m74_clean == 0) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Return with survey design variables
  result <- df %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(as.data.frame(result))
}
