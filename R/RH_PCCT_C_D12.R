#' Newborn's first postnatal checkup: 1-2 days
#'
#' Percentage of last births in the two years preceding the survey who had their first postnatal checkup 1-2 days after birth
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCT_C_D12 <- function(KRdata) {
  # RH_PCCT_C_D12
  # Percentage of last births in the two years preceding the survey 
  # who had their first postnatal checkup 1-2 days after birth
  
  # Denominator: Most recent live birth in last 2 years (m80 = 1 & p19 < 24)
  # Note: In KR file, we filter to births in last 2 years based on b19 < 24
  # and most recent birth is represented by midx == 1
  
  df <- KRdata %>%
    filter(
      b19 < 24,        # Birth in last 2 years (months since birth)
      midx == 1        # Most recent birth (index child = mother's youngest)
    ) %>%
    mutate(
      # Determine if first postnatal check was 1-2 days after birth
      # Based on m71 (timing of postnatal check) and m70 (whether check occurred)
      
      # m70: Baby postnatal check within 2 months (1=Yes, 0=No)
      # m71: Time after delivery postnatal check took place
      # m74: Child's health checked before discharge
      # m75: How long after delivery child's health check took place before discharge
      
      # First check if baby had any postnatal check
      had_check = case_when(
        m70 == 0 ~ 0,  # No postnatal check
        m70 == 1 ~ 1,  # Had postnatal check
        TRUE ~ NA_real_
      ),
      
      # For those with a check, determine timing of FIRST check
      # Priority: check before discharge (m74/m75) vs. after discharge (m71)
      # The FIRST check is what matters
      
      # m71 codes (typical): 
      # 100s = hours, 200s = days, 300s = weeks
      # 1-2 days would be codes 201-202
      
      # m75 codes for check before discharge - similar structure
      
      first_check_1_2_days = case_when(
        # No check
        m70 == 0 ~ 0,
        
        # Had check before discharge
        m74 == 1 & m75 >= 201 & m75 <= 202 ~ 1,
        
        # Had check after discharge  
        m74 != 1 & m71 >= 201 & m71 <= 202 ~ 1,
        
        # Check at other time
        m70 == 1 ~ 0,
        
        TRUE ~ NA_real_
      ),
      
      value = first_check_1_2_days
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(df)
}
