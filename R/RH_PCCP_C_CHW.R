#' Provider of newborns' first postnatal checkup: Community health worker
#'
#' Percentage of last births in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from a community health worker
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCP_C_CHW <- function(KRdata) {
  # Percentage of last births in the two years preceding the survey who received
  # their first postnatal checkup in the first two days after birth from a
  # community health worker
  
  # The KR recode file has one row per child born in last 5 years.
  # We need to identify the most recent birth in the last 2 years.
  # Variables are prefixed with b* (child-level) not m* (which is in IR/NR files)
  
  # Key variables in KR:
  # - b19: Current age of child in months (derived from date of birth)
  # - m70: Baby postnatal check within 2 months
  # - m71: Time after delivery postnatal check took place
  # - m72: Person who performed postnatal checkup
  # - m74: Child's health checked before discharge
  # - m75: How long after delivery child's health check took place
  # - m76: Who checked child health before discharge
  
  result <- KRdata %>%
    mutate(
      # Calculate months since birth
      months_since_birth = v008 - b3,
      
      # Filter to most recent birth in last 2 years (< 24 months)
      # Group by caseid to identify most recent birth per woman
      is_eligible = months_since_birth < 24
    ) %>%
    filter(is_eligible) %>%
    group_by(caseid) %>%
    filter(b3 == max(b3, na.rm = TRUE)) %>%  # Most recent birth
    ungroup() %>%
    mutate(
      # Determine if newborn had postnatal check within 2 days
      # m71 codes: 100-199 = hours, 200-299 = days
      # Within 2 days means: hours (100-199) or days 0-2 (200-202)
      pnc_within_2days = case_when(
        m70 == 1 & (inrange(m71, 100, 199) | inrange(m71, 200, 202)) ~ 1,
        m74 == 1 & (inrange(m75, 100, 199) | inrange(m75, 200, 202)) ~ 1,
        TRUE ~ 0
      ),
      
      # Provider was community health worker (code 22)
      # Check both m72 (postnatal checkup provider) and m76 (before discharge)
      provider_chw = case_when(
        pnc_within_2days == 1 & (m72 == 22 | m76 == 22) ~ 1,
        pnc_within_2days == 1 & (!is.na(m72) | !is.na(m76)) ~ 0,
        TRUE ~ 0
      ),
      
      # Final indicator value
      value = provider_chw
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(result)
}
