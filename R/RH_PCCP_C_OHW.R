#' Provider of newborns' first postnatal checkup: Other health worker
#'
#' Percentage of last births in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from another type of health worker
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCP_C_OHW <- function(KRdata) {
  # RH_PCCP_C_OHW: Other health worker provided newborn's first postnatal checkup 
  # within first 2 days after birth
  # Denominator: Most recent births in 2 years before survey
  
  # Filter to most recent births in last 2 years
  df <- KRdata %>%
    filter(
      b19 < 24,  # Birth in last 2 years (months since birth)
      b9 == 0    # Most recent birth (b9 = 0 for births alive at interview)
    )
  
  # Identify postnatal check within first 2 days
  # m70 = baby postnatal check within 2 months (1=yes)
  # m71 = timing of postnatal check
  # m74 = child's health checked before discharge (1=yes)
  # m75 = timing of child's health check before discharge
  
  # Create indicator for check within 2 days
  df <- df %>%
    mutate(
      # Check within 2 days from m70/m71
      check_m70 = case_when(
        m70 == 1 & m71 >= 100 & m71 <= 102 ~ TRUE,  # Hours: 100-102 (0-2 hrs)
        m70 == 1 & m71 >= 200 & m71 <= 202 ~ TRUE,  # Days: 200-202 (0-2 days)
        TRUE ~ FALSE
      ),
      # Check before discharge from m74/m75
      check_m74 = case_when(
        m74 == 1 & m75 >= 100 & m75 <= 102 ~ TRUE,  # Hours: 100-102
        m74 == 1 & m75 >= 200 & m75 <= 202 ~ TRUE,  # Days: 200-202
        TRUE ~ FALSE
      ),
      # Had any check within 2 days
      had_check_2days = check_m70 | check_m74
    )
  
  # Identify provider type
  # m72 = person who performed postnatal checkup (for m70 pathway)
  # m76 = who checked child health before discharge (for m74 pathway)
  # Other health worker codes: typically include auxiliary health workers, 
  # community volunteers, other non-professional health workers
  # Codes vary but typically: 23-29, 96 (other), excluding 11-13, 21-22
  
  df <- df %>%
    mutate(
      # Provider from m72 (m70 pathway)
      provider_m72 = case_when(
        !is.na(m72) & had_check_2days & check_m70 ~ m72,
        TRUE ~ NA_real_
      ),
      # Provider from m76 (m74 pathway)
      provider_m76 = case_when(
        !is.na(m76) & had_check_2days & check_m74 ~ m76,
        TRUE ~ NA_real_
      ),
      # Use m72 if available, otherwise m76
      provider = coalesce(provider_m72, provider_m76),
      # Other health worker: exclude doctor/nurse/midwife (11-13),
      # TBA (21), CHW (22), and code as "other" for remaining health workers
      value = case_when(
        !had_check_2days ~ 0,  # No check within 2 days
        is.na(provider) ~ NA_real_,  # Missing provider info
        provider %in% c(11, 12, 13, 21, 22) ~ 0,  # Specific categories
        provider >= 96 ~ 1,  # "Other" code
        provider >= 23 & provider <= 29 ~ 1,  # Other health worker codes
        TRUE ~ 0
      )
    )
  
  # Return with survey design columns
  result <- df %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(as.data.frame(result))
}
