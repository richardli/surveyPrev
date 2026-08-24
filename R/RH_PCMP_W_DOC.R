#' Provider of mothers' first postnatal checkup: Doctor/nurse/midwife
#'
#' Percentage of women giving birth in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from a doctor, nurse or midwife
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMP_W_DOC <- function(IRdata) {
  # Denominator: Women with birth in last 2 years (24 months)
  # Numerator: First PNC within 2 days from doctor/nurse/midwife (codes 11, 12)
  # Reference period: 24 months (2 years)
  
  result <- IRdata %>%
    filter(!is.na(b3_01), v008 - b3_01 < 24) %>%
    mutate(
      # PNC before discharge (in facility)
      timing_before = case_when(
        m62_1 == 1 & !is.na(m63_1) ~ m63_1,
        TRUE ~ NA_real_
      ),
      provider_before = case_when(
        m62_1 == 1 & !is.na(m64_1) ~ m64_1,
        TRUE ~ NA_real_
      ),
      within_2days_before = !is.na(timing_before) & 
        (timing_before <= 2 |                          # days 0, 1, 2 direct
         (timing_before >= 100 & timing_before <= 199) | # hours (all hour codes)
         (timing_before >= 200 & timing_before <= 202)), # days 0, 1, 2 coded as 200+
      
      # PNC after discharge/at home
      timing_after = case_when(
        m66_1 == 1 & !is.na(m67_1) ~ m67_1,
        TRUE ~ NA_real_
      ),
      provider_after = case_when(
        m66_1 == 1 & !is.na(m68_1) ~ m68_1,
        TRUE ~ NA_real_
      ),
      within_2days_after = !is.na(timing_after) & 
        (timing_after <= 2 |
         (timing_after >= 100 & timing_after <= 199) |
         (timing_after >= 200 & timing_after <= 202)),
      
      # First PNC within 2 days (prioritize before-discharge)
      first_provider = case_when(
        within_2days_before ~ provider_before,
        within_2days_after ~ provider_after,
        TRUE ~ NA_real_
      ),
      
      # Value: 1 if doctor/nurse/midwife (11, 12), 0 otherwise
      value = case_when(
        !is.na(first_provider) & first_provider %in% c(11, 12) ~ 1,
        !is.na(first_provider) ~ 0,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
