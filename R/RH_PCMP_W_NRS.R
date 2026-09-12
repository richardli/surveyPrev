#' Provider of mothers' first postnatal checkup: Auxiliary nurse/midwife
#'
#' Percentage of women giving birth in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from an auxiliary nurse
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_PCMP_W_NRS.R; validated on TZ_2022 (published 0.8, observed 0.008, abs_error 0.792).
#' @export
RH_PCMP_W_NRS <- function(IRdata) {
  # Denominator: Women with birth in last 2 years (24 months)
  # Numerator: First PNC within 2 days from auxiliary nurse/midwife (code 13)
  
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
        (timing_before <= 2 |
         (timing_before >= 100 & timing_before <= 199) |
         (timing_before >= 200 & timing_before <= 202)),
      
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
      
      # Value: 1 if auxiliary nurse/midwife (13), 0 otherwise
      value = case_when(
        !is.na(first_provider) & first_provider == 13 ~ 1,
        !is.na(first_provider) ~ 0,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
