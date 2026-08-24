#' Provider of mothers' first postnatal checkup: Traditional birth attendant
#'
#' Percentage of women giving birth in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from a traditional birth attendant
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMP_W_TBA <- function(IRdata) {
  # Denominator: Women with birth in last 2 years (24 months)
  # Numerator: First PNC within 2 days from traditional birth attendant (TBA = 21)
  
  result <- IRdata %>%
    mutate(
      # Recent birth: most recent birth in last 24 months
      recent_birth = !is.na(b3_01) & (v008 - b3_01) < 24,
      
      # PNC before discharge: had check, within 2 days, from TBA
      check_before = (!is.na(m62_1) & m62_1 == 1 &
                      !is.na(m63_1) & m63_1 < 203 &  # <203 = within 2 days (hours 100-199, days 200-202)
                      !is.na(m64_1) & m64_1 == 21),
      
      # PNC after discharge: had check, within 2 days, from TBA
      check_after = (!is.na(m66_1) & m66_1 == 1 &
                     !is.na(m67_1) & m67_1 < 203 &
                     !is.na(m68_1) & m68_1 == 21),
      
      # First check from TBA (before takes precedence if both exist)
      value = case_when(
        !recent_birth ~ NA_real_,
        check_before ~ 1,
        check_after ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
