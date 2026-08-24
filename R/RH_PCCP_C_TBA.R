#' Provider of newborns' first postnatal checkup: Traditional birth attendant
#'
#' Percentage of last births in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from a traditional birth attendant
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCP_C_TBA <- function(KRdata) {
  # Denominator: Births in last 24 months (b19 < 24)
  # Numerator: First PNC within 2 days from TBA (code 21)
  # Scale: proportion [0,1]
  
  result <- KRdata %>%
    mutate(
      # Timing within 2 days: codes 100-202 (hours=100-199, days 0-2=200-202)
      timing_ok = (!is.na(m71) & m71 >= 100 & m71 <= 202) |
                  (!is.na(m75) & m75 >= 100 & m75 <= 202),
      
      # Provider TBA (code 21): check m64, m68, m72, m76 per definition
      provider_tba = (!is.na(m64) & m64 == 21) |
                     (!is.na(m68) & m68 == 21) |
                     (!is.na(m72) & m72 == 21) |
                     (!is.na(m76) & m76 == 21),
      
      # Value
      value = case_when(
        !is.na(b19) & b19 < 24 & timing_ok & provider_tba ~ 1,
        !is.na(b19) & b19 < 24 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
