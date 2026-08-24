#' Provider of newborns' first postnatal checkup: Doctor/nurse/midwife
#'
#' Percentage of last births in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from a doctor, nurse or midwife
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCP_C_DOC <- function(KRdata) {
  # Denominator: Most recent births in last 2 years (bidx==1, v008-b3<24)
  # Numerator: First PNC within 2 days from doctor/nurse/midwife
  # Two pathways: regular PNC (m71/m72) or discharge check (m75/m76)
  # Timing: 100-148 = 0-48 hours, 200-202 = 0-2 days
  # Provider: 11 = doctor, 12 = nurse/midwife
  
  result <- KRdata %>%
    filter(bidx == 1, v008 - b3 < 24) %>%
    mutate(
      # Regular PNC within 2 days by doc/nurse/midwife
      pnc_qual = !is.na(m71) & !is.na(m72) & 
                 ((m71 >= 100 & m71 <= 148) | (m71 >= 200 & m71 <= 202)) &
                 m72 %in% c(11, 12),
      
      # Discharge check within 2 days by doc/nurse/midwife  
      discharge_qual = !is.na(m75) & !is.na(m76) & 
                       ((m75 >= 100 & m75 <= 148) | (m75 >= 200 & m75 <= 202)) &
                       m76 %in% c(11, 12),
      
      # Value: 1 if either pathway qualifies
      value = as.integer(pnc_qual | discharge_qual)
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
