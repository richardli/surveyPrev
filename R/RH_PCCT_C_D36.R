#' Newborn's first postnatal checkup: 3-6 days
#'
#' Percentage of last births in the two years preceding the survey who had their first postnatal checkup 3-6 days after birth
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCT_C_D36 <- function(KRdata) {
  # Denominator: Most recent live birth in last 24 months (bidx==1 & v008-b3<24)
  # Numerator: First postnatal check at 3-6 days after birth
  # First check timing: m75 if checked before discharge, else m71
  # Days 3-6: codes 203-206
  
  result <- KRdata %>%
    filter(bidx == 1, v008 - b3 < 24) %>%
    mutate(
      first_check_timing = case_when(
        m74 == 1 & !is.na(m75) ~ m75,
        m74 != 1 & m70 == 1 & !is.na(m71) ~ m71,
        TRUE ~ NA_real_
      ),
      value = case_when(
        !is.na(first_check_timing) & first_check_timing %in% 203:206 ~ 1,
        !is.na(first_check_timing) & first_check_timing < 900 ~ 0,
        m70 == 0 & (is.na(m74) | m74 != 1) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
