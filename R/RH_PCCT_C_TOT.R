#' Newborn's first postnatal checkup: Total
#'
#' Percentage of last births in the two years preceding the survey: Total
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCT_C_TOT <- function(KRdata) {
  # Percentage of last births in the two years preceding the survey: Total
  # This is the denominator for postnatal checkup timing indicators
  # All births in last 2 years = 100%
  # Note: value is in percentage scale (0-100)
  
  KRdata %>%
    filter(
      # Last birth in the 2 years before survey (24 months)
      v008 - b3 < 24
    ) %>%
    mutate(
      # All births in scope = 100% (value in percentage scale)
      value = 100
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
}
