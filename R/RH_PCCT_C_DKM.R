#' Newborn's first postnatal checkup: don't know or missing
#'
#' Percentage of last births in the two years preceding the survey with missing information on their first postnatal checkup after birth
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCT_C_DKM <- function(KRdata) {
  # Denominator: Most recent live birth in last 24 months (bidx==1 & b19<24)
  # Numerator: First postnatal check timing is don't know or missing
  # First check timing: m75 if checked before discharge (m74==1), else m71 if m70==1
  # DK/missing codes: 199, 299, 399, 998, 999
  
  result <- KRdata %>%
    filter(bidx == 1, b19 < 24) %>%
    mutate(
      # Determine first check timing (priority: before discharge)
      first_check_timing = case_when(
        m74 == 1 ~ m75,
        m74 != 1 & m70 == 1 ~ m71,
        TRUE ~ NA_real_
      ),
      value = case_when(
        # Explicitly coded as DK/missing
        first_check_timing %in% c(199, 299, 399, 998, 999) ~ 1,
        # Valid timing code
        !is.na(first_check_timing) ~ 0,
        # No check or no data
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
