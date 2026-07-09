#' Mother's first postnatal checkup: 7-41 days
#'
#' Percentage of women giving birth in the two years preceding the survey who had their first postnatal checkup 7-41 days after birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMT_W_D7P <- function(IRdata) {
  # Denominator: Women with live birth in last 24 months (2 years)
  # Numerator: First postnatal checkup 7-41 days after birth
  # Timing codes: 207-241 = days 7-41
  
  result <- IRdata %>%
    filter(!is.na(v208) & v208 > 0 & !is.na(b3_01) & (v008 - b3_01) < 24) %>%
    mutate(
      timing_before = case_when(
        !is.na(m62_1) & m62_1 == 1 & !is.na(m63_1) & m63_1 < 998 ~ m63_1,
        TRUE ~ NA_real_
      ),
      timing_after = case_when(
        !is.na(m66_1) & m66_1 == 1 & !is.na(m67_1) & m67_1 < 998 ~ m67_1,
        TRUE ~ NA_real_
      ),
      
      # First check is earliest of the two
      first_check = case_when(
        !is.na(timing_before) & !is.na(timing_after) ~ 
          pmin(timing_before, timing_after),
        !is.na(timing_before) ~ timing_before,
        !is.na(timing_after) ~ timing_after,
        TRUE ~ NA_real_
      ),
      
      # Value: 1 if first check was 7-41 days (codes 207-241)
      value = case_when(
        is.na(first_check) ~ NA_real_,
        first_check %in% 207:241 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
