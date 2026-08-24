#' Mother's first postnatal checkup: don't know or missing
#'
#' Percentage of women giving birth in the two years preceding the survey with missing information on their first postnatal checkup after birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMT_W_DKM <- function(IRdata) {
  # Denominator: women with birth in last 2 years (24 months)
  # v008 - b3_01 < 24: most recent birth within 24 months
  
  # Numerator: had postnatal check but timing is don't know or missing
  # m62_1: checked before discharge
  # m63_1: timing before discharge
  # m66_1: checked after discharge/at home  
  # m67_1: timing after discharge/at home
  
  result <- IRdata %>%
    filter(!is.na(b3_01), v008 - b3_01 < 24) %>%
    mutate(
      # Determine if woman had any postnatal check
      had_pnc = (m62_1 == 1 | m66_1 == 1),
      
      # Get timing: priority to check before discharge (m63_1), else after (m67_1)
      timing = case_when(
        m62_1 == 1 ~ m63_1,
        m66_1 == 1 ~ m67_1,
        TRUE ~ NA_real_
      ),
      
      # Value = 1 if had PNC but timing is DK (998), missing (999), or NA
      value = case_when(
        had_pnc & (is.na(timing) | timing %in% c(998, 999)) ~ 1,
        had_pnc ~ 0,  # Had PNC with valid timing
        TRUE ~ 0      # No PNC (goes to "NON" category)
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
