#' Mother's first postnatal checkup: Total
#'
#' Percentage of women giving birth in the two years preceding the survey: Total
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMT_W_TOT <- function(IRdata) {
  # RH_PCMT_W_TOT: Percentage of women giving birth in the two years preceding the survey
  # This is a denominator/base indicator - should be 100% by definition
  # It represents all women with births in the 2 years before survey
  
  IRdata %>%
    mutate(
      # Calculate time since birth in months for most recent birth
      time_since_birth = v008 - b3_01,
      
      # Women with births in the 2 years (24 months) preceding survey
      recent_birth = case_when(
        is.na(b3_01) ~ 0,
        time_since_birth < 24 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(recent_birth == 1) %>%
    mutate(
      value = 100  # All women in denominator = 100%
    ) %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
}
