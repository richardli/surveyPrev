#' No antenatal care
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who received no antenatal care during the pregnancy for the most recent live birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_NON <- function(IRdata) {
  # Denominator: women with a live birth in the last 24 months (2 years)
  # Numerator: those who received no antenatal care (m2n_1 = 1)
  # Reference period: 24 months based on definition "two years"
  
  result <- IRdata %>%
    mutate(
      # Recent birth: at least one birth in last 5 years AND most recent birth in last 24 months
      recent_birth = v208 > 0 & !is.na(b3_01) & (v008 - b3_01) < 24,
      # No ANC received
      value = case_when(
        !recent_birth ~ NA_real_,
        is.na(m2n_1) ~ NA_real_,
        m2n_1 %in% c(8, 9) ~ NA_real_,  # Don't know, missing
        m2n_1 == 1 ~ 1,  # No ANC
        TRUE ~ 0  # Had some ANC
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
