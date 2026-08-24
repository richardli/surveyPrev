#' No postnatal checkup for mother within first two days of birth
#'
#' Percentage of women giving birth in the two years preceding the survey who received no postnatal checkup in the first two days after birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMP_W_NON <- function(IRdata) {
  # Denominator: women with birth in last 24 months (2 years)
  # Numerator: NO postnatal check in first 2 days after birth
  # PNC timing codes: <=202 means within first 2 days
  
  result <- IRdata %>%
    mutate(
      # Denominator: recent birth using standard IR variables
      recent_birth = !is.na(b3_01) & (v008 - b3_01) < 24,
      
      # Had PNC within first 2 days
      # Check before discharge (m62_1=yes, m63_1<=202)
      pnc_before_2d = (!is.na(m62_1) & m62_1 == 1 & !is.na(m63_1) & m63_1 <= 202),
      # Check after discharge (m66_1=yes, m67_1<=202)
      pnc_after_2d = (!is.na(m66_1) & m66_1 == 1 & !is.na(m67_1) & m67_1 <= 202),
      
      # Had any PNC in first 2 days
      had_pnc_2days = (pnc_before_2d | pnc_after_2d),
      
      # Value: NO PNC in first 2 days (inverse)
      value = case_when(
        recent_birth ~ as.numeric(!had_pnc_2days),
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
