#' Antenatal care provider: Doctor
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey whose source of antenatal care during the pregnancy for the most recent live birth was a doctor
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_DOC <- function(IRdata) {
  # Denominator: Women with live birth in last 2 years (24 months)
  # Standard IR pattern: v208 > 0 & v008 - b3_01 < 24
  # Numerator: ANC from doctor for most recent birth (m2a_1 == 1)
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        # Not in denominator: no recent birth in last 24 months
        is.na(v208) | v208 == 0 | is.na(b3_01) | (v008 - b3_01) >= 24 ~ NA_real_,
        # In denominator, saw doctor
        !is.na(m2a_1) & m2a_1 == 1 ~ 1,
        # In denominator, did not see doctor
        !is.na(m2a_1) & m2a_1 != 1 ~ 0,
        # In denominator, m2a_1 missing
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
