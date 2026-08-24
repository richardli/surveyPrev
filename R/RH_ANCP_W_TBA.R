#' Antenatal care provider: Traditional birth attendant
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey whose source of antenatal care during the pregnancy for the most recent live birth was a traditional birth attendant
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_TBA <- function(IRdata) {
  # Denominator: women with birth in last 2 years (24 months)
  # b3_01: CMC of most recent birth
  # Numerator: received ANC from traditional birth attendant (m2g_1 = 1)
  
  result <- IRdata %>%
    filter(
      !is.na(b3_01),          # has most recent birth recorded
      v008 - b3_01 < 24       # birth in last 24 months (2 years)
    ) %>%
    mutate(
      value = case_when(
        m2g_1 == 1 ~ 1,           # TBA provided ANC
        m2g_1 == 0 ~ 0,           # TBA did not provide ANC
        is.na(m2g_1) | m2g_1 >= 8 ~ NA_real_,  # Missing/don't know
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
