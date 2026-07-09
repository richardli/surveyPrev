#' Antenatal care from a skilled provider
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who received antenatal care during the pregnancy for the most recent live birth from a skilled provider
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_SKP <- function(IRdata) {
  # Denominator: Women with live birth in last 5 years (v208 > 0)
  # Numerator: Received ANC from skilled provider for most recent birth
  # Skilled providers: Doctor (m2a_1), Nurse/midwife (m2b_1), Auxiliary midwife (m2c_1)
  
  result <- IRdata %>%
    filter(v208 > 0) %>%
    mutate(
      value = case_when(
        # Skilled provider
        m2a_1 == 1 | m2b_1 == 1 | m2c_1 == 1 ~ 1,
        # Non-skilled provider or no ANC
        m2g_1 == 1 | m2h_1 == 1 | m2i_1 == 1 | m2j_1 == 1 | 
        m2k_1 == 1 | m2l_1 == 1 | m2m_1 == 1 | m2n_1 == 1 ~ 0,
        # Missing data
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
