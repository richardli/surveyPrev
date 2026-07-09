#' Antenatal care provider: Nurse/midwife
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey whose source of antenatal care during the pregnancy for the most recent live birth was a nurse or midwife
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_NRS <- function(IRdata) {
  # Antenatal care provider: Nurse/midwife (most recent birth)
  # Denominator: women with recent birth in last 24 months
  # Numerator: highest-qualified ANC provider was nurse/midwife (m2b_1=1, with m2a_1!=1)
  
  result <- IRdata %>%
    mutate(
      # Denominator: recent birth within 24 months
      # v208 > 0: had birth in last 5 years
      # v008 - b3_01 < 24: most recent birth within 24 months
      in_denom = !is.na(v208) & v208 > 0 & 
                 !is.na(v008) & !is.na(b3_01) & 
                 (v008 - b3_01) < 24,
      
      # Numerator: cascade by provider qualification level
      # Doctor (m2a_1) > Nurse/midwife (m2b_1) > other
      value = case_when(
        !in_denom ~ NA_real_,
        m2a_1 == 1 ~ 0,        # Doctor (higher qualification)
        m2b_1 == 1 ~ 1,        # Nurse/midwife
        TRUE ~ 0                # Other provider or no ANC
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
