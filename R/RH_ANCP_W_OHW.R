#' Antenatal care provider: Other health worker
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey whose source of antenatal care during the pregnancy for the most recent live birth was another type of health worker
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_OHW <- function(IRdata) {
  # Denominator: Women with a live birth in the 2 years preceding the survey
  # Numerator: ANC provider was other health worker (m2d_1, m2e_1, or m2f_1 = 1)
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        # Not in denominator (no recent birth in last 24 months)
        is.na(v208) | v208 == 0 | is.na(b3_01) | (v008 - b3_01 >= 24) ~ NA_real_,
        
        # Other health worker provided ANC (country-specific health professionals)
        m2d_1 == 1 | m2e_1 == 1 | m2f_1 == 1 ~ 1,
        
        # In denominator but different provider
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
