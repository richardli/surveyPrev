#' Antenatal care provider: Community health worker
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey whose source of antenatal care during the pregnancy for the most recent live birth was a community health worker
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ANCP_W_CHW <- function(IRdata) {
  # Percentage of women with live birth/stillbirth in 2 years preceding survey
  # whose ANC provider was community health worker
  
  # The IR recode uses bidx_ prefix for birth history variables
  # We need to work with the most recent birth (bidx=01)
  
  # Create indicators from birth history variables
  df <- IRdata %>%
    mutate(
      # Most recent birth outcome (b19_01 is months since birth)
      recent_birth = if_else(!is.na(b19_01) & b19_01 < 24, 1, 0, missing = 0),
      
      # Check if received ANC from community health worker (m2i_1 for most recent birth)
      chw_anc = case_when(
        recent_birth == 1 & m2i_1 == 1 ~ 1,
        recent_birth == 1 & m2i_1 == 0 ~ 0,
        recent_birth == 1 & is.na(m2i_1) ~ NA_real_,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(recent_birth == 1) %>%
    mutate(value = chw_anc) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(df)
}
