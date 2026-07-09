#' Iron supplementation: 60-89 days
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who took iron containing supplements for 60-89 days during the pregnancy of the most recent live birth (or stillbirth)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ICSP_W_B69 <- function(IRdata) {
  # RH_ICSP_W_B69
  # Percentage of women who took iron supplements for 60-89 days during 
  # pregnancy of most recent live birth (or stillbirth) in the reference period
  
  # Reference period: 2 years (24 months) for recent births
  reference_period <- 24
  
  # Filter to women with a birth in the reference period
  # b3_01: CMC date of birth of most recent child
  # v008: CMC date of interview
  df <- IRdata %>%
    filter(!is.na(b3_01)) %>%
    filter((v008 - b3_01) < reference_period)
  
  # m46_1: Days iron supplements taken during most recent pregnancy
  # Values: 0-365 = actual days, 998 = Don't know, 999/missing = NA
  # Target: 60-89 days
  
  df <- df %>%
    mutate(
      value = case_when(
        is.na(m46_1) ~ NA_real_,
        m46_1 >= 998 ~ NA_real_,
        m46_1 >= 60 & m46_1 <= 89 ~ 1,
        m46_1 < 998 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Return with survey design columns
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(as.data.frame(result))
}
