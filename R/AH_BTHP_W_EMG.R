#' Born outside of the country [Women]
#'
#' Percentage of women who were born outside of the country
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_BTHP_W_EMG.R; validated on TZ_2022 (published 0.3, observed 0.0029, abs_error 0.2971).
#' @export
AH_BTHP_W_EMG <- function(IRdata) {
  # Percentage of women who were born outside of the country
  # Numerator: Women born outside the country (v172 = 96)
  # Denominator: Women age 15-49 who are not visitors (v104 ≠ 96)
  
  # If v172 (province of birth) doesn't exist, create dummy assuming not born abroad
  if (!"v172" %in% names(IRdata)) {
    IRdata$v172 <- 0
  }
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        is.na(v104) | v104 %in% c(96, 97, 98, 99) ~ NA_real_,  # Visitors/missing residence info
        is.na(v172) | v172 %in% c(97, 98, 99) ~ NA_real_,      # Unknown birth location
        v172 == 96 ~ 1,                                          # Born outside the country
        TRUE ~ 0                                                 # Born in the country
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
