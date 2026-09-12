#' Born in the country but outside of current place of residence [Women]
#'
#' Percentage of women who were born in the country but outside of current place of residence
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_BTHP_W_IMG.R; validated on TZ_2022 (published 34.5, observed 34.5067, abs_error 0.0067).
#' @export
AH_BTHP_W_IMG <- function(IRdata) {
  # Percentage of women who were born in the country but outside of current place of residence
  # Numerator: Women born in country but outside current place (v104 ≠ 95:99 and v172 ≠ 96)
  # Denominator: Women who are not visitors (v104 ≠ 96)
  
  # If v172 (province of birth) doesn't exist, create dummy assuming not born abroad
  if (!"v172" %in% names(IRdata)) {
    IRdata$v172 <- 0
  }
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        is.na(v104) | v104 %in% c(96, 97, 98, 99) ~ NA_real_,  # Visitors/missing residence info
        is.na(v172) | v172 %in% c(97, 98, 99) ~ NA_real_,      # Unknown birth location
        v104 == 95 ~ 0,                                          # Always lived here (born in current place)
        v172 == 96 ~ 0,                                          # Born outside the country
        TRUE ~ 1                                                 # Born in country but outside current place
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
