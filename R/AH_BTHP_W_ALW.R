#' Always lived in current place of residence [Women]
#'
#' Percentage of women who have always lived in current place of residence
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_BTHP_W_ALW.R; validated on TZ_2022 (published 65.2, observed 65.1984, abs_error 0.0016).
#' @export
AH_BTHP_W_ALW <- function(IRdata) {
  # Percentage of women who have always lived in current place of residence
  # Numerator: Women age 15-49 born in current place (v104=95) and not born abroad (v172≠96)
  # Denominator: Women age 15-49 who are not visitors (v104≠96)
  
  # If v172 (province of birth) doesn't exist, create dummy assuming not born abroad
  if (!"v172" %in% names(IRdata)) {
    IRdata$v172 <- 0
  }
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        is.na(v104) | v104 %in% c(96, 97, 98, 99) ~ NA_real_,  # Visitors/missing residence info
        is.na(v172) | v172 %in% c(97, 98, 99) ~ NA_real_,      # Unknown birth location
        v104 == 95 & v172 != 96 ~ 1,                            # Always lived here & not born abroad
        TRUE ~ 0                                                 # Lived here N years (not always)
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
