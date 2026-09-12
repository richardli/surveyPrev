#' Always lived in current place of residence [Men]
#'
#' Percentage of men who have always lived in current place of residence
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_BTHP_M_ALW.R; validated on TZ_2022 (published 63.6, observed 63.6249, abs_error 0.0249).
#' @export
AH_BTHP_M_ALW <- function(MRdata) {
  # Percentage of men who have always lived in current place of residence
  # Numerator: Men age 15-49 born in current place (mv104=95) and not born abroad (mv172≠96)
  # Denominator: Men age 15-49 who are not visitors (mv104≠96)
  
  # If mv172 (province of birth) doesn't exist, create dummy assuming not born abroad
  if (!"mv172" %in% names(MRdata)) {
    MRdata$mv172 <- 0
  }
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        is.na(mv104) | mv104 %in% c(96, 97, 98, 99) ~ NA_real_,  # Visitors/missing residence info
        is.na(mv172) | mv172 %in% c(97, 98, 99) ~ NA_real_,      # Unknown birth location
        mv104 == 95 & mv172 != 96 ~ 1,                            # Always lived here & not born abroad
        TRUE ~ 0                                                   # Lived here N years (not always)
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
