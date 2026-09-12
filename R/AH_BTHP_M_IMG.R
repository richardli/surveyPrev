#' Born in the country but outside of current place of residence [Men]
#'
#' Percentage of men who were born in the country but outside of current place of residence
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_BTHP_M_IMG.R; validated on TZ_2022 (published 36.2, observed 36.2172, abs_error 0.0172).
#' @export
AH_BTHP_M_IMG <- function(MRdata) {
  # MR recode -> mv* prefix throughout. Weight is mv005/1e6.
  # Denominator: Men age 15-49 who are not visitors (mv104 ≠ 96).
  # Numerator: Born in country but outside current place of residence
  #            (mv104 not in 95:99 AND mv172 ≠ 96).
  # mv104: 0-94=years lived, 95=always, 96=visitor, 97-99=missing
  # mv172: 0-95=province codes, 96=born outside, 97-99=missing
  
  # If mv172 doesn't exist, create dummy assuming not born abroad
  if (!"mv172" %in% names(MRdata)) {
    MRdata$mv172 <- 0
  }
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        # Exclude visitors from denominator
        mv104 == 96 ~ NA_real_,
        # Exclude missing data
        is.na(mv104) | mv104 %in% c(97, 98, 99) ~ NA_real_,
        is.na(mv172) | mv172 %in% c(97, 98, 99) ~ NA_real_,
        # Numerator: moved to current place (0-94 years) AND born in country (not 96)
        mv104 < 95 & mv172 != 96 ~ 1,
        # All others in valid denominator (always lived here OR born outside)
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
