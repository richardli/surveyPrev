#' Born outside of the country [Men]
#'
#' Percentage of men who were born outside of the country
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_BTHP_M_EMG.R; validated on TZ_2022 (published 0.2, observed 0.0016, abs_error 0.1984).
#' @export
AH_BTHP_M_EMG <- function(MRdata) {
  # Percentage of men age 15-49 who were born outside the country
  # Numerator: men born outside the country (mv172 = 96)
  # Denominator: men who are not visitors (mv104 != 96)
  
  # If mv172 (province of birth) doesn't exist, create dummy assuming not born abroad
  if (!"mv172" %in% names(MRdata)) {
    MRdata$mv172 <- 0
  }
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv104 == 96 ~ NA_real_,  # Visitors excluded from denominator
        is.na(mv104) | mv104 %in% c(97, 98, 99) ~ NA_real_,  # Missing residence info
        is.na(mv172) | mv172 %in% c(97, 98, 99) ~ NA_real_,  # Unknown birth location
        mv172 == 96 ~ 1,  # Born outside the country
        TRUE ~ 0  # Born in the country (valid province code 0-95)
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
