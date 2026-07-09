#' Iron supplementation: none
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who took no iron containing supplements during the pregnancy of the most recent live birth (or stillbirth)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ICSP_W_TNI <- function(IRdata) {
  # Denominator: Women with a birth in last 5 years (v208 > 0)
  # Numerator: Took NO iron supplements during pregnancy of most recent birth
  # Variable: m46_1 = days of iron taken (0=none, 1-900=days, 998/999=DK/missing, NA=not asked)
  # Include both explicit "none" (0) and skip pattern (NA) as numerator
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        v208 > 0 ~ case_when(
          m46_1 %in% c(998, 999) ~ NA_real_,  # Don't know/missing - exclude from analysis
          m46_1 == 0 ~ 1,  # Explicitly no iron supplements
          is.na(m46_1) ~ 1,  # Question not asked (likely no ANC) - count as no iron
          m46_1 > 0 & m46_1 < 900 ~ 0,  # Some iron supplements
          TRUE ~ NA_real_
        ),
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
