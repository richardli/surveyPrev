#' Problems in accessing health care: Concern there may not be a female provider
#'
#' Percentage of women who reported they have big problems in concern there may not be a female provider for treatment for themselves when they are sick
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PAHC_W_FEM <- function(IRdata) {
  # Denominator: All women age 15-49 (no filter needed)
  # Numerator: Women reporting big problem - concern no female provider
  # v467g: 1 = big problem, 0/2 = not a big problem, 8/9 = DK/missing
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        v467g == 1 ~ 1,  # Big problem - concern no female provider
        v467g %in% c(0, 2) ~ 0,  # Not a big problem
        TRUE ~ NA_real_  # Don't know or missing
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
