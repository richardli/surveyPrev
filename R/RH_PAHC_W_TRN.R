#' Problems in accessing health care: Having to take transport
#'
#' Percentage of women who reported they have big problems in having to take transport for treatment for themselves when they are sick
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PAHC_W_TRN <- function(IRdata) {
  # Denominator: All women age 15-49
  # Numerator: Women reporting transport as a big problem in accessing healthcare
  # Variable: v467e (having to take transport)
  # 1 = Big problem, 2 = Not a big problem, 8/9 = DK/missing
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        v467e == 1 ~ 1,  # Big problem
        v467e == 2 ~ 0,  # Not a big problem
        v467e %in% c(8, 9) ~ NA_real_,  # Don't know/missing
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
