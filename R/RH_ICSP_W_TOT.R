#' Iron supplementation: total
#'
#' Percentage of women: total
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ICSP_W_TOT <- function(IRdata) {
  # Denominator: Women with a birth in the last 5 years
  # Numerator: All women in denominator (total = 100%)
  # Note: This is a constant total/base indicator, output as percentage
  
  result <- IRdata %>%
    filter(
      v208 > 0                              # At least one birth in last 5 years
    ) %>%
    mutate(value = 100) %>%                 # Total = 100% (constant)
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
