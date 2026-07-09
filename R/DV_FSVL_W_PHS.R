#' Intimate partner violence: Any physical violence
#'
#' Percentage of ever married women or never married women who ever had an intimate partner whose current or most recent husband/partner committed any physical violence
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
DV_FSVL_W_PHS <- function(IRdata) {
  result <- IRdata %>%
    mutate(
      value = case_when(
        # Not selected for DV module -> NA
        v044 != 1 ~ NA_real_,
        # Not ever-married (currently or formerly married) -> NA
        !(v502 %in% c(1, 2)) ~ NA_real_,
        # Physical violence by current/most recent partner: any of d105a-f, j
        d105a > 0 | d105b > 0 | d105c > 0 | d105d > 0 | 
        d105e > 0 | d105f > 0 | d105j > 0 ~ 1,
        # In denominator but no violence
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
