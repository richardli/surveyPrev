#' Final say in own health care [Women]
#'
#' Percentage of women who say that they alone or jointly have the final say in own health care
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMAK_W_OHC <- function(IRdata) {
  # Denominator: currently married women age 15-49 (v502 == 1)
  # Numerator: decides alone or jointly on own health care (v743a in 1:2)
  #   v743a: 1=respondent alone, 2=jointly, 4=husband alone, 6=someone else
  
  result <- IRdata %>%
    filter(v502 == 1) %>%
    mutate(
      value = case_when(
        v743a %in% c(1, 2) ~ 1,
        !is.na(v743a) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
