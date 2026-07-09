#' Decision maker about Own health care: Mainly husband [Women]
#'
#' Percentage of women for whom the decision maker for their own health care is mainly the husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKH_W_HUS <- function(IRdata) {
  # Denominator: currently married women age 15-49 (v502 == 1)
  # Numerator: decisions about own health care made mainly by husband (v743a == 4)
  # 
  # Note: ZM 2018 appears to use non-standard coding where:
  # v743a: 1=respondent alone, 2=jointly, 4=husband alone, 6=someone else/other
  # (Standard code 3 is not used in this dataset)
  
  result <- IRdata %>%
    filter(v502 == 1) %>%  # Currently married women
    mutate(
      value = case_when(
        v743a == 4 ~ 1,  # Husband alone makes decision
        v743a %in% c(1, 2, 6) ~ 0,  # Other valid response categories
        TRUE ~ NA_real_  # Missing or invalid
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
