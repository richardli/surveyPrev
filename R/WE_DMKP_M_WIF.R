#' Decision maker about Major household purchases: Mainly wife [Men]
#'
#' Percentage of men for whom the decision maker for major household purchases is mainly the respondent
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKP_M_WIF <- function(MRdata) {
  # Denominator: Currently married men (mv502 == 1)
  # Numerator: Large household purchases decided mainly by wife (mv743b == 4)
  # rows after filter: 6472 currently married men
  
  result <- MRdata %>%
    filter(mv502 == 1) %>%
    mutate(
      value = case_when(
        mv743b == 4 ~ 1,  # Wife/partner decides
        mv743b %in% c(1, 2, 5, 6) ~ 0,  # Other valid responses
        mv743b %in% c(8, 9, 98, 99) | is.na(mv743b) ~ NA_real_,  # DK, missing
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
