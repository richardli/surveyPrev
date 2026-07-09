#' Decision maker about Own health care: Mainly wife [Men]
#'
#' Percentage of men for whom the decision maker for their own health care is mainly the respondent
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKH_M_WIF <- function(MRdata) {
  # MR recode -> mv* prefix. Weight is mv005/1e6.
  # Denominator: Currently married men (mv502 == 1)
  # Numerator: Wife/partner alone decides (mv743a == 4)
  # Note: Typical DHS coding: 1=respondent, 2=jointly, 3=respondent+other, 4=partner alone
  
  result <- MRdata %>%
    filter(mv502 == 1) %>%
    mutate(
      value = case_when(
        is.na(mv743a) | mv743a %in% c(8, 9, 98, 99) ~ NA_real_,
        mv743a == 4 ~ 1,  # Partner/wife alone decides
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
