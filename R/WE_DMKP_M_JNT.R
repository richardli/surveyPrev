#' Decision maker about Major household purchases: Wife and husband jointly [Men]
#'
#' Percentage of men for whom the decision maker for major household purchases is mainly the respondent and her husband
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
WE_DMKP_M_JNT <- function(MRdata) {
  # Denominator: Currently married men (mv502 == 1)
  # Numerator: Joint decision-making for major household purchases
  # mv743b == 2: Respondent and wife jointly
  
  result <- MRdata %>%
    filter(mv502 == 1) %>%
    mutate(
      value = case_when(
        is.na(mv743b) ~ NA_real_,
        mv743b %in% c(8, 9) ~ NA_real_,
        mv743b == 2 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
