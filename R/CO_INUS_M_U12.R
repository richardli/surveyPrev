#' Men who used the internet in the past 12 months
#'
#' Percentage of men who used the internet in the last 12 months
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_INUS_M_U12 <- function(MRdata) {
  # Men who used the internet in the past 12 months
  # Recode: MR (Men's Recode)
  # Denominator: Men age 15-49
  # Numerator: mv171a = 1 (used internet in past 12 months)
  
  result <- MRdata %>%
    filter(mv012 %in% 15:49) %>%
    mutate(
      value = case_when(
        mv171a == 1 ~ 1,
        mv171a %in% c(0, 2, 3) ~ 0,
        mv171a %in% c(8, 9) ~ NA_real_,
        is.na(mv171a) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
