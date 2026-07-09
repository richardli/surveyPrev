#' Men who ever used the internet
#'
#' Percentage of men who ever used the internet
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_INUS_M_EVU <- function(MRdata) {
  # MR recode -> mv* prefix throughout. Weight is mv005/1e6.
  # Denominator: Men age 15-49 (explicit filter per DHS guide)
  # Numerator: Ever used internet (mv171a in 1:3 per DHS guide)
  # mv171a coding: 1/2/3 = ever used, 0 = never, 8/9 = DK/missing
  
  result <- MRdata %>%
    filter(mv012 %in% 15:49) %>%
    mutate(
      value = case_when(
        is.na(mv171a) ~ NA_real_,
        mv171a %in% c(8, 9, 98, 99) ~ NA_real_,
        mv171a %in% 1:3 ~ 1,
        mv171a == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
