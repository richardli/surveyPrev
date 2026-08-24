#' Men who have heard of HIV or AIDS
#'
#' Percentage of men who have heard of HIV or AIDS
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
HA_KAID_M_HRD <- function(MRdata) {
  # MR recode -> mv* prefix throughout. Weight is mv005/1e6.
  # Denominator: All men age 15-49 (no filter beyond frame default)
  # Numerator: Men who have heard of HIV/AIDS
  # Variable: mv751 (1=yes, 0=no, 8/9/98/99=DK/missing)
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        is.na(mv751) ~ NA_real_,
        mv751 %in% c(8, 9, 98, 99) ~ NA_real_,
        mv751 == 1 ~ 1,
        mv751 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
