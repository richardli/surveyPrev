#' Knowledge of HIV prevention methods - Composite of 2 components (prompted) [Men]
#'
#' Percentage of men who, in response to a prompted question, say that people can protect themselves from contracting HIV by using condoms or having sex only with one faithful, uninfected partner.
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
HA_KHVP_M_CS1 <- function(MRdata) {
  # Denominator: All men 15-49
  # Numerator: mv754cp==1 & mv754dp==1 (knows both prevention methods)
  # Code 9 (refused) → NA; code 8 (don't know), 0 (No), system NA → 0
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv754cp == 9 | mv754dp == 9 ~ NA_real_,
        mv754cp == 1 & mv754dp == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
