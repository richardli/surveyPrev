#' Men who have a bank account
#'
#' Percentage of men who have and use a bank account
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CO_MOBB_M_BNK <- function(MRdata) {
  # Men age 15-49 who have and use a bank account
  # Definition: mv170 = 1 (has account in bank or other financial institution)
  # Universe: All men age 15-49
  
  MRdata %>%
    filter(
      mv012 >= 15,
      mv012 <= 49
    ) %>%
    mutate(
      value = case_when(
        mv170 == 1 ~ 1,
        mv170 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      mv001,
      mv002,
      mv005,
      mv021,
      mv022,
      mv023,
      mv024,
      mv025,
      value
    )
}
