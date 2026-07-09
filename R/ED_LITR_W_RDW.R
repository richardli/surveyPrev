#' Women who can read a whole sentence
#'
#' Percentage of women who can read a whole sentence
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ED_LITR_W_RDW <- function(IRdata) {
  # Women who can read a whole sentence
  # Denominator: all women 15-49
  # Numerator: women without higher education (v106 < 3) who can read whole sentence (v155 = 2)
  # Note: Women with higher education (v106 >= 3) are coded as 0 because they are
  # in a separate category (ED_LITR_W_SCH), not part of the literacy test breakdown.
  
  result <- IRdata %>%
    mutate(
      value = case_when(
        # Missing education level
        v106 %in% c(8, 9) | is.na(v106) ~ NA_real_,
        # Higher education -> separate category (not "can read whole sentence")
        v106 >= 3 ~ 0,
        # No higher education (v106 < 3) -> check literacy test result
        v155 == 2 ~ 1,  # Can read whole sentence
        v155 %in% c(0, 1, 3, 4) ~ 0,  # Other literacy levels
        v155 %in% c(8, 9) | is.na(v155) ~ NA_real_,  # Missing literacy data
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
