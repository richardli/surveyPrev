#' Current marital status [Young women]: Married or living in union
#'
#' Percentage of young women age 15-19, 20-24, or 15-24 married or living in union
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
MA_MSTY_W_UNI <- function(IRdata) {
  # MA_MSTY_W_UNI: Percentage of young women age 15-19
  # married or living in union
  
  # Restrict to young women aged 15-19
  IRdata_young <- IRdata %>%
    filter(v012 >= 15 & v012 <= 19)
  
  # v501 contains current marital status:
  # 0 = never in union
  # 1 = married
  # 2 = living with partner
  # 3 = widowed
  # 4 = divorced
  # 5 = no longer living together/separated
  
  # Married or living in union = codes 1 or 2
  result <- IRdata_young %>%
    mutate(
      value = case_when(
        v501 %in% c(1, 2) ~ 1,
        v501 %in% c(0, 3, 4, 5) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
