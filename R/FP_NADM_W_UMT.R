#' Unmet need for modern methods
#'
#' Percentage of currently married or in union women with an unmet need for modern methods
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NADM_W_UMT <- function(IRdata) {
  # Denominator: currently married or in union women (v502 == 1)
  # Numerator: women with unmet need for modern methods
  #   = women with unmet need (v626a = 1 or 2) OR using non-modern methods (v313 = 1 or 2)
  # 
  # v626a: 1=unmet spacing, 2=unmet limiting, 3=met spacing, 4=met limiting, 9=not applicable
  # v313: 0=not using, 1=traditional, 2=folkloric, 3=modern
  
  result <- IRdata %>%
    filter(v502 == 1) %>%  # currently married/in union
    mutate(
      value = case_when(
        v626a %in% c(1, 2) ~ 1,              # has unmet need
        v313 %in% c(1, 2) ~ 1,               # using traditional or folkloric method
        v626a %in% c(3, 4, 9) ~ 0,           # met need or not applicable
        v313 %in% c(0, 3) ~ 0,               # not using or modern
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
