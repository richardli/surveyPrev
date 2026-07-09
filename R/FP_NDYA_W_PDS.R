#' Demand for family planning satisfied (all young women)
#'
#' Percentage of demand for family planning satisfied is calculated as the number of young women age 15-24 using family planning divided by the number of all young women with demand for family planning (either with unmet need or currently using any family planning)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYA_W_PDS <- function(IRdata) {
  # Demand for family planning satisfied (all young women age 15-24)
  # Numerator: Young women using any family planning method
  # Denominator: Young women with demand (using + unmet need)
  
  # Keep only young women age 15-24
  df <- IRdata %>%
    filter(v012 >= 15 & v012 <= 24)
  
  # Current use of any family planning method (modern or traditional)
  # v313 = 0: Not using, 1: Folkloric, 2: Traditional, 3: Modern
  df <- df %>%
    mutate(
      using_any = case_when(
        v313 %in% c(1, 2, 3) ~ 1,
        v313 == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Unmet need for family planning
  # v626a = 1: Unmet need for spacing, 2: Unmet need for limiting
  # 3: Using for spacing, 4: Using for limiting, 7: No unmet need, 9: Infecund/menopausal
  df <- df %>%
    mutate(
      unmet_need = case_when(
        v626a %in% c(1, 2) ~ 1,
        v626a %in% c(3, 4, 7, 9) ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Total demand = using any method OR has unmet need
  df <- df %>%
    mutate(
      demand = case_when(
        using_any == 1 | unmet_need == 1 ~ 1,
        using_any == 0 & unmet_need == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Demand satisfied = using any method among those with demand
  # Indicator value = (using_any / demand) * 100
  df <- df %>%
    mutate(
      value = case_when(
        demand == 1 & using_any == 1 ~ 1,
        demand == 1 & using_any == 0 ~ 0,
        demand == 0 ~ NA_real_,
        TRUE ~ NA_real_
      )
    )
  
  # Select relevant columns
  result <- df %>%
    select(
      caseid,
      v001, v002, v005, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
