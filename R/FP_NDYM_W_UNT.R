#' Unmet need for family planning (married young women)
#'
#' Percentage of currently married or in union young women age 15-24 with an unmet need for family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYM_W_UNT <- function(IRdata) {
  # FP_NDYM_W_UNT: Unmet need for family planning (married young women)
  # Percentage of currently married or in union young women age 15-24 with an unmet need for family planning
  
  # Restrict to currently married or in union young women age 15-24
  df <- IRdata %>%
    filter(
      v012 >= 15 & v012 <= 24,  # Young women age 15-24
      v502 == 1                  # Currently married or in union
    ) %>%
    mutate(
      # Unmet need variable: v626a
      # 1 = unmet need for spacing
      # 2 = unmet need for limiting
      # 3 = using for spacing
      # 4 = using for limiting
      # 7 = no unmet need
      # 9 = missing/infecund/menopausal
      
      value = case_when(
        v626a %in% c(1, 2) ~ 1,    # Has unmet need (spacing or limiting)
        v626a %in% c(3, 4, 7) ~ 0, # No unmet need (using or no need)
        TRUE ~ NA_real_            # Missing or other
      )
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(df)
}
