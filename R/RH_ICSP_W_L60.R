#' Iron supplementation: less than 60 days
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who took iron containing supplements for less than 60 days during the pregnancy of the most recent live birth (or stillbirth)
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ICSP_W_L60 <- function(IRdata) {
  # Denominator: women with live birth in last 36 months (3 years)
  # Numerator: took iron supplements for 1-59 days during most recent pregnancy
  # m46_1: number of days of iron supplementation (m45_1 is typically yes/no)
  
  result <- IRdata %>%
    mutate(
      # Recent birth: last 36 months (3 years)
      recent_birth = !is.na(b3_01) & (v008 - b3_01) < 36,
      
      # Iron supplement duration in days
      iron_days = case_when(
        is.na(m46_1) ~ NA_real_,
        m46_1 >= 998 ~ NA_real_,
        TRUE ~ as.numeric(m46_1)
      ),
      
      # Value: 1 if 1-59 days, 0 otherwise (including 0 or 60+)
      value = case_when(
        !recent_birth ~ NA_real_,
        is.na(iron_days) ~ NA_real_,
        iron_days >= 1 & iron_days < 60 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
