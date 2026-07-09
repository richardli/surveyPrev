#' Iron supplementation: don't know/missing
#'
#' Percentage of women who had a live birth (or stillbirth) in the two (or three/five) years preceding the survey who did not know if they took iron containing supplements or did not know the number of days they took iron supplementation during the pregnancy of the most recent live birth (or stillbirth) or where the information was missing
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_ICSP_W_IDK <- function(IRdata) {
  # RH_ICSP_W_IDK
  # Percentage of women who had a live birth (or stillbirth) in the two 
  # (or three/five) years preceding the survey who did not know if they 
  # took iron containing supplements or did not know the number of days 
  # they took iron supplementation during the pregnancy of the most recent 
  # live birth (or stillbirth) or where the information was missing
  
  # Determine reference period from survey (typically 2, 3, or 5 years)
  # Default to 5 years (60 months) to be inclusive
  period_months <- 60
  
  # Filter to women with a birth in the reference period
  df <- IRdata %>%
    filter(
      !is.na(v008),
      !is.na(b3_01),
      (v008 - b3_01) < period_months
    )
  
  # m45_1: Whether took iron during pregnancy of most recent birth
  # m46_1: Number of days took iron during pregnancy of most recent birth
  # 
  # Don't know/missing includes:
  # - m45_1 is missing, don't know (8), or other non-response
  # - m45_1 is yes (1) but m46_1 is missing/don't know (998)
  
  df <- df %>%
    mutate(
      # Check if m45_1 (whether took iron) is don't know or missing
      m45_dk_miss = is.na(m45_1) | m45_1 == 8 | m45_1 == 9,
      
      # Check if m46_1 (days took iron) is don't know or missing 
      # (when m45_1 indicates they took iron)
      m46_dk_miss = (m45_1 == 1) & (is.na(m46_1) | m46_1 == 998 | m46_1 == 999),
      
      # Value is 1 if either condition is true
      value = as.numeric(m45_dk_miss | m46_dk_miss)
    )
  
  # Select relevant columns
  result <- df %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025,
      value
    )
  
  return(result)
}
