#' Physical or sexual or emotional violence committed by husband/partner in last 12 months
#'
#' Percentage of ever married women or never married women who have experienced physical or sexual or emotional violence committed by their husband/partner in the 12 months preceding the survey
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
DV_SPV1_W_ANY <- function(IRdata) {
  # Physical or sexual or emotional violence by husband/partner in last 12 months
  # Among ever-married or never-married women with intimate partner, selected for DV module
  
  # Check if d100 exists in the dataset
  has_d100 <- "d100" %in% names(IRdata)
  
  # Apply denominator filter
  if (has_d100) {
    result <- IRdata %>%
      filter(v044 == 1 & (v502 %in% 1:2 | d100 %in% 1:2))
  } else {
    result <- IRdata %>%
      filter(v044 == 1 & v502 %in% 1:2)
  }
  
  result <- result %>%
    mutate(
      # Physical violence in last 12 months (any of d105a-f, j == 1 or 2)
      phys_violence = case_when(
        d105a %in% 1:2 | d105b %in% 1:2 | d105c %in% 1:2 | d105d %in% 1:2 | 
        d105e %in% 1:2 | d105f %in% 1:2 | d105j %in% 1:2 ~ 1,
        !is.na(d105a) | !is.na(d105b) | !is.na(d105c) | !is.na(d105d) |
        !is.na(d105e) | !is.na(d105f) | !is.na(d105j) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Sexual violence in last 12 months (any of d105h, i, k == 1 or 2)
      sex_violence = case_when(
        d105h %in% 1:2 | d105i %in% 1:2 | d105k %in% 1:2 ~ 1,
        !is.na(d105h) | !is.na(d105i) | !is.na(d105k) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Emotional violence in last 12 months (any of d103a-c == 1 or 2)
      emo_violence = case_when(
        d103a %in% 1:2 | d103b %in% 1:2 | d103c %in% 1:2 ~ 1,
        !is.na(d103a) | !is.na(d103b) | !is.na(d103c) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Any violence: physical OR sexual OR emotional
      value = case_when(
        phys_violence == 1 | sex_violence == 1 | emo_violence == 1 ~ 1,
        !is.na(phys_violence) | !is.na(sex_violence) | !is.na(emo_violence) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(result)
}
