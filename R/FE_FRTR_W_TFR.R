#' Total fertility rate 15-49
#'
#' Total fertility rate for the three years preceding the survey for age group 15-49 expressed per woman
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FE_FRTR_W_TFR <- function(IRdata) {
  # Total fertility rate for the three years preceding the survey
  # TFR is the sum of age-specific fertility rates (ASFRs) for 5-year age groups
  # multiplied by 5 (the width of the age groups)
  
  # Reference period: 3 years (36 months) before survey
  IRdata <- IRdata %>%
    mutate(
      # Century month code of interview
      doi = v008,
      # Calculate births in the last 3 years for each woman
      births_3yr = 0
    )
  
  # Count births in last 36 months for each woman
  # Birth history variables: b3_01, b3_02, ..., b3_20 (up to 20 births)
  birth_cols <- grep("^b3_\\d+$", names(IRdata), value = TRUE)
  
  for (col in birth_cols) {
    if (col %in% names(IRdata)) {
      IRdata <- IRdata %>%
        mutate(
          births_3yr = births_3yr + 
            ifelse(!is.na(.data[[col]]) & (doi - .data[[col]]) < 36 & (doi - .data[[col]]) >= 0, 1, 0)
        )
    }
  }
  
  # Calculate age-specific fertility rates for 5-year age groups
  # Age groups: 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49
  
  asfr_results <- IRdata %>%
    mutate(
      # Age at start of reference period (3 years ago)
      age_at_start = v012 - 3,
      # Assign to 5-year age group based on age at start of period
      age_group = case_when(
        age_at_start >= 15 & age_at_start < 20 ~ "15-19",
        age_at_start >= 20 & age_at_start < 25 ~ "20-24",
        age_at_start >= 25 & age_at_start < 30 ~ "25-29",
        age_at_start >= 30 & age_at_start < 35 ~ "30-34",
        age_at_start >= 35 & age_at_start < 40 ~ "35-39",
        age_at_start >= 40 & age_at_start < 45 ~ "40-44",
        age_at_start >= 45 & age_at_start < 50 ~ "45-49",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_group)) %>%
    group_by(age_group) %>%
    summarise(
      total_births = sum(births_3yr, na.rm = TRUE),
      total_women = n(),
      .groups = "drop"
    ) %>%
    mutate(
      # ASFR = (births / woman-years) * 1000
      # Woman-years = total_women * 3 years
      asfr = (total_births / (total_women * 3)) * 1000
    )
  
  # TFR = sum of ASFRs * 5 / 1000 (to convert back to per woman)
  tfr <- sum(asfr_results$asfr, na.rm = TRUE) * 5 / 1000
  
  # Return result with survey design columns
  result <- IRdata %>%
    slice(1) %>%
    select(any_of(c("v001", "v002", "v005", "v022", "v023", "v024", "v025"))) %>%
    mutate(value = tfr)
  
  return(result)
}
