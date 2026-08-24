#' Total fertility rate 15-44
#'
#' Total fertility rate for the three years preceding the survey for age group 15-44 expressed per woman
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FE_FRTR_W_TF4 <- function(IRdata) {
  # Total fertility rate for ages 15-44 for the 3 years preceding the survey
  # TFR is the sum of age-specific fertility rates (ASFRs) for 5-year age groups
  # multiplied by 5 (the width of the age interval)
  
  # Reference period: 3 years = 36 months before interview
  IRdata <- IRdata %>%
    mutate(
      # Century month code of interview
      doi = v008,
      # Calculate date 36 months before interview
      doi_minus_36 = doi - 36
    )
  
  # Process birth history from b3_* variables
  # We need to identify all births in the 3 years before the survey
  birth_cols <- grep("^b3_", names(IRdata), value = TRUE)
  
  if (length(birth_cols) == 0) {
    return(data.frame(
      v001 = numeric(0), v002 = numeric(0), v005 = numeric(0),
      v022 = numeric(0), v023 = numeric(0), v024 = numeric(0),
      v025 = numeric(0), value = numeric(0)
    ))
  }
  
  # Reshape to long format for births
  id_cols <- c(
    "caseid", "v001", "v002", "v005", "v022",
    "v023", "v024", "v025", "v008", "v011"
  )
  birth_data <- data.table::melt(
    data.table::as.data.table(
      IRdata[, c(id_cols, birth_cols), drop = FALSE]
    ),
    id.vars = id_cols,
    measure.vars = birth_cols,
    variable.name = "birth_order",
    value.name = "birth_cmc",
    variable.factor = FALSE,
    na.rm = TRUE
  ) %>%
    as.data.frame() %>%
    filter(birth_cmc > 0)  # Valid births only
  
  # Filter births in the 3-year reference period
  birth_data <- birth_data %>%
    filter(birth_cmc >= v008 - 36 & birth_cmc < v008)
  
  # Calculate woman's age at birth
  birth_data <- birth_data %>%
    mutate(
      age_at_birth = floor((birth_cmc - v011) / 12),
      # Assign to 5-year age groups 15-19, 20-24, ..., 40-44
      age_group = case_when(
        age_at_birth >= 15 & age_at_birth < 20 ~ "15-19",
        age_at_birth >= 20 & age_at_birth < 25 ~ "20-24",
        age_at_birth >= 25 & age_at_birth < 30 ~ "25-29",
        age_at_birth >= 30 & age_at_birth < 35 ~ "30-34",
        age_at_birth >= 35 & age_at_birth < 40 ~ "35-39",
        age_at_birth >= 40 & age_at_birth < 45 ~ "40-44",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_group))
  
  # Calculate exposure time (woman-years) for each woman in each age group
  # during the 3-year reference period
  exposure_data <- IRdata %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, v008, v011) %>%
    mutate(
      period_start = v008 - 36,
      period_end = v008 - 1
    )
  
  # For each age group, calculate exposure
  age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44")
  exposure_list <- list()
  
  for (ag in age_groups) {
    ag_start <- as.numeric(substr(ag, 1, 2))
    ag_end <- ag_start + 4
    
    exp_temp <- exposure_data %>%
      mutate(
        # CMC when woman enters age group
        age_group_start_cmc = v011 + ag_start * 12,
        # CMC when woman exits age group
        age_group_end_cmc = v011 + (ag_end + 1) * 12 - 1,
        # Exposure start is max of period start and age group start
        exp_start = pmax(period_start, age_group_start_cmc),
        # Exposure end is min of period end and age group end
        exp_end = pmin(period_end, age_group_end_cmc),
        # Exposure in months
        exposure_months = pmax(0, exp_end - exp_start + 1),
        # Exposure in years
        exposure_years = exposure_months / 12,
        age_group = ag
      ) %>%
      filter(exposure_years > 0)
    
    exposure_list[[ag]] <- exp_temp
  }
  
  exposure_all <- bind_rows(exposure_list)
  
  # Calculate ASFR for each age group
  asfr_list <- list()
  
  for (ag in age_groups) {
    births_ag <- birth_data %>%
      filter(age_group == ag) %>%
      summarise(total_births = sum(v005 / 1000000), .groups = "drop")
    
    exposure_ag <- exposure_all %>%
      filter(age_group == ag) %>%
      summarise(total_exposure = sum(exposure_years * v005 / 1000000), .groups = "drop")
    
    asfr <- if (nrow(exposure_ag) > 0 && exposure_ag$total_exposure > 0) {
      births_ag$total_births / exposure_ag$total_exposure
    } else {
      0
    }
    
    asfr_list[[ag]] <- data.frame(age_group = ag, asfr = asfr)
  }
  
  asfr_df <- bind_rows(asfr_list)
  
  # TFR is sum of ASFRs * 5
  tfr <- sum(asfr_df$asfr, na.rm = TRUE) * 5
  
  # Return single row with survey design variables
  result <- IRdata %>%
    slice(1) %>%
    select(v001, v002, v005, v022, v023, v024, v025) %>%
    mutate(value = tfr)
  
  return(result)
}
