#' Age specific fertility rate: 15-19
#'
#' Age-specific fertility rate for the three years preceding the survey for age group 15-19 expressed per 1,000 women
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FE_FRTR_W_A15 <- function(IRdata) {
  # Age-specific fertility rate for women aged 15-19
  # for the three years preceding the survey, per 1,000 women
  
  # Calculate exposure time in months for each woman in the 15-19 age group
  # during the three years (36 months) before the survey
  
  # Step 1: Identify women who were aged 15-19 at any point during the 3-year period
  IRdata$age_at_interview <- IRdata$v012
  IRdata$cmc_interview <- IRdata$v008
  
  # Women need to have been in the 15-19 age range during the reference period
  # Reference period: 36 months before interview (v008-36 to v008-1)
  # Age at start of period: v012 - 3 years
  # Age at end of period: v012
  
  # A woman contributes if she was 15-19 at any point in the last 3 years
  IRdata$age_start <- IRdata$age_at_interview - 3
  IRdata$age_end <- IRdata$age_at_interview
  
  # Include if age range overlaps with 15-19
  IRdata <- IRdata %>%
    filter(age_end >= 15 & age_start < 20)
  
  # Step 2: Calculate person-months of exposure for each woman
  IRdata$exposure_months <- 36
  
  # Adjust for women who entered age 15 during the period
  IRdata$cmc_age15 <- IRdata$v011 + 15*12
  entered_during <- IRdata$cmc_age15 > (IRdata$cmc_interview - 36) & 
                    IRdata$cmc_age15 <= IRdata$cmc_interview
  IRdata$exposure_months[entered_during] <- 
    IRdata$cmc_interview[entered_during] - IRdata$cmc_age15[entered_during]
  
  # Adjust for women who turned 20 during the period
  IRdata$cmc_age20 <- IRdata$v011 + 20*12
  exited_during <- IRdata$cmc_age20 > (IRdata$cmc_interview - 36) & 
                   IRdata$cmc_age20 <= IRdata$cmc_interview
  IRdata$exposure_months[exited_during] <- 
    IRdata$cmc_age20[exited_during] - (IRdata$cmc_interview[exited_during] - 36)
  
  # Both entered and exited during period
  both <- entered_during & exited_during
  IRdata$exposure_months[both] <- 
    IRdata$cmc_age20[both] - IRdata$cmc_age15[both]
  
  # Step 3: Count births in the reference period for these women
  birth_cols <- paste0("b3_", sprintf("%02d", 1:20))
  birth_cols <- intersect(birth_cols, names(IRdata))
  
  IRdata$births <- 0
  for (col in birth_cols) {
    if (col %in% names(IRdata)) {
      birth_cmc <- IRdata[[col]]
      # Birth in last 36 months and mother was 15-19 at birth
      in_period <- !is.na(birth_cmc) & 
                   birth_cmc >= (IRdata$cmc_interview - 36) & 
                   birth_cmc < IRdata$cmc_interview
      
      age_at_birth <- floor((birth_cmc - IRdata$v011) / 12)
      age_eligible <- age_at_birth >= 15 & age_at_birth < 20
      
      IRdata$births <- IRdata$births + (in_period & age_eligible)
    }
  }
  
  # Step 4: Calculate ASFR
  total_births <- sum(IRdata$births * IRdata$v005 / 1000000, na.rm = TRUE)
  total_exposure_years <- sum(IRdata$exposure_months * IRdata$v005 / 1000000 / 12, na.rm = TRUE)
  
  asfr_per_woman <- total_births / total_exposure_years
  asfr_per_1000 <- asfr_per_woman * 1000
  
  # Return result
  result <- data.frame(
    value = asfr_per_1000,
    v001 = IRdata$v001[1],
    v002 = IRdata$v002[1],
    v005 = IRdata$v005[1],
    v022 = IRdata$v022[1],
    v023 = IRdata$v023[1],
    v024 = IRdata$v024[1],
    v025 = IRdata$v025[1]
  )
  
  return(result)
}
