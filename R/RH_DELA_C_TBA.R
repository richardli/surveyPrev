#' Assistance during delivery: Traditional birth attendant
#'
#' Percentage of live births (or stillbirths) in the two (or three/five) years preceding the survey assisted by a traditional birth attendant
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_DELA_C_TBA <- function(KRdata) {
  # RH_DELA_C_TBA: Percentage of live births in the 2 years (or 3/5 years) 
  # preceding the survey assisted by a traditional birth attendant
  
  # Determine reference period based on available data
  # First check if we have p19 (months since pregnancy outcome)
  if ("p19" %in% names(KRdata)) {
    # Use p19 for the reference period filter
    period_months <- if (max(KRdata$p19, na.rm = TRUE) > 36) 60 else 24
    KRdata <- KRdata %>%
      filter(p19 < period_months)
  } else {
    # Fallback: calculate from v008 (interview date) and b3 (birth date)
    if ("b3" %in% names(KRdata)) {
      KRdata <- KRdata %>%
        mutate(months_since_birth = v008 - b3) %>%
        filter(months_since_birth < 24)
    }
  }
  
  # Filter to live births only (exclude stillbirths)
  # m80 = 1 indicates live birth (from most recent pregnancy)
  # For KR file, we generally have live births by design
  if ("m80" %in% names(KRdata)) {
    KRdata <- KRdata %>%
      filter(m80 %in% c(1, 2))  # Live births (1=most recent, 2=other live birth)
  }
  
  # Create indicator for traditional birth attendant assistance
  # m3g = 1 indicates traditional birth attendant assisted
  KRdata <- KRdata %>%
    mutate(
      tba_assistance = case_when(
        m3g == 1 ~ 1,
        m3g == 0 ~ 0,
        is.na(m3g) ~ NA_real_,
        TRUE ~ 0
      ),
      value = tba_assistance
    )
  
  # Return result preserving survey design columns
  result <- KRdata %>%
    select(
      caseid,
      v001, v002, v005, 
      any_of(c("v021", "v022", "v023", "v024", "v025")),
      value
    )
  
  return(as.data.frame(result))
}
