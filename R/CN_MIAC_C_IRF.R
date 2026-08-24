#' Children 6-23 months that consumed foods rich in iron in the last 24 hours
#'
#' Percentage of youngest children age 6-23 months living with the mother who consumed foods rich in iron in the 24 hours preceding the survey. Food rich in iron include meat (and organ meat), fish, poultry, eggs.
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CN_MIAC_C_IRF <- function(KRdata) {
  # Children 6-23 months that consumed foods rich in iron in the last 24 hours
  # Iron-rich foods include: meat (v414h), organ meat (v414m), fish (v414n), 
  # poultry, and eggs (v414g)
  
  # Identify youngest child living with mother aged 6-23 months
  KRdata <- KRdata %>%
    mutate(
      # Child is alive
      child_alive = b5 == 1,
      # Age in months 6-23
      age_6_23 = b19 >= 6 & b19 <= 23,
      # Child lives with mother (b9 = 0 means living with respondent)
      lives_with_mother = b9 == 0
    ) %>%
    filter(child_alive & age_6_23 & lives_with_mother)
  
  # Identify youngest child per mother (caseid)
  KRdata <- KRdata %>%
    group_by(caseid) %>%
    arrange(b19) %>%
    slice(1) %>%
    ungroup()
  
  # Calculate indicator: consumed iron-rich foods
  # Iron-rich foods: eggs (v414g), meat (v414h), organ meat (v414m), fish (v414n)
  result <- KRdata %>%
    mutate(
      consumed_iron = case_when(
        v414g == 1 | v414h == 1 | v414m == 1 | v414n == 1 ~ 1,
        is.na(v414g) & is.na(v414h) & is.na(v414m) & is.na(v414n) ~ NA_real_,
        TRUE ~ 0
      ),
      value = consumed_iron
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(result)
}
