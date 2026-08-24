#' Fully vaccinated (according to national schedule)
#'
#' Percentage of children 12-23 (or 24-35) months who were fully vaccinated according to the national schedule
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_APP <- function(KRdata) {
  # Fully vaccinated according to national schedule
  # Denominator: living children age 12-23 months
  # Numerator: BCG + OPV0-3 + Pent1-3 + PCV1-3 + Rota1-2 + Measles (14 vaccines)
  # Accepts both vaccination card and mother's report
  
  result <- KRdata %>%
    filter(
      b5 == 1,           # child is alive
      b19 >= 12,         # age >= 12 months
      b19 <= 23          # age <= 23 months
    ) %>%
    mutate(
      bcg = case_when(
        h2 %in% c(1, 2, 3) ~ 1,
        h2 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      opv0 = case_when(
        h0 %in% c(1, 2, 3) ~ 1,
        h0 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      opv1 = case_when(
        h4 %in% c(1, 2, 3) ~ 1,
        h4 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      opv2 = case_when(
        h6 %in% c(1, 2, 3) ~ 1,
        h6 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      opv3 = case_when(
        h8 %in% c(1, 2, 3) ~ 1,
        h8 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      pent1 = case_when(
        h51 %in% c(1, 2, 3) ~ 1,
        h51 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      pent2 = case_when(
        h52 %in% c(1, 2, 3) ~ 1,
        h52 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      pent3 = case_when(
        h53 %in% c(1, 2, 3) ~ 1,
        h53 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      pcv1 = case_when(
        h54 %in% c(1, 2, 3) ~ 1,
        h54 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      pcv2 = case_when(
        h55 %in% c(1, 2, 3) ~ 1,
        h55 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      pcv3 = case_when(
        h56 %in% c(1, 2, 3) ~ 1,
        h56 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      rota1 = case_when(
        h57 %in% c(1, 2, 3) ~ 1,
        h57 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      rota2 = case_when(
        h58 %in% c(1, 2, 3) ~ 1,
        h58 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      measles = case_when(
        h9 %in% c(1, 2, 3) ~ 1,
        h9 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Fully vaccinated: all vaccines received
      value = case_when(
        bcg == 1 & opv0 == 1 & opv1 == 1 & opv2 == 1 & opv3 == 1 &
        pent1 == 1 & pent2 == 1 & pent3 == 1 &
        pcv1 == 1 & pcv2 == 1 & pcv3 == 1 &
        rota1 == 1 & rota2 == 1 & measles == 1 ~ 1,
        is.na(bcg) | is.na(opv0) | is.na(opv1) | is.na(opv2) | is.na(opv3) |
        is.na(pent1) | is.na(pent2) | is.na(pent3) |
        is.na(pcv1) | is.na(pcv2) | is.na(pcv3) |
        is.na(rota1) | is.na(rota2) | is.na(measles) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
