#' Fully vaccinated (according to national schedule)
#'
#' Percentage of children 12-23 (or 24-35) months who were fully vaccinated according to the national schedule
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACC_C_APP <- function(KRdata) {
  # Fully vaccinated according to national schedule
  # Denominator: living children age 12-23 months (b5=1, b19 in 12:23)
  # Numerator: Polio 0 + BCG + Pentavalent 1-3 + Polio 1-3 + PCV 1-3 + Rotavirus 1-2 + Measles 1
  
  result <- KRdata %>%
    filter(b5 == 1 & b19 >= 12 & b19 <= 23) %>%
    mutate(
      value = case_when(
        # If any required vaccine is DK/missing, overall status is NA
        h0 %in% c(8, 9) | h2 %in% c(8, 9) | 
        h51 %in% c(8, 9) | h52 %in% c(8, 9) | h53 %in% c(8, 9) |
        h4 %in% c(8, 9) | h6 %in% c(8, 9) | h8 %in% c(8, 9) |
        h54 %in% c(8, 9) | h55 %in% c(8, 9) | h56 %in% c(8, 9) |
        h57 %in% c(8, 9) | h58 %in% c(8, 9) | h9 %in% c(8, 9)
        ~ NA_real_,
        # Fully vaccinated if all required vaccines received
        h0 %in% c(1, 2, 3) &      # Polio 0 (at birth)
        h2 %in% c(1, 2, 3) &      # BCG
        h51 %in% c(1, 2, 3) &     # Pentavalent 1
        h52 %in% c(1, 2, 3) &     # Pentavalent 2
        h53 %in% c(1, 2, 3) &     # Pentavalent 3
        h4 %in% c(1, 2, 3) &      # Polio 1
        h6 %in% c(1, 2, 3) &      # Polio 2
        h8 %in% c(1, 2, 3) &      # Polio 3
        h54 %in% c(1, 2, 3) &     # Pneumococcal 1
        h55 %in% c(1, 2, 3) &     # Pneumococcal 2
        h56 %in% c(1, 2, 3) &     # Pneumococcal 3
        h57 %in% c(1, 2, 3) &     # Rotavirus 1
        h58 %in% c(1, 2, 3) &     # Rotavirus 2
        h9 %in% c(1, 2, 3)        # Measles 1
        ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
