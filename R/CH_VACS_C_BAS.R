#' Fully vaccinated (8 basic antigens)
#'
#' Percentage of children 12-23 (or 24-35) months who were fully vaccinated with 8 basic antigens (BCG, Polio 1-3, DPT 1-3, Measles)
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_BAS <- function(KRdata) {
  # Percentage of children 12-23 months who were fully vaccinated with 8 basic antigens
  # (BCG, Polio 1-3, DPT 1-3, Measles)
  
  # Filter to living children age 12-23 months
  df <- KRdata %>%
    filter(
      b5 == 1,           # Child is alive
      b19 >= 12,         # Age >= 12 months
      b19 <= 23          # Age <= 23 months
    ) %>%
    mutate(
      # BCG: h2 (card) or h2m/h2d/h2y (date/recall)
      bcg = case_when(
        h2 %in% c(1, 2, 3) ~ 1,  # 1=card, 2=recall, 3=vacc mark
        h2 %in% c(0, 8) ~ 0,      # 0=no, 8=don't know
        TRUE ~ NA_real_
      ),
      
      # DPT 1: h3
      dpt1 = case_when(
        h3 %in% c(1, 2, 3) ~ 1,
        h3 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # DPT 2: h5
      dpt2 = case_when(
        h5 %in% c(1, 2, 3) ~ 1,
        h5 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # DPT 3: h7
      dpt3 = case_when(
        h7 %in% c(1, 2, 3) ~ 1,
        h7 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Polio 1: h4
      polio1 = case_when(
        h4 %in% c(1, 2, 3) ~ 1,
        h4 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Polio 2: h6
      polio2 = case_when(
        h6 %in% c(1, 2, 3) ~ 1,
        h6 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Polio 3: h8
      polio3 = case_when(
        h8 %in% c(1, 2, 3) ~ 1,
        h8 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Measles: h9
      measles = case_when(
        h9 %in% c(1, 2, 3) ~ 1,
        h9 %in% c(0, 8) ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Fully vaccinated = all 8 antigens received
      value = case_when(
        bcg == 1 & dpt1 == 1 & dpt2 == 1 & dpt3 == 1 & 
        polio1 == 1 & polio2 == 1 & polio3 == 1 & measles == 1 ~ 1,
        is.na(bcg) | is.na(dpt1) | is.na(dpt2) | is.na(dpt3) |
        is.na(polio1) | is.na(polio2) | is.na(polio3) | is.na(measles) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
  
  return(df)
}
