#' Breastfed children 6-23 months fed both 4+ food groups and the minimum meal frequency
#'
#' Percentage of breastfed children age 6-23 months fed four or more food groups and the minimum meal frequency
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CN_IYCF_C_BTB <- function(KRdata) {
  # Restrict to youngest living child 6-23 months living with mother
  df <- KRdata %>%
    filter(b19 < 24, b9 == 0) %>%
    arrange(caseid) %>%
    group_by(caseid) %>%
    slice(1) %>%
    ungroup() %>%
    filter(b19 >= 6, b19 <= 23)
  
  # Helper: check if variable equals 1 (treating NA and other values as 0)
  is_yes <- function(x) {
    ifelse(!is.na(x) & x == 1, 1, 0)
  }
  
  df <- df %>%
    mutate(
      # Breastfeeding status
      breastfeeding = ifelse(!is.na(m4) & m4 == 95, 1, 0),
      
      # Food groups (count only if breastfeeding)
      # a) Breastmilk
      fg1 = ifelse(breastfeeding == 1, 1, 0),
      
      # b) Grains, roots, tubers
      fg2 = is_yes(v412a == 1 | v414e == 1 | v414f == 1),
      
      # c) Legumes and nuts
      fg3 = is_yes(v414o == 1 | v414c == 1),
      
      # d) Dairy products
      fg4 = is_yes(v411 == 1 | v411a == 1 | v414v == 1 | v414p == 1),
      
      # e) Flesh foods
      fg5 = is_yes(v414h == 1 | v414m == 1 | v414n == 1 | v414b == 1),
      
      # f) Eggs
      fg6 = is_yes(v414g == 1),
      
      # g) Vitamin A rich fruits and vegetables
      fg7 = is_yes(v414i == 1 | v414j == 1 | v414k == 1),
      
      # h) Other fruits and vegetables
      fg8 = is_yes(v414a == 1 | v414l == 1),
      
      # Total food groups
      food_groups = fg1 + fg2 + fg3 + fg4 + fg5 + fg6 + fg7 + fg8,
      
      # Minimum dietary diversity: 5+ of 8 food groups
      mdd = ifelse(breastfeeding == 1 & food_groups >= 5, 1, 0),
      
      # Minimum meal frequency for breastfed
      # 6-8 months: m39 in 2:7 (at least 2 times)
      # 9-23 months: m39 in 3:7 (at least 3 times)
      mmf = case_when(
        breastfeeding == 1 & b19 >= 6 & b19 <= 8 & 
          !is.na(m39) & m39 >= 2 & m39 <= 7 ~ 1,
        breastfeeding == 1 & b19 >= 9 & b19 <= 23 & 
          !is.na(m39) & m39 >= 3 & m39 <= 7 ~ 1,
        breastfeeding == 1 ~ 0,
        TRUE ~ NA_real_
      ),
      
      # Both criteria must be met
      value = ifelse(breastfeeding == 1, 
                    ifelse(mdd == 1 & mmf == 1, 1, 0),
                    NA_real_)
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(df)
}
