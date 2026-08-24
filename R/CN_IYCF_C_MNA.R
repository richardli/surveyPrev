#' Children 6-23 months fed the minimum meal frequency
#'
#' Percentage of children age 6-23 months fed the minimum meal frequency defined as receiving solid or semi-solid food at least twice a day for infants 6-8 months and at least three times a day for children 9-23 months
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CN_IYCF_C_MNA <- function(KRdata) {
  # Percentage of children age 6-23 months fed the minimum meal frequency
  # This is the combined indicator for both breastfed and non-breastfed children
  
  # Step 1: Filter to living children living with mother, born in last 24 months
  df <- KRdata %>%
    filter(b5 == 1, b9 == 0, b19 < 24)
  
  # Step 2: Keep only youngest child per mother
  df <- df %>%
    group_by(caseid) %>%
    arrange(caseid, bidx) %>%
    slice(1) %>%
    ungroup()
  
  # Step 3: Restrict to age 6-23 months
  df <- df %>%
    filter(b19 >= 6, b19 <= 23)
  
  # Step 4: Determine breastfeeding status
  # Missing or DK for m4 treated as not breastfeeding
  df <- df %>%
    mutate(
      breastfed = ifelse(m4 == 95, 1, 0),
      breastfed = ifelse(is.na(breastfed), 0, breastfed)
    )
  
  # Step 5: Calculate minimum meal frequency for breastfed children
  # Breastfed 6-8 months: m39 >= 2
  # Breastfed 9-23 months: m39 >= 3
  df <- df %>%
    mutate(
      mmf_bf = case_when(
        breastfed == 1 & b19 >= 6 & b19 <= 8 & m39 >= 2 & m39 <= 7 ~ 1,
        breastfed == 1 & b19 >= 9 & b19 <= 23 & m39 >= 3 & m39 <= 7 ~ 1,
        breastfed == 1 & (is.na(m39) | m39 > 7) ~ NA_real_,
        breastfed == 1 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Step 6: Calculate minimum meal frequency for non-breastfed children
  # Need: solid feeds + milk feeds >= 4, with at least one solid feed
  # Milk feeds = v469e + v469f + v469x (if v413a==1)
  df <- df %>%
    mutate(
      # Milk feeds count
      milk_v469e = ifelse(!is.na(v469e) & v469e >= 1 & v469e <= 7, v469e, 0),
      milk_v469f = ifelse(!is.na(v469f) & v469f >= 1 & v469f <= 7, v469f, 0),
      milk_v469x = ifelse(!is.na(v469x) & v469x >= 1 & v469x <= 7 & !is.na(v413a) & v413a == 1, v469x, 0),
      total_milk_feeds = milk_v469e + milk_v469f + milk_v469x,
      
      # Solid feeds count
      solid_feeds = ifelse(!is.na(m39) & m39 >= 1 & m39 <= 7, m39, 0),
      
      # Total feeds
      total_feeds = total_milk_feeds + solid_feeds,
      
      # Minimum meal frequency for non-breastfed
      mmf_nbf = case_when(
        breastfed == 0 & total_feeds >= 4 & solid_feeds >= 1 ~ 1,
        breastfed == 0 & (is.na(m39) | m39 > 7) ~ NA_real_,
        breastfed == 0 ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Step 7: Combine breastfed and non-breastfed MMF
  df <- df %>%
    mutate(
      value = case_when(
        breastfed == 1 ~ mmf_bf,
        breastfed == 0 ~ mmf_nbf,
        TRUE ~ NA_real_
      )
    )
  
  # Step 8: Select required columns
  result <- df %>%
    select(
      caseid, v001, v002, v005, v022, v023, v024, v025, value
    )
  
  return(result)
}
