#' Breastfed children 6-23 months fed the minimum meal frequency
#'
#' Percentage of breastfed children age 6-23 months fed the minimum meal frequency defined as receiving solid or semi-solid food at least twice a day for infants 6-8 months and at least three times a day for children 9-23 months
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CN_IYCF_C_MNB <- function(KRdata) {
  # Denominator: youngest breastfed children age 6-23 months living with mother
  # Numerator: minimum meal frequency per age group
  result <- KRdata %>%
    # Step 1: under 24 months and living with mother
    filter(b19 < 24, b9 == 0) %>%
    # Step 2: keep only youngest child per respondent
    arrange(caseid) %>%
    group_by(caseid) %>%
    slice(1) %>%
    ungroup() %>%
    # Step 3: restrict to age 6-23 months
    filter(b19 >= 6) %>%
    # Step 4: restrict to breastfed children
    filter(m4 == 95) %>%
    # Numerator: minimum meal frequency
    mutate(
      value = case_when(
        # 6-8 months: m39 in 2:7 (at least 2 times)
        b19 %in% 6:8 & !is.na(m39) & m39 %in% 2:7 ~ 1,
        # 9-23 months: m39 in 3:7 (at least 3 times)
        b19 %in% 9:23 & !is.na(m39) & m39 %in% 3:7 ~ 1,
        # Missing or don't know for m39 (excluded from numerator, included in denominator)
        is.na(m39) | m39 > 7 ~ NA_real_,
        # Valid m39 but doesn't meet threshold
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
