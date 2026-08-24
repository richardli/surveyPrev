#' No postnatal checkup for newborn within first two days of birth
#'
#' Percentage of last births in the two years preceding the survey who received no postnatal checkup in the first two days after birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCCP_C_NON <- function(IRdata) {
  # Check for b19_01
  if (!("b19_01" %in% names(IRdata))) {
    IRdata$b19_01 <- NA_real_
  }
  b19_included <- ifelse(all(is.na(IRdata$b19_01)), 0, 1)
  
  # Calculate age in months
  if (b19_included == 1) {
    IRdata <- IRdata %>% mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>% mutate(age = v008 - b3_01)
  }
  
  # Check for PNC indicators
  if (!("m70_1" %in% names(IRdata))) IRdata$m70_1 <- NA_real_
  if (!("m74_1" %in% names(IRdata))) IRdata$m74_1 <- NA_real_
  if (!("bidx_01" %in% names(IRdata))) IRdata$bidx_01 <- 1
  
  m70_included <- ifelse(all(is.na(IRdata$m70_1)), 0, 1)
  
  if (m70_included == 0) {
    IRdata <- IRdata %>%
      mutate(value = NA_real_) %>%
      select(v001, v002, v005, v022, v023, v024, v025, value)
    return(as.data.frame(IRdata))
  }
  
  IRdata <- IRdata %>%
    mutate(
      nbcheck = case_when(m70_1 == 1 | m74_1 == 1 ~ 1, TRUE ~ 0),
      pnc_nb_timing_all = case_when(
        nbcheck != 1 & age < 24 ~ 0,
        !is.na(m76_1) & m76_1 >= 11 & m76_1 <= 29 & age < 24 ~ as.numeric(m75_1),
        age < 24 & nbcheck == 1 ~ 999,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(
      pnc_nb_timing_all = case_when(
        !is.na(pnc_nb_timing_all) & pnc_nb_timing_all < 1000 & 
          !is.na(m76_1) & m76_1 > 30 & m76_1 < 100 & age < 24 ~ 0,
        pnc_nb_timing_all == 999 & !is.na(m72_1) & 
          m72_1 >= 11 & m72_1 <= 29 & age < 24 ~ as.numeric(m71_1),
        !is.na(m71_1) & m71_1 < 1000 & !is.na(m72_1) & 
          m72_1 > 30 & m72_1 < 100 & age < 24 ~ 0,
        TRUE ~ pnc_nb_timing_all
      )
    ) %>%
    mutate(
      rh_pnc_nb_timing = case_when(
        (pnc_nb_timing_all >= 207 & pnc_nb_timing_all <= 297) | 
          (pnc_nb_timing_all >= 301 & pnc_nb_timing_all < 397) | 
          pnc_nb_timing_all == 0 ~ 0,
        pnc_nb_timing_all == 100 ~ 1,
        pnc_nb_timing_all %in% c(101, 102, 103) ~ 2,
        (pnc_nb_timing_all >= 104 & pnc_nb_timing_all <= 123) | 
          pnc_nb_timing_all == 200 ~ 3,
        (pnc_nb_timing_all >= 124 & pnc_nb_timing_all <= 171) | 
          pnc_nb_timing_all %in% c(201, 202) ~ 4,
        (pnc_nb_timing_all >= 172 & pnc_nb_timing_all <= 197) | 
          pnc_nb_timing_all %in% c(203, 204, 205, 206) ~ 5,
        pnc_nb_timing_all %in% c(198, 199, 298, 299, 398, 399, 998, 999) ~ 9,
        TRUE ~ NA_real_
      ),
      rh_pnc_nb_2days = case_when(
        rh_pnc_nb_timing %in% c(1, 2, 3, 4) ~ 1,
        rh_pnc_nb_timing %in% c(0, 5, 9) ~ 0,
        TRUE ~ NA_real_
      ),
      value = case_when(
        bidx_01 != 1 | age >= 24 ~ NA_real_,
        rh_pnc_nb_2days == 0 ~ 1,
        rh_pnc_nb_2days == 1 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(IRdata))
}
