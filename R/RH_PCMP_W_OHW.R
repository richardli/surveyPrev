#' Provider of mothers' first postnatal checkup: Other health worker
#'
#' Percentage of women giving birth in the two years preceding the survey who received their first postnatal checkup in the first two days after birth from another type of health worker
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PCMP_W_OHW <- function(IRdata) {
  # Denominator: Women with live birth or stillbirth in last 2 years
  # Numerator: First PNC within 2 days from "other health worker"
  # Other health worker = residual (not doctor/nurse/midwife/aux/CHW/TBA)
  
  result <- IRdata %>%
    mutate(
      # Recent birth in last 24 months
      recent_birth = !is.na(b3_01) & (v008 - b3_01) < 24,
      
      # Determine timing and provider of first PNC
      pnc_timing = case_when(
        !is.na(m62_1) & m62_1 == 1 ~ m63_1,  # Check before discharge
        !is.na(m66_1) & m66_1 == 1 ~ m67_1,  # Check after discharge/home
        TRUE ~ NA_real_
      ),
      pnc_provider = case_when(
        !is.na(m62_1) & m62_1 == 1 ~ m64_1,  # Provider before discharge
        !is.na(m66_1) & m66_1 == 1 ~ m68_1,  # Provider after discharge/home
        TRUE ~ NA_real_
      ),
      
      # Within 2 days: codes 100-199 (hours) or 200-202 (0-2 days)
      within_2days = case_when(
        is.na(pnc_timing) ~ FALSE,
        pnc_timing >= 100 & pnc_timing < 200 ~ TRUE,
        pnc_timing >= 200 & pnc_timing <= 202 ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # Other health worker: excluding main categories from sibling indicators
      # Main: 11/12 (doc/nurse/midwife), 13 (aux), 21 (TBA), 22 (CHW)
      value = case_when(
        !recent_birth ~ NA_real_,
        !within_2days ~ 0,
        is.na(pnc_provider) ~ NA_real_,
        pnc_provider %in% c(98, 99) ~ NA_real_,
        pnc_provider %in% c(11, 12, 13, 21, 22) ~ 0,
        pnc_provider > 0 & pnc_provider < 98 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
