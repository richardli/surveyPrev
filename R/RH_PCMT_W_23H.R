#' Mother's first postnatal checkup: 4-23 hours
#'
#' Percentage of women giving birth in the two years preceding the survey who had their first postnatal checkup 4-23 hours after birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_PCMT_W_23H.R; validated on ZM_2018 (published 12.8, observed 12.7458, abs_error 0.0542).
#' @export
RH_PCMT_W_23H <- function(IRdata) {
  # Mother's first postnatal checkup: 4-23 hours (timing category 2).
  # Denominator: women whose most recent birth (bidx_01==1) was in the 2 years
  #   before the survey (age<24 months); no-check women are category 0 (kept in
  #   the denominator). Replicates DHS github RH_PNC rh_pnc_wm_timing
  #   (m51_included==0 branch).
  # @source LLM-authored | seed=RH_PCMT_W_23H | iter=8 | fix=port authoritative DHS
  #   PNC-timing derivation; prior iter used an invented m62/m66 rule and
  #   dropped no-check women, inflating the estimate. Validated on ZM_2018.
  result <- IRdata %>%
mutate(age = v008 - b3_01) %>%
    mutate(momcheck = case_when((m62_1!=1 & m66_1!=1) & age<24 ~ 0, (m62_1==1 | m66_1==1) & age<24 ~ 1, age<24 ~ 0)) %>%
    mutate(pnc_wm_time = case_when((m64_1>=11 & m64_1<=29) & age<24 ~ as.numeric(m63_1), age<24 & momcheck==1 ~ 999)) %>%
    mutate(pnc_wm_time1000 = case_when(pnc_wm_time<1000 ~ 1)) %>%
    mutate(pnc_wm_time = case_when(pnc_wm_time1000==1 & (m64_1>30 & m64_1<100) & age<24 ~ 0, TRUE ~ pnc_wm_time)) %>%
    mutate(pnc_wm_time999 = case_when(pnc_wm_time==999 ~ 1)) %>%
    mutate(pnc_wm_time = case_when(pnc_wm_time999==1 & (m68_1>=11 & m68_1<=29) & age<24 ~ m67_1, TRUE ~ pnc_wm_time)) %>%
    mutate(pnc_wm_time0 = case_when(m67_1<1000 & m68_1>30 & m68_1<100 & age<24 ~ 1, TRUE ~ 0)) %>%
    mutate(pnc_wm_time = case_when(pnc_wm_time0==1 ~ 0, pnc_wm_time0==0 ~ as.numeric(pnc_wm_time))) %>%
    mutate(pnc_wm_time00 = case_when(momcheck==0 & age<24 ~ 1)) %>%
    mutate(pnc_wm_time = case_when(pnc_wm_time00==1 ~ 0, TRUE ~ as.numeric(pnc_wm_time))) %>%
    mutate(timing = case_when(
      (pnc_wm_time>=242 & pnc_wm_time<=299) | (pnc_wm_time>=306 & pnc_wm_time<900) | pnc_wm_time==0 ~ 0,
      pnc_wm_time %in% c(100,101,102,103) ~ 1,
      (pnc_wm_time>=104 & pnc_wm_time<=123) | pnc_wm_time==200 ~ 2,
      (pnc_wm_time>=124 & pnc_wm_time<=171) | pnc_wm_time %in% c(201,202) ~ 3,
      (pnc_wm_time>=172 & pnc_wm_time<=197) | pnc_wm_time %in% c(203,204,205,206) ~ 4,
      (pnc_wm_time>=207 & pnc_wm_time<=241) | (pnc_wm_time>=301 & pnc_wm_time<=305) ~ 5,
      pnc_wm_time %in% c(198,199,298,299,399,998,999) ~ 9,
      bidx_01!=1 | age>=24 ~ NA_real_)) %>%
    mutate(value = case_when(is.na(timing) ~ NA_real_, timing == 2 ~ 1, TRUE ~ 0)) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
