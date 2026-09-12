#' No postnatal checkup for newborn
#'
#' Percentage of last births in the two years preceding the survey with no postnatal checkup in the first 7 days after birth
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_RH/RH_PCCT_C_NON.R; validated on ZM_2018 (published 20.9, observed 20.8851, abs_error 0.0149).
#' @export
RH_PCCT_C_NON <- function(IRdata) {
  # No postnatal checkup for newborn (newborn PNC timing category 0).
  # Denominator: women whose most recent birth (bidx_01==1) was in the 2 years
  #   before the survey (age<24 months). Replicates DHS github RH_PNC
  #   rh_pnc_nb_timing (m51_included==0 branch: nbcheck via m70/m74; timing via
  #   m75/m76/m71/m72).
  # @source LLM-authored | seed=RH_PCCT_C_NON | iter=10 | fix=port authoritative DHS
  #   newborn PNC-timing derivation for DHS7-style vars; validated on ZM_2018.
  result <- IRdata %>%
mutate(age = v008 - b3_01) %>%
    mutate(nbcheck = case_when((m70_1==1 | m74_1==1) ~ 1, (m70_1!=1 | m74_1!=1) ~ 0)) %>%
    mutate(pnc = case_when(nbcheck!=1 & age<24 ~ 0, (m76_1>=11 & m76_1<=29) & age<24 ~ as.numeric(m75_1), age<24 & nbcheck==1 ~ 999)) %>%
    mutate(pnc1000 = case_when(pnc<1000 ~ 1)) %>%
    mutate(pnc = case_when(pnc1000==1 & (m76_1>30 & m76_1<100) & age<24 ~ 0, TRUE ~ pnc)) %>%
    mutate(pnc999 = case_when(pnc==999 ~ 1)) %>%
    mutate(pnc = case_when(pnc999==1 & (m72_1>=11 & m72_1<=29) & age<24 ~ m71_1, TRUE ~ pnc)) %>%
    mutate(pnc0 = case_when(m71_1<1000 & m72_1>30 & m72_1<100 & age<24 ~ 1, TRUE ~ 0)) %>%
    mutate(pnc = case_when(pnc0==1 ~ 0, pnc0==0 ~ as.numeric(pnc))) %>%
    mutate(timing = case_when(
      (pnc>=207 & pnc<=297) | (pnc>=301 & pnc<397) | pnc==0 ~ 0,
      pnc %in% c(100) ~ 1,
      pnc %in% c(101,102,103) ~ 2,
      (pnc>=104 & pnc<=123) | pnc==200 ~ 3,
      (pnc>=124 & pnc<=171) | pnc %in% c(201,202) ~ 4,
      (pnc>=172 & pnc<=197) | pnc %in% c(203,204,205,206) ~ 5,
      pnc %in% c(198,199,298,299,399,998,999) ~ 9,
      age>=24 ~ NA_real_)) %>%
    mutate(timing = ifelse(bidx_01!=1 | age>=24, NA_real_, timing)) %>%
    mutate(value = case_when(is.na(timing) ~ NA_real_, timing == 0 ~ 1, TRUE ~ 0)) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
