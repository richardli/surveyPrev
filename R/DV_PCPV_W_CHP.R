#' Physical violence committed by current husband/partner
#'
#' Percentage of women for whom physical violence was committed by current husband or partner
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_DV/DV_PCPV_W_CHP.R; validated on ZM_2018 (published 54.2, observed 54.2494, abs_error 0.0494).
#' @export
DV_PCPV_W_CHP <- function(IRdata) {
  # Physical violence committed by current husband/partner.
  # Denominator: women who experienced physical violence since age 15 (dv_phy).
  # Uses the domestic-violence module weight d005 (not v005): the DV module is a
  # subsample, so d005 is the correct weight. Output as v005 for the estimator.
  # @source LLM-authored | seed=DV_PCPV_W_CHP | iter=11 | fix=replicate DHS
  #   github dv_phy/dv_phy_hus_curr AND weight by d005; validated on ZM_2018.
  result <- IRdata %>%
    mutate(
      dv_phy = case_when(
        d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 | d105j>0 ~ 1,
        d130a>0 ~ 1, d115y==0 ~ 1, d118y==0 ~ 1, v044==1 ~ 0),
      value = case_when(
        v502==1 & (d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 | d105j>0 | d118a==1) ~ 1,
        dv_phy==1 ~ 0),
      v005 = d005
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
