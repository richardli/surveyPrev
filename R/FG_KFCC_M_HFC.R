#' Ever heard of female circumcision (men)
#'
#' Percentage of men who have ever heard of female circumcision
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_FG/FG_KFCC_M_HFC.R; validated on TZ_2022 (published 90.3, observed 90.3335, abs_error 0.0335).
#' @export
FG_KFCC_M_HFC <- function(MRdata) {
  # Ever heard of female circumcision among men age 15-49.
  # Numerator: heard of female circumcision (mg100 == 1) OR, among those who
  #   said no, heard when probed with the "genital cutting" term (mg101 == 1).
  # Denominator: men asked the FGM module (mg100 in 0/1).
  # @source LLM-authored | seed=FG_KFCC_M_HFC | iter=9 | fix=combine mg100
  #   with the mg101 probe (prior iter used mg100 only, undercounting by ~1.8pp);
  #   validated on TZ_2022 (90.3 vs 90.3).
  result <- MRdata %>%
    mutate(value = case_when(
      mg100 == 1 ~ 1,
      mg100 == 0 & mg101 == 1 ~ 1,
      mg100 == 0 ~ 0,
      TRUE ~ NA_real_
    )) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
