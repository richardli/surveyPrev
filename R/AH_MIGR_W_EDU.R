#' Migration reason: education/ training [Women]
#'
#' Percentage who moved because of education/ training
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGR_W_EDU.R; validated on TZ_2022 (published 3.4, observed 3.3598, abs_error 0.0402).
#' @export
AH_MIGR_W_EDU <- function(IRdata) {
  # Migration reason: education/training [Women]
  # Denominator: everyone who ever moved to their current place of residence
  #   (v104 in 0:94; excludes 95=always lived here and 96:99 missing) with a
  #   known reason for moving (v175).
  # Numerator: those whose reason for moving was education/training (v175 == 2).
  # @source LLM-authored | seed=AH_MIGR_W_EDU | iter=6 | fix=denominator = ALL lifetime
  #   migrators (was wrongly restricted to <5yr migrants in the numerator);
  #   v175 code map verified against TZ_2022 labels; distribution matches DHS API.
  reason_col <- if ("v175" %in% names(IRdata)) IRdata$v175 else NA_real_
  result <- IRdata %>%
    mutate(.reason = reason_col) %>%
    filter(v104 %in% 0:94) %>%
    mutate(value = case_when(
      .reason == 2 ~ 1,
      !is.na(.reason)   ~ 0,
      TRUE              ~ NA_real_
    )) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
