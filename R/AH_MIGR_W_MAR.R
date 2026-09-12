#' Migration reason: marriage formation [Women]
#'
#' Percentage who moved because of marriage formation
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGR_W_MAR.R; validated on TZ_2022 (published 25.1, observed 25.1362, abs_error 0.0362).
#' @export
AH_MIGR_W_MAR <- function(IRdata) {
  # Migration reason: marriage formation [Women]
  # Denominator: everyone who ever moved to their current place of residence
  #   (v104 in 0:94; excludes 95=always lived here and 96:99 missing) with a
  #   known reason for moving (v175).
  # Numerator: those whose reason for moving was marriage formation (v175 == 3).
  # @source LLM-authored | seed=AH_MIGR_W_MAR | iter=7 | fix=denominator = ALL lifetime
  #   migrators (was wrongly restricted to <5yr migrants in the numerator);
  #   v175 code map verified against TZ_2022 labels; distribution matches DHS API.
  reason_col <- if ("v175" %in% names(IRdata)) IRdata$v175 else NA_real_
  result <- IRdata %>%
    mutate(.reason = reason_col) %>%
    filter(v104 %in% 0:94) %>%
    mutate(value = case_when(
      .reason == 3 ~ 1,
      !is.na(.reason)   ~ 0,
      TRUE              ~ NA_real_
    )) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
