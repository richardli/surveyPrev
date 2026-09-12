#' Migration reason: forced displacement [Men]
#'
#' Percentage who moved because of forced displacement
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_AH/AH_MIGR_M_DIS.R; validated on TZ_2022 (published 3.9, observed 3.868, abs_error 0.032).
#' @export
AH_MIGR_M_DIS <- function(MRdata) {
  # Migration reason: forced displacement [Men]
  # Denominator: everyone who ever moved to their current place of residence
  #   (mv104 in 0:94; excludes 95=always lived here and 96:99 missing) with a
  #   known reason for moving (mv175).
  # Numerator: those whose reason for moving was forced displacement (mv175 == 5).
  # @source LLM-authored | seed=AH_MIGR_M_DIS | iter=7 | fix=denominator = ALL lifetime
  #   migrators (was wrongly restricted to <5yr migrants in the numerator);
  #   mv175 code map verified against TZ_2022 labels; distribution matches DHS API.
  reason_col <- if ("mv175" %in% names(MRdata)) MRdata$mv175 else NA_real_
  result <- MRdata %>%
    mutate(.reason = reason_col) %>%
    filter(mv104 %in% 0:94) %>%
    mutate(value = case_when(
      .reason == 5 ~ 1,
      !is.na(.reason)   ~ 0,
      TRUE              ~ NA_real_
    )) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
