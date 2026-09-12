#' Total demand for family planning (married young women)
#'
#' Total demand for family planning is the sum of unmet need for family planning and those currently using family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_FP/FP_NDYM_W_TDT.R; validated on ZM_2018 (published 66, observed 66.0017, abs_error 0.0017).
#' @export
FP_NDYM_W_TDT <- function(IRdata) {
  # Total demand for family planning (currently married young women 15-24) =
  # unmet need + current use. v626a: 1=unmet spacing, 2=unmet limiting,
  # 3=using for spacing, 4=using for limiting (0=no need, 7=infecund/no need,
  # 8/9=missing all count as no demand).
  # @source LLM-authored | seed=FP_NDYM_W_TDT | iter=8 | fix=include current
  #   users (v626a 3,4); prior iter counted only unmet need. Validated ZM_2018.
  result <- IRdata %>%
    filter(v012 >= 15 & v012 <= 24, v502 == 1) %>%
    mutate(value = case_when(
      v626a %in% c(1, 2, 3, 4) ~ 1,
      TRUE ~ 0
    )) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
