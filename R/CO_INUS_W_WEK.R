#' Women who used the internet at least once a week
#'
#' Percentage of women who used the internet at least once a week of those who used the internet in the last 12 months
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CO/CO_INUS_W_WEK.R; validated on ZM_2018 (published 28.2, observed 28.2097, abs_error 0.0097).
#' @export
CO_INUS_W_WEK <- function(IRdata) {
  # Women who used the internet at least once a week. Denominator: those who used the internet in the last 12 months
  # (v171a==1). Numerator: frequency category "at least once a week"
  # (v171b==2). Note: this is a percent-distribution category, so
  # "almost every day" (v171b==3) is a SEPARATE row and is NOT included.
  # @source LLM-authored | seed=CO_INUS_W_WEK | iter=3 | fix=v171b==2 exactly
  #   (not 2:3); validated on ZM_2018 (28.2 / 34.4).
  result <- IRdata %>%
    filter(v171a == 1) %>%
    mutate(value = case_when(v171b == 2 ~ 1, TRUE ~ 0)) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
