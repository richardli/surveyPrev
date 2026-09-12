#' Men who used the internet at least once a week
#'
#' Percentage of men who used the internet at least once a week of those who used the internet in the last 12 months
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CO/CO_INUS_M_WEK.R; validated on ZM_2018 (published 34.4, observed 34.1641, abs_error 0.2359).
#' @export
CO_INUS_M_WEK <- function(MRdata) {
  # Men who used the internet at least once a week. Denominator: those who used the internet in the last 12 months
  # (mv171a==1). Numerator: frequency category "at least once a week"
  # (mv171b==2). Note: this is a percent-distribution category, so
  # "almost every day" (mv171b==3) is a SEPARATE row and is NOT included.
  # @source LLM-authored | seed=CO_INUS_M_WEK | iter=3 | fix=mv171b==2 exactly
  #   (not 2:3); validated on ZM_2018 (28.2 / 34.4).
  result <- MRdata %>%
    filter(mv171a == 1) %>%
    mutate(value = case_when(mv171b == 2 ~ 1, TRUE ~ 0)) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
