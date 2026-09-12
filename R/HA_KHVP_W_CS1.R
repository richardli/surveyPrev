#' Knowledge of HIV prevention methods - Composite of 2 components (prompted) [Women]
#'
#' Percentage of women who, in response to a prompted question, say that people can protect themselves from contracting HIV by using condoms or having sex only with one faithful, uninfected partner.
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_HA/HA_KHVP_W_CS1.R; validated on ZM_2018 (published 79.8, observed 79.8165, abs_error 0.0165).
#' @export
HA_KHVP_W_CS1 <- function(IRdata) {
  # Knowledge of HIV prevention - composite of 2 methods: knows that using
  # condoms (v754cp) AND limiting sex to one uninfected partner (v754dp) reduce
  # HIV risk. Denominator: all women 15-49; don't-know/missing counts as "no".
  # @source LLM-authored | seed=HA_KHVP_W_CS1 | iter=8 | fix=denominator = all
  #   women (prior iter dropped don't-know/missing, inflating the share); ZM_2018.
  result <- IRdata %>%
    mutate(value = case_when(
      v754cp == 1 & v754dp == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  return(as.data.frame(result))
}
