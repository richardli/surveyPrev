#' Men with no access to mass media
#'
#' Percentage of men with no access to mass media
#'
#' @details Numerator: men age 15-49 who do not read a newspaper (`mv157`),
#'   listen to the radio (`mv158`), or watch television (`mv159`) at least
#'   once a week (codes 2/3). Denominator: all men age 15-49. The 15-49
#'   filter matches the published "Total 15-49" figure; some MR files
#'   cover men up to age 59.
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_ED/ED_MDIA_M_N3M.R; validated on ZM_2018 (published 30.9, observed 30.8636, abs_error 0.0364) and TZ_2022 (published 34.4, observed 34.4308, abs_error 0.0308).
#' @export


ED_MDIA_M_N3M <- function(MRdata) {
  # Men with no access to mass media.
  # @source LLM-authored | seed=ED_MDIA_M_N3M | iter=2 | fix=restrict to men 15-49 (published Total 15-49; ZM MR is 15-59) | none of newspaper, TV, radio at least once a week (mv157, mv159, mv158 all not in 2:3); denominator all respondents.
  result <- MRdata %>%
    filter(mv012 >= 15 & mv012 <= 49) %>%
    mutate(value = case_when(
      mv157 %in% c(2,3) | mv158 %in% c(2,3) | mv159 %in% c(2,3) ~ 0,
      TRUE ~ 1
    )) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
