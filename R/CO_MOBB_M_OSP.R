#' Men who have a smartphone
#'
#' Percentage of men who have a smartphone
#'
#' @details Numerator: men age 15-49 whose mobile phone is a smartphone
#'   (`mv169c == 1`). Denominator: all men age 15-49. Requires `mv169c`,
#'   which is only fielded in DHS-8 surveys; on DHS-7 surveys the function
#'   errors with "object 'mv169c' not found".
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_CO/CO_MOBB_M_OSP.R; validated on TZ_2022 (published 21.3, observed 21.3103, abs_error 0.0103).
#' @export


CO_MOBB_M_OSP <- function(MRdata) {
  # Men age 15-49 who own a smartphone.
  # Denominator: all men 15-49. Numerator: owns a phone that is a smartphone.
  # @source LLM-authored | seed=CO_MOBB_M_OSP | iter=3 | fix=use mv169c
  #   (mobile phone is a smartphone), mirroring CO_MOBB_W_OSP iter3; prior
  #   iters used mv171a (internet use), which is not smartphone ownership.
  result <- MRdata %>%
    filter(mv012 >= 15 & mv012 <= 49) %>%
    mutate(value = case_when(
      mv169c == 1 ~ 1,
      TRUE ~ 0
    )) %>%
    select(mcaseid, mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  return(as.data.frame(result))
}
