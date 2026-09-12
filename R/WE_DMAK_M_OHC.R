#' Final say in own health care [Men]
#'
#' Percentage of men who say that they alone or jointly have the final say in own health care
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_WE/WE_DMAK_M_OHC.R; validated on ZM_2018 (published 81.8, observed 82.1436, abs_error 0.3436).
#' @export
WE_DMAK_M_OHC <- function(MRdata) {
  # MR recode -> mv* prefix. Weight is mv005/1e6.
  # Denominator: currently married men (mv502 == 1)
  # Numerator: decides on own health care alone or jointly (mv743a in 1:2)
  #   mv743a: 1=respondent alone, 2=respondent & wife jointly, 3+=other
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv502 == 1 & mv743a %in% c(1, 2) ~ 1,
        mv502 == 1 & !is.na(mv743a) & mv743a > 2 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
