#' Wife beating justified for at least one specific reason [Men]
#'
#' Percentage of men who agree that a husband is justified in hitting or beating his wife for at least one specific reason
#'
#' @param MRdata A DHS Men's Recode (MR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_WE/WE_AWBT_M_AGR.R; validated on ZM_2018 (published 24.8, observed 24.1433, abs_error 0.6567).
#' @export
WE_AWBT_M_AGR <- function(MRdata) {
  # Men's recode -> mv* prefix. Weight is mv005/1e6.
  # Denominator: all men 15-49 (no filter per guide).
  # Numerator: agrees beating justified for at least one specific reason
  #            (any of mv744a-mv744e == 1).
  
  result <- MRdata %>%
    mutate(
      value = case_when(
        mv744a == 1 | mv744b == 1 | mv744c == 1 | mv744d == 1 | mv744e == 1 ~ 1,
        !is.na(mv744a) & !is.na(mv744b) & !is.na(mv744c) & 
          !is.na(mv744d) & !is.na(mv744e) ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(mv001, mv002, mv005, mv022, mv023, mv024, mv025, value)
  
  return(as.data.frame(result))
}
