#' Sought help to stop violence
#'
#' Percentage of ever married women or never married women who have ever experienced any physical or sexual violence who sought help to stop violence
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_DV/DV_STPV_W_HLP.R; validated on ZM_2018 (published 35, observed 35.0088, abs_error 0.0088).
#' @export
DV_STPV_W_HLP <- function(IRdata) {
  result <- IRdata %>%
    mutate(
      value = case_when(
        v044 != 1 ~ NA_real_,
        !(d105a > 0 | d105b > 0 | d105c > 0 | d105d > 0 | d105e > 0 | 
          d105f > 0 | d105g > 0 | d105h > 0 | d105i > 0 | d105j > 0 | 
          d105k > 0 | d130a > 0 | d130b > 0 | d115y == 0 | d118y == 0 | 
          d124 == 1 | d125 == 1) ~ NA_real_,
        d119y == 0 ~ 1,
        d119y == 1 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    mutate(v005 = d005) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
