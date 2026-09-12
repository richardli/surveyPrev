#' Women who experienced physical or sexual violence
#'
#' Percentage of women who experienced physical or sexual violence
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column (1/0/NA), ready for
#'   surveyPrev::getDHSindicator().
#' @source DHS-indicator-factory sideproject/results/passing_current/Chap_DV/DV_FMVL_W_POS.R; validated on ZM_2018 (published 39.2, observed 38.5855, abs_error 0.6145).
#' @export
DV_FMVL_W_POS <- function(IRdata) {
  result <- IRdata %>%
    mutate(
      # Physical violence by anyone
      phys_any = case_when(
        v044 != 1 ~ NA_real_,
        d105a > 0 | d105b > 0 | d105c > 0 | d105d > 0 | 
        d105e > 0 | d105f > 0 | d105g > 0 | d105j > 0 |
        d111 > 0 | d130a > 0 | d115y == 0 ~ 1,
        TRUE ~ 0
      ),
      # Sexual violence by anyone
      sex_any = case_when(
        v044 != 1 ~ NA_real_,
        d105h > 0 | d105i > 0 | d105k > 0 |
        d124 > 0 | d127 > 0 ~ 1,
        TRUE ~ 0
      ),
      # Physical OR sexual violence
      value = case_when(
        v044 != 1 ~ NA_real_,
        phys_any == 1 | sex_any == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    mutate(v005 = d005) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
