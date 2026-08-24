#' Family planning use decisionmaking mainly by husband
#'
#' Among currently married women using family planning, the percentage for whom the decision was made mainly by the husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_DMKF_W_UHS <- function(IRdata) {
  # FP_DMKF_W_UHS: Family planning use decisionmaking mainly by husband
  # Among currently married women using family planning, the percentage for whom 
  # the decision was made mainly by the husband
  
  # Currently married women (v502 == 1)
  # Using family planning (v312 != 0 & v312 < 90)
  # Decision made mainly by husband (v632 == 2)
  
  result <- IRdata %>%
    filter(
      v502 == 1,                    # Currently married
      !is.na(v312),
      v312 != 0,                    # Using any method
      v312 < 90                     # Exclude special codes
    ) %>%
    mutate(
      value = case_when(
        is.na(v632) ~ NA_real_,
        v632 == 2 ~ 1,              # Mainly husband
        v632 %in% c(1, 3, 4, 6) ~ 0, # Wife, joint, other, or don't know
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      caseid, v001, v002, v005, v021, v022, v023, v024, v025, value
    )
  
  return(result)
}
