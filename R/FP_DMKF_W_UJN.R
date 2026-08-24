#' Family planning use decisionmaking jointly by wife and husband
#'
#' Among currently married women using family planning, the percentage for whom the decision was made jointly by wife and husband
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_DMKF_W_UJN <- function(IRdata) {
  # FP_DMKF_W_UJN: Among currently married women using family planning,
  # the percentage for whom the decision was made jointly by wife and husband
  
  # Numerator: Currently married women using FP, decision made jointly
  # Denominator: Currently married women using FP
  
  IRdata %>%
    filter(
      v502 == 1,                    # Currently married
      v312 %in% 1:18                # Currently using a method (modern or traditional)
    ) %>%
    mutate(
      # v632: Person who usually makes decision about using contraception
      # 1 = Mainly respondent
      # 2 = Mainly husband/partner
      # 3 = Joint decision
      # 6 = Other
      # 8 = Don't know
      # 9 = Missing
      value = case_when(
        v632 == 3 ~ 1,              # Joint decision
        v632 %in% c(1, 2, 6) ~ 0,   # Other decision-making patterns
        TRUE ~ NA_real_             # DK/Missing
      )
    ) %>%
    select(caseid, v001, v002, v005, v022, v023, v024, v025, value)
}
