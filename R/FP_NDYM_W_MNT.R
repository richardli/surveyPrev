#' Met need for family planning (currently using), total (married young women)
#'
#' Percentage of currently married or in union young women age 15-24 using family planning
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYM_W_MNT <- function(IRdata) {
  # FP_NDYM_W_MNT: Percentage of currently married or in union young women age 15-24 using family planning
  # Numerator: Currently married/in union young women age 15-24 currently using any family planning method
  # Denominator: All currently married/in union young women age 15-24
  
  IRdata %>%
    filter(
      v012 >= 15 & v012 <= 24,  # Young women age 15-24
      v502 == 1                  # Currently married or in union
    ) %>%
    mutate(
      # v312 = current contraceptive method
      # 0 = not using, 1-19 = using a method (modern or traditional)
      value = case_when(
        v312 >= 1 & v312 <= 19 ~ 1,  # Currently using any method
        v312 == 0 ~ 0,                 # Not currently using
        TRUE ~ NA_real_
      )
    ) %>%
    select(
      caseid,
      v001, v002, v005,  # cluster, household, weight
      v022, v023,         # strata
      v024, v025,         # region, urban/rural
      value
    )
}
