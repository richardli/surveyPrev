#' Problems in accessing health care: Any of the specified problems
#'
#' Percentage of women who reported they have big problems in any of the specified problems for treatment for themselves when they are sick
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
RH_PAHC_W_PR1 <- function(IRdata) {
  # Denominator: All women age 15-49 (no filter needed)
  # Numerator: At least one of 6 problems in accessing health care
  # Variables: v467b (permission), v467c (money), v467d (distance), 
  #            v467e (transport), v467f (alone), v467g (no female provider)
  # Coding: 1=big problem, 0/2=no problem or not big, 9/NA=missing
  
  result <- IRdata %>%
    mutate(
      p_permit = case_when(v467b == 1 ~ 1, v467b %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
      p_money = case_when(v467c == 1 ~ 1, v467c %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
      p_dist = case_when(v467d == 1 ~ 1, v467d %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
      p_transport = case_when(v467e == 1 ~ 1, v467e %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
      p_alone = case_when(v467f == 1 ~ 1, v467f %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
      p_female = case_when(v467g == 1 ~ 1, v467g %in% c(0, 2) ~ 0, TRUE ~ NA_real_),
      # Composite: at least one problem
      value = case_when(
        p_permit == 1 | p_money == 1 | p_dist == 1 | p_transport == 1 | p_alone == 1 | p_female == 1 ~ 1,
        is.na(p_permit) & is.na(p_money) & is.na(p_dist) & is.na(p_transport) & is.na(p_alone) & is.na(p_female) ~ NA_real_,
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
