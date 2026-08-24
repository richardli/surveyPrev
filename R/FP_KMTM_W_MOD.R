#' Knowledge of any modern method of contraception (married women)
#'
#' Percentage of currently married or in union women who know any modern method of contraception
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_KMTM_W_MOD <- function(IRdata) {
  # Denominator: Currently married women (v502 == 1)
  # Numerator: Knows at least one modern contraceptive method
  # Modern methods per DHS guide:
  # pill (01), IUD (02), injectables (03), diaphragm (04), condom (05),
  # female sterilization (06), male sterilization (07), implants (11),
  # LAM (13), female condom (14), foam/jelly (15), emergency contraception (16),
  # other modern (17), SDM (18)
  
  result <- IRdata %>%
    mutate(
      knows_any_modern = (
        v304_01 %in% c(1, 2) | v304_02 %in% c(1, 2) | v304_03 %in% c(1, 2) |
        v304_04 %in% c(1, 2) | v304_05 %in% c(1, 2) | v304_06 %in% c(1, 2) |
        v304_07 %in% c(1, 2) | v304_11 %in% c(1, 2) | v304_13 %in% c(1, 2) |
        v304_14 %in% c(1, 2) | v304_15 %in% c(1, 2) | v304_16 %in% c(1, 2) |
        v304_17 %in% c(1, 2) | v304_18 %in% c(1, 2)
      ),
      value = case_when(
        v502 == 1 & knows_any_modern ~ 1,
        v502 == 1 & !knows_any_modern ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
