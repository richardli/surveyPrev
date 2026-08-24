#' Unmet need for modern methods, total (all young women)
#'
#' Percentage of young women age 15-24 with an unmet need for modern methods
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
FP_NDYA_W_UMT <- function(IRdata) {
  # FP_NDYA_W_UMT: Unmet need for modern methods, total (all young women)
  # Percentage of young women age 15-24 with an unmet need for modern methods
  
  # Subset to young women age 15-24
  df <- IRdata %>%
    filter(v012 >= 15 & v012 <= 24)
  
  # Unmet need for modern methods is defined as:
  # Women who are fecund and want to space or limit but are not using a modern method
  
  # Key variables:
  # v213: Current pregnancy status
  # v312: Current contraceptive method (0 = not using, 1-18 are method codes)
  # v626a: Unmet need status (standard DHS indicator)
  # v632: Intention to use contraception
  
  # Traditional methods (non-modern): 8=periodic abstinence, 9=withdrawal, 10=other traditional
  # Modern methods: 1-7, 11+
  
  result <- df %>%
    mutate(
      # Determine if currently using modern method
      using_modern = case_when(
        is.na(v312) ~ NA_real_,
        v312 == 0 ~ 0,  # Not using
        v312 %in% c(8, 9, 10) ~ 0,  # Traditional method
        v312 >= 1 & v312 <= 18 ~ 1,  # Modern method
        TRUE ~ NA_real_
      ),
      
      # Unmet need for modern methods combines:
      # 1. Women with unmet need (v626a = 1 for spacing, 2 for limiting)
      # 2. Women using traditional methods who want to space/limit
      value = case_when(
        is.na(v626a) | is.na(v312) ~ NA_real_,
        # Has unmet need (spacing or limiting)
        v626a %in% c(1, 2) ~ 1,
        # Using traditional method when modern method is preferred
        v626a == 3 & v312 %in% c(8, 9, 10) ~ 1,
        # All others (met need or no need)
        TRUE ~ 0
      )
    ) %>%
    select(v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(result)
}
