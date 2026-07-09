#' Households with at least one insecticide-treated mosquito net (ITN) for every two persons and/or indoor residual spraying (IRS) in the past 12 months
#'
#' Households with at least one ITN for every two persons and/or IRS in the past 12 months
#'
#' @param HRdata A DHS Household Recode (HR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
ML_IRSM_H_I2I <- function(HRdata) {
  # Households with at least one ITN for every two persons and/or IRS in past 12 months
  # Denominator: all households
  # Numerator: (num_nets * 2 >= de_jure_members) OR (IRS in past 12 months)
  
  result <- HRdata %>%
    mutate(
      # Adequate ITN coverage: at least 1 net per 2 de jure members
      itn_adequate = case_when(
        is.na(hml1) | is.na(hv012) ~ NA_real_,
        hml1 >= 90 | hv012 <= 0 ~ NA_real_,
        as.numeric(hml1) * 2 >= as.numeric(hv012) ~ 1,
        TRUE ~ 0
      ),
      
      # IRS in past 12 months
      irs = case_when(
        is.na(hv253) ~ NA_real_,
        hv253 %in% c(8, 9) ~ NA_real_,
        hv253 == 1 ~ 1,
        TRUE ~ 0
      ),
      
      # Combined: adequate ITN coverage OR IRS
      value = case_when(
        is.na(itn_adequate) & is.na(irs) ~ NA_real_,
        itn_adequate == 1 | irs == 1 ~ 1,
        TRUE ~ 0
      )
    ) %>%
    select(hv001, hv002, hv005, hv022, hv023, hv024, hv025, value)
  
  return(as.data.frame(result))
}
