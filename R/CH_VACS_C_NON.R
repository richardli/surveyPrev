#' Received no vaccinations
#'
#' Percentage of children 12-23 (or 24-35) months who had received no vaccinations
#'
#' @param KRdata A DHS Children's Recode (KR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
CH_VACS_C_NON <- function(KRdata) {
  # Denominator: living children age 12-23 months
  # Numerator: children who received NO vaccinations
  
  result <- KRdata %>%
    filter(b5 == 1, b19 >= 12, b19 <= 23) %>%
    mutate(
      # For each core vaccine: 1 if received (codes 1,2,3), 0 if not (code 0), NA otherwise
      # BCG, DPT 1-3, Polio 1-3, Measles
      h2_rcv = ifelse(h2 %in% c(1, 2, 3), 1, ifelse(h2 == 0, 0, NA)),
      h3_rcv = ifelse(h3 %in% c(1, 2, 3), 1, ifelse(h3 == 0, 0, NA)),
      h4_rcv = ifelse(h4 %in% c(1, 2, 3), 1, ifelse(h4 == 0, 0, NA)),
      h5_rcv = ifelse(h5 %in% c(1, 2, 3), 1, ifelse(h5 == 0, 0, NA)),
      h6_rcv = ifelse(h6 %in% c(1, 2, 3), 1, ifelse(h6 == 0, 0, NA)),
      h7_rcv = ifelse(h7 %in% c(1, 2, 3), 1, ifelse(h7 == 0, 0, NA)),
      h8_rcv = ifelse(h8 %in% c(1, 2, 3), 1, ifelse(h8 == 0, 0, NA)),
      h9_rcv = ifelse(h9 %in% c(1, 2, 3), 1, ifelse(h9 == 0, 0, NA)),
      
      # Count vaccines received and valid records
      vacc_count = rowSums(cbind(h2_rcv, h3_rcv, h4_rcv, h5_rcv, h6_rcv, h7_rcv, h8_rcv, h9_rcv), na.rm = TRUE),
      vacc_valid = rowSums(!is.na(cbind(h2_rcv, h3_rcv, h4_rcv, h5_rcv, h6_rcv, h7_rcv, h8_rcv, h9_rcv))),
      
      # No vaccinations: valid info exists AND count is zero
      value = case_when(
        vacc_valid == 0 ~ NA_real_,
        vacc_count > 0 ~ 0,
        TRUE ~ 1
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(result))
}
