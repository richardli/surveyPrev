#' Comprehensive correct knowledge about AIDS [Women]
#'
#' Percentage of women who correctly identify the two major ways of preventing the sexual transmission of HIV (using condoms and limiting sex to one faithful, uninfected partner), who reject the two most common local misconceptions about HIV transmission, and who know that a healthy-looking person can have HIV.
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
HA_CKNA_W_CKA <- function(IRdata) {
  # Comprehensive correct knowledge about AIDS - Women age 15-24
  # All five conditions must be satisfied:
  # 1. v754cp = 1 (condoms reduce risk)
  # 2. v754dp = 1 (one faithful partner reduces risk)
  # 3. v756 = 1 (healthy-looking person can have HIV)
  # 4. v754jp = 0 (mosquito bites do NOT transmit HIV)
  # 5. v754wp = 0 (sharing food does NOT transmit HIV)
  
  out <- IRdata %>%
    filter(v012 >= 15 & v012 <= 24) %>%
    mutate(
      value = case_when(
        # All five conditions met
        v754cp == 1 & v754dp == 1 & v756 == 1 & 
        v754jp == 0 & v754wp == 0 ~ 1,
        
        # Only treat as missing if truly NA (system missing)
        is.na(v754cp) | is.na(v754dp) | is.na(v756) | 
        is.na(v754jp) | is.na(v754wp) ~ NA_real_,
        
        # All other cases (including DK codes 8, 9): does not meet criteria
        TRUE ~ 0
      )
    ) %>%
    select(caseid, v001, v002, v005, v021, v022, v023, v024, v025, value)
  
  return(out)
}
