#' Physical violence committed by former husband/partner
#'
#' Percentage of women for whom physical violence was committed by former husband or partner
#'
#' @param IRdata A DHS Individual Recode (IR) data.frame.
#' @return A data.frame with a `value` column.
#' @export
DV_PCPV_W_FHP <- function(IRdata) {
  
  # Defensive handling of variables that may not exist
  if (!"d130a" %in% names(IRdata)) IRdata$d130a <- NA_real_
  if (!"d100" %in% names(IRdata)) IRdata$d100 <- NA_real_
  if (!"d115y" %in% names(IRdata)) IRdata$d115y <- NA_real_
  if (!"d118y" %in% names(IRdata)) IRdata$d118y <- NA_real_
  
  IRdata <- IRdata %>%
    mutate(
      # Experienced ANY physical violence since age 15 (denominator)
      dv_phy = case_when(
        d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 | d105j>0 ~ 1, 
        d130a>0  ~ 1,  
        d115y==0 ~ 1, 
        d118y==0 ~ 1, 
        v044==1  ~ 0 
      ),
      
      # Violence by former husband/partner (numerator)
      value = case_when(
        # Former partner identified (any marital status)
        d115j == 1 | d118j == 1 ~ 1,
        
        # Previous husband (ever-married only)
        (v502 %in% c(1, 2)) & (d130a>0 & d130a<=4) ~ 1,
        
        # Formerly married & d105 violence (ex-partner)
        (v502 == 2) & (d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 | d105j>0) ~ 1,
        
        # Never married & had former intimate partner
        (v502 == 0) & (d100 == 2) ~ 1,
        
        # Experienced violence but not by former partner
        dv_phy == 1 ~ 0
      )
    ) %>%
    select(v001, v002, v005, v022, v023, v024, v025, value)
  
  return(as.data.frame(IRdata))
}
