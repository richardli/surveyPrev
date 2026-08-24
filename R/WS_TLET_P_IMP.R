#' Population with an improved sanitation facility
#'
#' Percentage of the de jure population living in households with an improved sanitation facility
#'
#' @param PRdata A DHS Household Member (PR) Recode data.frame.
#' @return A data.frame with a `value` column.
#' @export
WS_TLET_P_IMP <- function(PRdata) {
  # Filter to de jure population (usual residents)
  PRdata <- PRdata[PRdata$hv102 == 1 & !is.na(PRdata$hv102), ]
  
  # Define improved sanitation facilities per DHS definition
  # 11=flush to sewer, 12=flush to septic, 13=flush to pit,
  # 15=flush unknown, 21=VIP, 22=pit with slab, 41=composting
  improved <- c(11, 12, 13, 15, 21, 22, 41)
  
  # Create binary indicator
  PRdata$value <- ifelse(is.na(PRdata$hv205), NA,
                        ifelse(PRdata$hv205 %in% improved, 1, 0))
  
  # Preserve survey design columns
  result <- PRdata[, c("hv001", "hv002", "hv005", "hv022", "hv023", 
                       "hv024", "hv025", "value")]
  
  return(result)
}
