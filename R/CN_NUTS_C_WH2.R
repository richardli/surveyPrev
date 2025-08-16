##'CN_NUTS_C_WH2
#' @param PRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CN_NUTS_C_WH2", year = 2018)
#' }
#'
#' @export
CN_NUTS_C_WH2 <- function(PRdata){
# /*****************************************************************************************************
# Program: 			NT_CH_NUT.R
# Purpose: 			Code to compute anthropometry and anemia indicators in children
# Data inputs: 	PR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: July 31, 2023 by Shireen Assaf 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# nt_ch_sev_stunt		"Severely stunted child under 5 years"
# nt_ch_stunt			  "Stunted child under 5 years"
# nt_ch_mean_haz		"Mean z-score for height-for-age for children under 5 years"
# nt_ch_sev_wast		"Severely wasted child under 5 years"
# nt_ch_wast			  "Wasted child under 5 years"
# nt_ch_ovwt_ht		  "Overweight for heigt child under 5 years"
# nt_ch_mean_whz		"Mean z-score for weight-for-height for children under 5 years"
# nt_ch_sev_underwt	"Severely underweight child under 5 years"
# nt_ch_underwt		  "Underweight child under 5 years"
# nt_ch_ovwt_age		"Overweight for age child under 5 years"
# nt_ch_mean_waz		"Mean weight-for-age for children under 5 years"
# 	
# nt_ch_any_anem		"Any anemia - child 6-59 months"
# nt_ch_mild_anem		"Mild anemia - child 6-59 months"
# nt_ch_mod_anem		"Moderate anemia - child 6-59 months"
# nt_ch_sev_anem		"Severe anemia - child 6-59 months"
# ----------------------------------------------------------------------------*/
# 
PRdata <- PRdata %>%
  mutate(wt = hv005/1000000)

# *** Anthropometry indicators ***

# //Severely stunted
PRdata <- PRdata %>%
  mutate(nt_ch_sev_stunt =
           case_when(
             hv103==1 &  hc70< -300  ~ 1 ,
             hv103==1 &  hc70>= -300 & hc70<9996 ~ 0 ,
             hc70>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_sev_stunt = c(99))) %>%
  set_value_labels(nt_ch_sev_stunt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_sev_stunt = "Severely stunted child under 5 years")

# //Stunted
PRdata <- PRdata %>%
  mutate(nt_ch_stunt =
           case_when(
             hv103==1 &  hc70< -200  ~ 1 ,
             hv103==1 &  hc70>= -200 & hc70<9996 ~ 0 ,
             hc70>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_stunt = c(99))) %>%
  set_value_labels(nt_ch_stunt = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_stunt = "Stunted child under 5 years")

# //Mean haz
PRdata <- PRdata %>%
  mutate(haz = case_when(hv103 ==1 & hc70<996 ~ hc70/100)) 
  PRdata$nt_ch_mean_haz <- matrixStats::weightedMean(PRdata$haz, PRdata$wt, idxs = NULL, na.rm = TRUE) 

# //Severely wasted 
PRdata <- PRdata %>%
  mutate(nt_ch_sev_wast =
           case_when(
             hv103==1 &  hc72< -300  ~ 1 ,
             hv103==1 &  hc72>= -300 & hc72<9996 ~ 0 ,
             hc72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_sev_wast = c(99))) %>%
  set_value_labels(nt_ch_sev_wast = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_sev_wast = "Severely wasted child under 5 years")

# //Wasted
PRdata <- PRdata %>%
  mutate(nt_ch_wast =
           case_when(
             hv103==1 &  hc72< -200  ~ 1 ,
             hv103==1 &  hc72>= -200 & hc72<9996~ 0 ,
             hc72>=9996 ~ 99)) %>%
  replace_with_na(replace = list(nt_ch_wast = c(99))) %>%
  set_value_labels(nt_ch_wast = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_ch_wast = "Wasted child under 5 years")



colnames(PRdata)[colnames(PRdata) == 'nt_ch_wast'] <- 'value'
return(PRdata)
}
