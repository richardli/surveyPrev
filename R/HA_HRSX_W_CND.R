##'HA_HRSX_W_CND
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
HA_HRSX_W_CND <- function(IRdata){
# ******************************************************************************
# Program: 			  HK_RSKY_BHV.R
# Purpose: 			  Code to compute Multiple Sexual Partners, Higher-Risk Sexual Partners, and Condom Use 
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# ******************************************************************************

# Variables created in this file ------------------------------------------------
#   
# hk_sex_2plus		"Have two or more sexual partners in the past 12 months"
# hk_sex_notprtnr		"Had sexual intercourse with a person that is not their spouse and does not live with them in the past 12 months"
# hk_cond_2plus		"Have two or more sexual partners in the past 12 months and used a condom at last sex"
# hk_cond_notprtnr	"Used a condom at last sex with a partner that is not their spouse and does not live with them in the past 12 months"
# hk_sexprtnr_mean 	"Mean number of sexual partners"
# 
# *only among men	
# hk_paid_sex_ever	"Ever paid for sex among men 15-49"
# hk_paid_sex_12mo	"Paid for sex in the past 12 months among men 15-49"
# hk_paid_sex_cond	"Used a condom at last paid sexual intercourse in the past 12 months among men 15-49"


# NOTES ------------------------------------------------------------------------
# For the indicator hk_cond_notprtnr, please see the DHS guide to statistics for changes over time.
# The current code will only match recent surveys. 					
# 

# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)

# create weight
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)


# MULTIPLE SEXUAL PARTNERS (WOMEN) ---------------------------------------------

# //Two or more sexual partners
IRdata <- IRdata %>% mutate(hk_sex_2plus = case_when(
  (v527 %in% 100:251 | v527 %in% 300:311) & v766b %in% 2:99 ~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sex_2plus = yesno) %>%
  set_variable_labels(hk_sex_2plus = "Have two or more sexual partners in the past 12 months")

# //Had sex with a person that was not their partner

  # last partner
  IRdata <- IRdata %>% mutate(risk1= case_when(
    (v527 %in% 100:251 | v527 %in% 300:311) & (v767a %in% 2:6 | v767a %in% 8:96) ~ 1,
    TRUE ~ 0))
  
    # next-to-last-partner
    IRdata <- IRdata %>% mutate(risk2 = case_when(
      (v527 %in% 100:251 | v527 %in% 300:311) & (v767b %in% 2:6 | v767b %in% 8:96) ~ 1,
      TRUE ~ 0)) 
    
    # third-to-last-partner
    IRdata <- IRdata %>% mutate(risk3 = case_when(
      (v527 %in% 100:251 | v527 %in% 300:311) & (v767c %in% 2:6 | v767c %in% 8:96) ~ 1,
      TRUE ~ 0))
    
    # combining all partners
    IRdata <- IRdata %>% mutate(hk_sex_notprtnr = case_when(
      risk1>0| risk2>0 |risk3>0 ~ 1,
      TRUE ~ 0)) %>%
      set_value_labels(hk_sex_notprtnr = yesno) %>%
      set_variable_labels(hk_sex_notprtnr = "Had sexual intercourse with a person that is not their spouse and does not live with them in the past 12 months")

# //Have two or more sexual partners and used condom at last sex
IRdata <- IRdata %>% mutate(hk_cond_2plus = case_when(
  (v527 %in% 100:251 | v527 %in% 300:311) & v766b %in% 2:99 & v761==1 ~ 1,
  v766b < 2 ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_cond_2plus = yesno) %>%
  set_variable_labels(hk_cond_2plus = "Have two or more sexual partners in the past 12 months and used a condom at last sex")

# //Had sex with a person that was not their partner and used condom
  # NOTE see risk1, risk2, and risk3 variables above
IRdata <- IRdata %>% mutate(hk_cond_notprtnr = case_when(
  risk1==1 & v761==1 ~ 1,
  risk1!=1 & risk2==1 & v761b==1 ~ 1,
  risk1!=1 & risk2!=1 & risk3==1 & v761c==1 ~ 1,
  hk_sex_notprtnr==1 ~ 0)) %>%  set_value_labels(hk_cond_notprtnr = yesno) %>%
  set_variable_labels(hk_cond_notprtnr = "Used a condom at last sex with a partner that is not their spouse and does not live with them in the past 12 months")



colnames(IRdata)[colnames(IRdata) == 'hk_cond_notprtnr'] <- 'value'
return(IRdata)
}
