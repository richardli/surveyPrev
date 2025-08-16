##'FP_KMTA_W_IMP
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "FP_KMTA_W_IMP", year = 2018)
#' }
#'
#' @export
FP_KMTA_W_IMP <- function(IRdata){
# *****************************************************************************
# Program: 			  FP_KNOW.R
# Purpose: 			  Code contraceptive knowledge indicators
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29 2019 by Courtney Allen 
# ****************************************************************************

   
# ----------------------------------------------------------------------------# 
# Variables created in this file:
# 
# fp_know_any			"Know any contraceptive method"
# fp_know_mod			"Know any modern method"
# fp_know_fster		"Know female sterilization"
# fp_know_mster		"Know male sterilization"
# fp_know_pill		"Know pill"
# fp_know_iud			"Know IUD"
# fp_know_inj			"Know injectables"
# fp_know_imp			"Know implants"
# fp_know_mcond		"Know male condoms"
# fp_know_fcond		"Know female condom"
# fp_know_ec			"Know emergency contraception"
# fp_know_sdm			"Know standard days method"
# fp_know_lam			"Know LAM"
# fp_know_omod		"Know other modern method"
# fp_know_trad		"Know any traditional method"
# fp_know_rhy			"Know rhythm method"
# fp_know_wthd		"Know withdrawal method"
# fp_know_other		"Know other method"
# fp_know_mean_all	"Mean number of methods known - all"
# fp_know_mean_mar	"Mean number of methods known - among currently married"
# fp_know_fert_all	"Knowledge of fertile period among all women"
# fp_know_fert_rhy	"Knowledge of fertile period among rhythm method users"
# fp_know_fert_sdm	"Knowledge of fertile period among standard days method users"
# fp_know_fert_cor	"Correct knowledge of fertile period"
# ----------------------------------------------------------------------------*/
  


## KNOWLEDGE OF FAMILY PLANNING METHODS
  

## indicators from IR file

# to correct for the situation where variables that should be named as v304_0`i' but where named v304_`i', where i is from 1 to 9.
if("v304_1" %in% colnames(IRdata)) {
  for(i in 1:9) {
    IRdata <- IRdata %>%
      rename(paste0(v304_0,i) == paste0(v304_,i))
  }
}

  

# Any method 
IRdata <- IRdata %>%
  mutate(fp_know_any = 
           ifelse(v301 > 0 & v301 < 8, 1, 0)) %>%
  set_value_labels(fp_know_any = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_any = "Know any contraceptive method")


# Modern method
IRdata <- IRdata %>%
  mutate(fp_know_mod = 
           ifelse(v301 ==3, 1, 0)) %>%
  set_value_labels(fp_know_mod = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_mod = "Know any modern method")


# Female sterilization  
IRdata <- IRdata %>%
  mutate(fp_know_fster = 
           ifelse(v304_06>0 & v304_06<8, 1, 0)) %>%
  set_value_labels(fp_know_fster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_fster = "Know female sterilization")


# Male sterilization  
IRdata <- IRdata %>%
  mutate(fp_know_mster = 
           ifelse(v304_07>0 & v304_07<8, 1, 0)) %>%
  set_value_labels(fp_know_mster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_mster = "Know male sterilization")


# The contraceptive pill 
IRdata <- IRdata %>%
  mutate(fp_know_pill = 
           ifelse(v304_01>0 & v304_01<8, 1, 0)) %>%
  set_value_labels(fp_know_pill = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_pill = "Know pill")


# Intrauterine contraceptive device 
IRdata <- IRdata %>%
  mutate(fp_know_iud = 
           ifelse(v304_02>0 & v304_02<8, 1, 0)) %>%
  set_value_labels(fp_know_iud = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_iud = "Know IUD")


# Injectables (Depo-Provera) 
IRdata <- IRdata %>%
  mutate(fp_know_inj = 
           ifelse(v304_03>0 & v304_03<8, 1, 0)) %>%
  set_value_labels(fp_know_inj = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_inj = "Know injectables")


# Implants (Norplant) 
IRdata <- IRdata %>%
  mutate(fp_know_imp = 
           ifelse(v304_11>0 & v304_11<8, 1, 0)) %>%
  set_value_labels(fp_know_imp = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_know_imp = "Know implants")




colnames(IRdata)[colnames(IRdata) == 'fp_know_imp'] <- 'value'
return(IRdata)
}
