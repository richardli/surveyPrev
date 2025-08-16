##'FP_CUSA_W_FCN
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "FP_CUSA_W_FCN", year = 2018)
#' }
#'
#' @export
FP_CUSA_W_FCN <- function(IRdata){
# ******************************************************************************
# Program: 			  FP_USE.do
# Purpose: 		    Code contraceptive use indicators (ever and current use). Also source of method, brands, and information given. 
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29  2021 by Courtney Allen
# ******************************************************************************

# NOTE: this script is created to run from the FPmain.R file where the following libraries are loaded
# -----------------------------------------------------------------------------#
# Variables created in this file:
# 
# fp_evuse_any		"Ever used any contraceptive method"
# fp_evuse_mod		"Ever used any modern method"
# fp_evuse_fster	"Ever used female sterilization"
# fp_evuse_mster	"Ever used male sterilization"
# fp_evuse_pill		"Ever used pill"
# fp_evuse_iud		"Ever used IUD"
# fp_evuse_inj		"Ever used injectables"
# fp_evuse_imp		"Ever used implants"
# fp_evuse_mcond	"Ever used male condoms"
# fp_evuse_fcond	"Ever used female condom"
# fp_evuse_diaph	"Ever used diaphragm"
# fp_evuse_lam		"Ever used LAM"
# fp_evuse_ec			"Ever used emergency contraception"
# fp_evuse_omod		"Ever used other modern method"
# fp_evuse_trad		"Ever used any traditional method"
# fp_evuse_rhy		"Ever used rhythm"
# fp_evuse_wthd		"Ever used withdrawal"
# fp_evuse_other	"Ever used other"
# 
# fp_cruse_any		"Currently use any contraceptive method"
# fp_cruse_mod		"Currently use any modern method
# fp_cruse_fster	"Currently use female sterilization"
# fp_cruse_mster	"Currently use male sterilization"
# fp_cruse_pill		"Currently use pill"
# fp_cruse_iud		"Currently use IUD"
# fp_cruse_inj		"Currently use injectables"
# fp_cruse_imp		"Currently use implants"
# fp_cruse_mcond	"Currently use male condoms"
# fp_cruse_fcond	"Currently use female condom"
# fp_cruse_diaph	"Currently use diaphragm"
# fp_cruse_lam		"Currently use LAM"
# fp_cruse_ec			"Currently use emergency contraception"
# fp_cruse_omod		"Currently use other modern method"
# fp_cruse_trad		"Currently use any traditional method"
# fp_cruse_rhy		"Currently use rhythm"
# fp_cruse_wthd		"Currently use withdrawal"
# fp_cruse_other	"Currently use other"
# 
# fp_ster_age			"Age at time of sterilization for women"
# fp_ster_median	"Median age at time of sterilization for women"
# 
# fp_source_tot		"Source of contraception - total"
# fp_source_fster	"Source for female sterilization"
# fp_source_pill	"Source for pill"
# fp_source_iud		"Source for IUD"
# fp_source_inj		"Source for injectables"
# fp_source_imp		"Source for implants"
# fp_source_mcond	"Source for male condom"
# 
# fp_brand_pill		"Pill users using a social marketing brand"
# fp_brand_cond		"Male condom users using a social marketing brand"
# 
# fp_info_sideff		  "Informed about side effects or problems among female sterilization, pill, IUD, injectables, and implant users"
# fp_info_what_to_do	"Informed of what to do if experienced side effects among female sterilization, pill, IUD, injectables, and implant users"
# fp_info_other_meth	"Informed of other methods by health or FP worker among female sterilization, pill, IUD, injectables, and implant users"
# fp_info_all 		    "Informed of all three (method information index) among female sterilization, pill, IUD, injectables, and implant users"

#------------------------------------------------------------------------------


# Ever used any contraceptive method 
IRdata <- IRdata %>%
  mutate(fp_evuse_any = 
           ifelse(v302 > 0  & v302 < 8, 1, 0)) %>%  
  set_value_labels(fp_evuse_any = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_any = "Ever used any contraceptive method")


# Ever use modern method
IRdata <- IRdata %>%
  mutate(fp_evuse_mod = 
           ifelse(v302 == 3, 1, 0)) %>%   
  set_value_labels(fp_evuse_mod = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_mod =  "Ever used any modern method")


# Ever use female sterilization  
IRdata <- IRdata %>%
  mutate(fp_evuse_fster = 
           ifelse(v305_06 > 0 & v305_06 < 8, 1, 0)) %>%
  set_value_labels(fp_evuse_fster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_fster = "Ever used female sterilization")


# Ever use male sterilization  
IRdata <- IRdata %>%
  mutate(fp_evuse_mster = 
           ifelse(v305_07 > 0 & v305_07 < 8, 1, 0)) %>%  
  set_value_labels(fp_evuse_mster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_mster = "Ever used male sterilization")


# Ever use the contraceptive pill  
IRdata <- IRdata %>%
  mutate(fp_evuse_pill = 
           ifelse(v305_01 > 0 & v305_01 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_pill = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_pill = "Ever used pill")


# Ever use Interuterine contraceptive device (IUD)
IRdata <- IRdata %>%
  mutate(fp_evuse_iud = 
           ifelse(v305_02 > 0 & v305_02 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_iud = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_iud = "Ever used IUD")


# Ever use injectables (Depo-Provera) 
IRdata <- IRdata %>%
  mutate(fp_evuse_inj = 
           ifelse(v305_03 > 0 & v305_03 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_inj = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_inj = "Ever used injectables")


# Ever use implants (Norplant)  
IRdata <- IRdata %>%
  mutate(fp_evuse_imp = 
           ifelse(v305_11 > 0 & v305_11 < 8, 1, 0)) %>%  
  set_value_labels(fp_evuse_imp = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_imp =  "Ever used implants")


# Ever use male condoms  
IRdata <- IRdata %>%
  mutate(fp_evuse_mcond = 
           ifelse(v305_05 > 0 & v305_05 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_mcond = c(yes = 1, no = 0)) %>%
           set_variable_labels(fp_evuse_mcond = "Ever used male condoms")


# Ever use female condoms 
IRdata <- IRdata %>%
  mutate(fp_evuse_fcond = 
           ifelse(v305_14 > 0 & v305_14 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_fcond = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_fcond =  "Ever used female condoms")


# Ever use diaphragm  
IRdata <- IRdata %>%
  mutate(fp_evuse_diaph = 
           ifelse(v305_04 > 0 & v305_04 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_diaph = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_diaph =  "Ever used diaphragm")


# Ever use standard days method (SDM) 
IRdata <- IRdata %>%
  mutate(fp_evuse_sdm = 
           ifelse(v305_18 > 0 & v305_18 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_sdm = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_sdm = "Ever used standard days method")


# Ever use Lactational amenorrhea method (LAM) 
IRdata <- IRdata %>%
  mutate(fp_evuse_lam = 
           ifelse(v305_13 > 0 & v305_13 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_lam = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_lam =  "Ever used LAM")


## Ever use emergency contraception  
IRdata <- IRdata %>%
  mutate(fp_evuse_ec = 
           ifelse(v305_16 > 0 & v305_16 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_ec = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_ec = "Ever used emergency contraception")


# Ever use country-specific modern methods and other modern contraceptive methods 
IRdata <- IRdata %>%
  mutate(fp_evuse_omod = 
           ifelse(v305_17 > 0 & v305_17 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_omod = c(yes = 1, no = 0)) %>%
set_variable_labels(fp_evuse_omod = "Ever used other modern method")


# Ever use periodic abstinence (rhythm, calendar method) 
IRdata <- IRdata %>%
  mutate(fp_evuse_rhy = 
           ifelse(v305_08 > 0 & v305_08 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_any = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_rhy = "Ever used rhythm method")


# Ever use withdrawal  
IRdata <- IRdata %>%
  mutate(fp_evuse_wthd = 
           ifelse(v305_09 > 0 & v305_09 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_wthd = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_wthd =  "Ever used withdrawal method")


# Ever use country-specific traditional methods, and folk methods 
IRdata <- IRdata %>%
  mutate(fp_evuse_other = 
           ifelse(v305_10 > 0 & v305_10 < 8, 1, 0)) %>%   
  set_value_labels(fp_evuse_other = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_other =  "Ever used other method")


# Ever use any traditional 
IRdata <- IRdata %>%
  mutate(fp_evuse_trad = 
           ifelse(fp_evuse_rhy==1 | fp_evuse_wthd==1 | fp_evuse_other==1, 1, 0)) %>%
  set_value_labels(fp_evuse_rhy = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_evuse_trad = "Ever used any traditional method")
#------------------------------------------------------------------------------#



### Current use of contraceptive methods
 
 

# Currently use any method
IRdata <- IRdata %>%
  mutate(fp_cruse_any = 
           ifelse(v313 > 0 & v313 < 8, 1, 0)) %>%   
  set_value_labels(fp_cruse_any = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_any =  "Currently used any contraceptive method")

# Currently use modern method
IRdata <- IRdata %>%
  mutate(fp_cruse_mod = 
           ifelse(v313 == 3, 1, 0)) %>%   
  set_value_labels(fp_cruse_mod = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_mod ="Currently used any modern method")

# Currently use female sterilization  
IRdata <- IRdata %>%
  mutate(fp_cruse_fster = 
         ifelse(v312 == 6, 1, 0)) %>%   
  set_value_labels(fp_cruse_fster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_fster = "Currently used female sterilization")

# Currently use male sterilization  
IRdata <- IRdata %>%
  mutate(fp_cruse_mster = 
         ifelse(v312 == 7, 1, 0)) %>%   
  set_value_labels(fp_cruse_mster = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_mster = "Currently used male sterilization")

# Currently use the contraceptive pill 
IRdata <- IRdata %>%
  mutate(fp_cruse_pill = 
         ifelse(v312 == 1, 1, 0)) %>%   
  set_value_labels(fp_cruse_pill = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_pill = "Currently used pill")

# Currently use Interuterine contraceptive device 
IRdata <- IRdata %>%
  mutate(fp_cruse_iud = 
         ifelse(v312 == 2, 1, 0)) %>%  
  set_value_labels(fp_cruse_iud = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_iud =  "Currently used IUD")

# Currently use injectables (Depo-Provera) 
IRdata <- IRdata %>%
  mutate(fp_cruse_inj = 
         ifelse(v312 == 3, 1, 0)) %>%   
  set_value_labels(fp_cruse_inj = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_inj = "Currently used injectables")

# Currently use implants (Norplant) 
IRdata <- IRdata %>%
  mutate(fp_cruse_imp = 
         ifelse(v312 == 11, 1, 0)) %>%   
  set_value_labels(fp_cruse_imp = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_imp = "Currently used implants")

# Currently use male condom 
IRdata <- IRdata %>%
  mutate(fp_cruse_mcond = 
         ifelse(v312 == 5, 1, 0)) %>%   
  set_value_labels(fp_cruse_mcond = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_mcond = "Currently used male condoms")

# Currently use female condom 
IRdata <- IRdata %>%
  mutate(fp_cruse_fcond = 
         ifelse(v312 == 14, 1, 0)) %>%   
  set_value_labels(fp_cruse_fcond = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_cruse_fcond = "Currently used female condom")



colnames(IRdata)[colnames(IRdata) == 'fp_cruse_fcond'] <- 'value'
return(IRdata)
}
