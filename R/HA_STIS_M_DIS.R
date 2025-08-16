##'HA_STIS_M_DIS
#' @param MRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "HA_STIS_M_DIS", year = 2018)
#' }
#'
#' @export
HA_STIS_M_DIS <- function(MRdata){
# ******************************************************************************
# Program: 			  HK_STIL.R
# Purpose: 			  Code to compute indicators on Sexually Transmitted Infections (STI)
# Data inputs: 		IR and MR datasets
# Data outputs:		coded variables
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_sti				"Had STI in the past 12 months"
# hk_gent_disch		"Had abnormal (or bad-smelling) genital discharge in the past 12 months"
# hk_gent_sore		"Had genital sore or ulcer in the past 12 months"
# hk_sti_symp			"Had STI or STI symptoms in the past 12 months"
# hk_sti_trt_doc		"Had STI or STI symptoms in the past 12 months and sought advice or treatment from a clinic/hospital/private doctor"
# hk_sti_trt_pharm	"Had STI or STI symptoms in the past 12 months and sought advice or treatment from a pharmacy"
# hk_sti_trt_other	"Had STI or STI symptoms in the past 12 months and sought advice or treatment from any other source"
# hk_sti_notrt		"Had STI or STI symptoms in the past 12 months and sought no advice or treatment"




# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)


# SELF REPORT STIS (MEN) -------------------------------------------------------



MRdata <- MRdata %>%  mutate(hk_sti = case_when(
  mv763a==1 & mv525==1~ 1,
  TRUE ~ 0)) %>%
  set_value_labels(hk_sti = yesno) %>%
  set_variable_labels(hk_sti = "Had STI in the past 12 months")

# Discharge in the past 12 months
MRdata <- MRdata %>%  mutate(hk_gent_disch = case_when(
  mv763c==1 ~ 1,
  mv525==0 | mv525==99 | is.na(mv525) ~ NA_real_,
  TRUE ~ 0)) %>%
  set_value_labels(hk_gent_disch = yesno) %>%
  set_variable_labels(hk_gent_disch= "Had abnormalgenital discharge in past 12 mnths")



colnames(MRdata)[colnames(MRdata) == 'hk_gent_disch'] <- 'value'
return(MRdata)
}
