##'CN_BRFI_C_EVR
#' @param KRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CN_BRFI_C_EVR", year = 2018)
#' }
#'
#' @export
CN_BRFI_C_EVR <- function(KRdata){
# /*****************************************************************************************************
# Program: 			NT_BF_INIT.R
# Purpose: 			Code to compute initial breastfeeding indicators
# Data inputs: 	KR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Nov 24, 2021 by Shireen Assaf
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# nt_bf_ever			  "Ever breastfed - last-born in the past 2 years"
# nt_bf_start_1hr		"Started breastfeeding within one hour of birth - last-born in the past 2 years"
# nt_bf_start_1day	"Started breastfeeding within one day of birth - last-born in the past 2 years"
# nt_bf_prelac		  "Received a prelacteal feed - last-born in the past 2 years ever breast fed"
# 
# nt_bottle			    "Drank from a bottle with a nipple yesterday - under 2 years"
# ----------------------------------------------------------------------------*/

# age of child. If b19 is not available in the data use v008 - b3
if ("TRUE" %in% (!("b19" %in% names(KRdata))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  KRdata <- KRdata %>%
    mutate(age = b19)
} else {
  KRdata <- KRdata %>%
    mutate(age = v008 - b3)
}


KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# // INITIAL BREASTFEEDING

#//Ever breastfed
KRdata <- KRdata %>%
  mutate(nt_bf_ever =
           case_when(
             (midx==1 & age<24) & m4 %in% c(93,95)  ~ 1 ,
             (midx==1 & age<24) & m4 %in% c(94,98,99) ~ 0)) %>%
  set_value_labels(nt_bf_ever = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(nt_bf_ever = "Ever breastfed - last-born in the past 2 years")



colnames(KRdata)[colnames(KRdata) == 'nt_bf_ever'] <- 'value'
return(KRdata)
}
