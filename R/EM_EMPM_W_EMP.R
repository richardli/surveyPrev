##'EM_EMPM_W_EMP
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "EM_EMPM_W_EMP", year = 2018)
#' }
#'
#' @export
EM_EMPM_W_EMP <- function(IRdata){
# /*****************************************************************************************************
# Program: 			  WE_ASSETS.R
# Purpose: 			  Code to compute employment, earnings, and asset ownership in men and women
# Data inputs: 		IR or MR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: Nov 18, 2021 by Shireen Assaf 
# Note:				    The indicators below can be computed for men and women. 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# we_empl				      "Employment status in the last 12 months among those currently in a union"
# we_empl_earn		    "Type of earnings among those employed in the past 12 months and currently in a union"
# we_earn_wm_decide	  "Who decides on wife's cash earnings for employment in the last 12 months"
# we_earn_wm_compare	"Comparison of cash earnings with husband's cash earnings"
# we_earn_mn_decide	  "Who decides on husband's cash earnings for employment in the last 12 months among men currently in a union"
# we_earn_hs_decide	  "Who decides on husband's cash earnings for employment in the last 12 months among women currently in a union"
# we_own_house		    "Ownership of housing"
# we_own_land			    "Ownership of land"
# we_house_deed		    "Title or deed possession for owned house"
# we_land_deed		    "Title or deed possession for owned land"
# we_bank				      "Use an account in a bank or other financial institution"
# we_mobile			      "Own a mobile phone"
# we_mobile_finance	  "Use mobile phone for financial transactions"
# ----------------------------------------------------------------------------*/

# indicators from IR file
IRdata <- IRdata %>%
  mutate(wt = v005/1000000)

# *** Employment and earnings ***
# 
# //Employment in the last 12 months
IRdata <- IRdata %>%
  mutate(we_empl =
           case_when(
             v502==1 & v731 == 0   ~ 0 ,
             v502==1 & v731>0 & v731<8 ~ 1,
             v502==1 & v731 >=8 ~ 99)) %>%
  replace_with_na(replace = list(we_empl = c(99))) %>%
  set_value_labels(we_empl = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_empl = "Employment status in the last 12 months among those currently in a union")



colnames(IRdata)[colnames(IRdata) == 'we_empl'] <- 'value'
return(IRdata)
}
