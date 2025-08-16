##'FP_EFPM_M_NWS
#' @param MRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "FP_EFPM_M_NWS", year = 2018)
#' }
#'
#' @export
FP_EFPM_M_NWS <- function(MRdata){
# ******************************************************************************
# Program: 			  FP_COMM.R
# Purpose: 		    Code communication related indicators: exposure to FP messages,
#                 decision on use/nonuse, discussions.  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Courtney Allen
# Date last modified: March 29 2021 by Courtney Allen
# ******************************************************************************
#   

# -----------------------------------------------------------------------------#
# # Variables created in this file:
# fp_message_radio		"Exposure to family planning message by radio"
# fp_message_tv			  "Exposure to family planning message by TV"
# fp_message_paper		"Exposure to family planning message by newspaper/magazine"
# fp_message_mobile		"Exposure to family planning message by mobile phone"
# fp_message_noneof4	"Not exposed to any of the four media sources"
# fp_message_noneof3 	"Not exposed to TV, radio, or paper media sources"
# 
# fp_decyes_user			"Who makes the decision to use family planning among users"
# fp_decno_nonuser		"Who makes decision not to use family planning among non-users"
# 
# fp_fpvisit_discuss	"Women non-users that were visited by a FP worker who discussed FP"
# fp_hf_discuss			  "Women non-users who visited a health facility in last 12 months and discussed FP"
# fp_hf_notdiscuss		"Women non-users who visited a health facility in last 12 months and did not discuss FP"
# fp_any_notdiscuss		"Women non-users who did not discuss FP neither with FP worker or in a health facility"
# ------------------------------------------------------------------------------


## indicators from MR file


## FAMILY PLANNING MESSAGES



# Family planning messages by radio 
MRdata <- MRdata %>%
  mutate(fp_message_radio = 
           ifelse(mv384a == 1, 1, 0)) %>%
  set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")


# Family planning messages by TV 
MRdata <- MRdata %>%
  mutate(fp_message_tv = 
           ifelse(mv384b == 1, 1, 0)) %>%
  set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")


# Family planning messages by newspaper and/or magazine 
MRdata <- MRdata %>%
  mutate(fp_message_paper = 
           ifelse(mv384c == 1, 1, 0)) %>%
  set_value_labels(fp_message_paper = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_paper = "Exposure to family planning message by newspaper/magazine")




colnames(MRdata)[colnames(MRdata) == 'fp_message_paper'] <- 'value'
return(MRdata)
}
