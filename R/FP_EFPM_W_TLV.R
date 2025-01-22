##'FP_EFPM_W_TLV
#' @param IRdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
FP_EFPM_W_TLV <- function(IRdata){
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
  

## FAMILY PLANNING MESSAGES

# Family planning messages by radio 
IRdata <- IRdata %>%
  mutate(fp_message_radio = 
           ifelse(v384a == 1, 1, 0)) %>%
  set_value_labels(fp_message_radio = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_radio = "Exposure to family planning message by radio")


# Family planning messages by TV 
IRdata <- IRdata %>%
  mutate(fp_message_tv = 
           ifelse(v384b == 1, 1, 0)) %>%
  set_value_labels(fp_message_tv = c(yes = 1, no = 0)) %>%
  set_variable_labels(fp_message_tv = "Exposure to family planning message by TV")




colnames(IRdata)[colnames(IRdata) == 'fp_message_tv'] <- 'value'
return(IRdata)
}
