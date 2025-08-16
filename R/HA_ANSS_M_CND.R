##'HA_ANSS_M_CND
#' @param MRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "HA_ANSS_M_CND", year = 2018)
#' }
#'
#' @export
HA_ANSS_M_CND <- function(MRdata){
# /*****************************************************************************************************
# Program: 			WE_EMPW.R
# Purpose: 			Code to compute decision making and justification of violence among in men and women
# Data inputs: 	IR or MR dataset
# Data outputs:	coded variables
# Author:				Shireen Assaf
# Date last modified: Nov 19, 2021 by Shireen Assaf 
# Note:				The indicators below can be computed for men and women. 
# 					  The indicators we_decide_all and we_decide_none have different variable labels for men compared to women. 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# we_decide_health			  "Decides on own health care"
# we_decide_hhpurch			  "Decides on large household purchases"
# we_decide_visits			  "Decides on visits to family or relatives"
# we_decide_health_self		"Decides on own health care either alone or jointly with partner"
# we_decide_hhpurch_self	"Decides on large household purchases either alone or jointly with partner"
# we_decide_visits_self		"Decides on visits to family or relatives either alone or jointly with partner"
# we_decide_all				    "Decides on all three: health, purchases, and visits  either alone or jointly with partner" (for women)
# 							          "Decides on both health and purchases either alone or jointly with partner" (for men)
# we_decide_none				  "Does not decide on any of the three decisions either alone or jointly with partner" (for women)
# 							          "Does not decide on health or purchases either alone or jointly with partner" (for men)
# 	
# we_dvjustify_burn			  "Agree that husband is justified in hitting or beating his wife if she burns food"
# we_dvjustify_argue			"Agree that husband is justified in hitting or beating his wife if she argues with him"
# we_dvjustify_goout			"Agree that husband is justified in hitting or beating his wife if she goes out without telling him"
# we_dvjustify_neglect		"Agree that husband is justified in hitting or beating his wife if she neglects the children"
# we_dvjustify_refusesex	"Agree that husband is justified in hitting or beating his wife if she refuses to have sexual intercourse with him"
# we_dvjustify_onereas		"Agree that husband is justified in hitting or beating his wife for at least one of the reasons"
# 	
# we_justify_refusesex		"Believe a woman is justified to refuse sex with her husband if she knows he's having sex with other women"
# we_justify_cond				  "Believe a women is justified in asking that her husband to use a condom if she knows that he has an STI"
# we_havesay_refusesex		"Can say no to their husband if they do not want to have sexual intercourse"
# we_havesay_condom			  "Can ask their husband to use a condom"
# 	
# we_num_decide				    "Number of decisions made either alone or jointly with husband among women currently in a union"
# we_num_justifydv		  	"Number of reasons for which wife beating is justified among women currently in a union"
# ----------------------------------------------------------------------------*/
# 

# * indicators from MR file

# 
# *** Decision making ***

# //Decides on own health
MRdata <- MRdata %>%
  mutate(we_decide_health =
           case_when(mv502==1  ~ mv743a )) %>%
  set_variable_labels(we_decide_health = "Decides on own health care")

# //Decides on household purchases
MRdata <- MRdata %>%
  mutate(we_decide_hhpurch =
           case_when(mv502==1  ~ mv743b )) %>%
  set_variable_labels(we_decide_hhpurch = "Decides on large household purchases")

# //Decides on visits
MRdata <- MRdata %>%
  mutate(we_decide_visits =
           case_when(mv502==1  ~ mv743d )) %>%
  set_variable_labels(we_decide_visits = "Decides on visits to family or relatives")

# //Decides on own health either alone or jointly
MRdata <- MRdata %>%
  mutate(we_decide_health_self =
           case_when(
             mv502==1 & mv743a <3  ~ 1 ,
             mv502==1 & mv743a>2 ~ 0)) %>%
  set_value_labels(we_decide_health_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_health_self = "Decides on own health care either alone or jointly with partner")

# //Decides on household purchases either alone or jointly
MRdata <- MRdata %>%
  mutate(we_decide_hhpurch_self =
           case_when(
             mv502==1 & mv743b <3  ~ 1 ,
             mv502==1 & mv743b>2 ~ 0)) %>%
  set_value_labels(we_decide_hhpurch_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_hhpurch_self = "Decides on large household purchases either alone or jointly with partner")

# //Decides on visits either alone or jointly
MRdata <- MRdata %>%
  mutate(we_decide_visits_self =
           case_when(
            mv502==1 & mv743d <3  ~ 1 ,
            mv502==1 & mv743d>2 ~ 0)) %>%
  set_value_labels(we_decide_visits_self = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_visits_self = "Decides on visits to family or relatives either alone or jointly with partner")

# //Decides on both health and purchases either alone or jointly with partner
MRdata <- MRdata %>%
  mutate(we_decide_all =
           case_when(
             mv502==1 & mv743a <3 & mv743b <3  ~ 1 ,
             mv502==1 & (mv743a >2 | mv743b >2) ~ 0)) %>%
  set_value_labels(we_decide_all = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_all = "Decides on all three: health, purchases, and visits  either alone or jointly with partner")

# //Does not decide on health or purchases either alone or jointly with partner
MRdata <- MRdata %>%
  mutate(we_decide_none =
           case_when(
             mv502==1 & mv743a <3 | mv743b <3  ~ 0 ,
             mv502==1 & (mv743a >2 & mv743b >2 ) ~ 1)) %>%
  set_value_labels(we_decide_none = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_decide_none = "Does not decide on any of the three decisions either alone or jointly with partner")


# *** Justification of violence ***

# //Justify violence - burned food
MRdata <- MRdata %>%
  mutate(we_dvjustify_burn =
           case_when(
             mv744e==1  ~ 1, 
             mv744e %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_burn = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_burn = "Agree that husband is justified in hitting or beating his wife if she burns food")

# //Justify violence - argues
MRdata <- MRdata %>%
  mutate(we_dvjustify_argue =
           case_when(
             mv744c==1  ~ 1, 
             mv744c %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_argue = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_argue = "Agree that husband is justified in hitting or beating his wife if she argues with him")

# //Justify violence - goes out without saying
MRdata <- MRdata %>%
  mutate(we_dvjustify_goout =
           case_when(
             mv744a==1  ~ 1, 
             mv744a %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_goout = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_goout = "Agree that husband is justified in hitting or beating his wife if she goes out without telling him")

# //Justify violence - neglects children 
MRdata <- MRdata %>%
  mutate(we_dvjustify_neglect =
           case_when(
             mv744b==1  ~ 1, 
             mv744b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_neglect = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_neglect = "Agree that husband is justified in hitting or beating his wife if she neglects the children")

# //Justify violence - no sex
MRdata <- MRdata %>%
  mutate(we_dvjustify_refusesex =
           case_when(
             mv744d==1  ~ 1, 
             mv744d %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_dvjustify_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_refusesex = "Agree that husband is justified in hitting or beating his wife if she refuses to have sexual intercourse with him")

# //Justify violence - at least one reason
MRdata <- MRdata %>%
  mutate(we_dvjustify_onereas =
           ifelse(mv744a==1 | mv744b==1 | mv744c==1 | mv744d==1 | mv744e==1 , 1,0)) %>%
  set_value_labels(we_dvjustify_onereas = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_dvjustify_onereas = "Agree that husband is justified in hitting or beating his wife for at least one of the reasons")

# //Justify to reuse sex - he's having sex with another woman
MRdata <- MRdata %>%
  mutate(we_justify_refusesex =
           case_when(
             mv633b==1  ~ 1, 
             mv633b %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_justify_refusesex = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_justify_refusesex = "Believe a woman is justified to refuse sex with her husband if she knows he's having sex with other women")

# //Justify to ask to use condom - he has STI
MRdata <- MRdata %>%
  mutate(we_justify_cond =
           case_when(
             mv822==1  ~ 1, 
             mv822 %in% c(0,8,9)  ~ 0)) %>%
  set_value_labels(we_justify_cond = c("Yes" = 1, "No"=0  )) %>%
  set_variable_labels(we_justify_cond = "Believe a women is justified in asking that her husband to use a condom if she knows that he has an STI")


colnames(MRdata)[colnames(MRdata) == 'we_justify_cond'] <- 'value'
return(MRdata)
}
