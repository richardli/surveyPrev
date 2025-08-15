##'DV_PCPV_W_CBF
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
DV_PCPV_W_CBF <- function(IRdata){
# /*****************************************************************************************************
# Program: 			DV_viol.R
# Purpose: 			Code domestic violence indicators from the IR file
# Data inputs: 	IR dataset
# Data outputs:	coded variables
# Author:				Courtney Allen and translated to R by Shireen Assaf 
# Date last modified: September 16, 2021 by Shireen Assaf
# *****************************************************************************************************/
# 
# ______________________________________________________________________________
# Variables created in this file:
# 
# //EXPERIENCE OF PHYSICAL, SEXUAL, EMOTIONAL VIOLENCE
# 	dv_phy				  "Experienced physical violence since age 15"
# 	dv_phy_12m		  "Experienced physical violence in the past 12 months"
# 	dv_phy_preg		  "Experienced physical violence during pregnancy"
# 	dv_sex				  "Ever experienced sexual violence"
# 	dv_sex_12m		  "Experienced sexual violence in the past 12 months"
# 	dv_sex_age		  "Specific age experienced sexual violence"
# 	dv_phy_only		  "Experienced physical violence only"
# 	dv_sex_only		  "Experienced sexual violence only"
# 	dv_phy_sex_all  "Experienced physical and sexual violence"
# 	dv_phy_sex_any	"Experienced physical or sexual violence"
# 	dv_viol_type		"Experienced physical only, sexual only, or both"
# 
# //PERSONS COMMITTING PHYSICAL OR SEXUAL VIOLENCE
# 	dv_phy_hus_curr		  "Person committing physical violence: current husband/partner"
# 	dv_phy_hus_form		  "Person committing physical violence: former husband/partner"
# 	dv_phy_bf_curr		  "Person committing physical violence: current boyfriend"
# 	dv_phy_bf_form		  "Person committing physical violence: former boyfriend"
# 	dv_phy_father		    "Person committing physical violence: father/step-father"
# 	dv_phy_mother		    "Person committing physical violence: mother/step-mother"
# 	dv_phy_sibling		  "Person committing physical violence: sister or bother"
# 	dv_phy_bychild		  "Person committing physical violence: daughter/son"
# 	dv_phy_other_rel	  "Person committing physical violence: other relative"
# 	dv_phy_mother_inlaw	"Person committing physical violence: mother-in-law"
# 	dv_phy_father_inlaw	"Person committing physical violence: father-in-law"
# 	dv_phy_other_inlaw	"Person committing physical violence: other-in-law"
# 	dv_phy_teacher		  "Person committing physical violence: teacher"
# 	dv_phy_atwork		    "Person committing physical violence: employer/someone at work"
# 	dv_phy_police		    "Person committing physical violence: police/soldier"
# 	dv_phy_other		    "Person committing physical violence: other"
# 	
# 	dv_sex_hus_curr		  "Person committing sexual violence: current husband/partner"
# 	dv_sex_hus_form		  "Person committing sexual violence: former husband/partner"
# 	dv_sex_bf			      "Person committing sexual violence: current/former boyfriend"
# 	dv_sex_father		    "Person committing sexual violence: father/step-father"
# 	dv_sex_brother		  "Person committing sexual violence: brother/step-brother"
# 	dv_sex_other_rel	  "Person committing sexual violence: other relative"
# 	dv_sex_inlaw		    "Person committing sexual violence: in-law"
# 	dv_sex_friend		    "Person committing sexual violence: friend/acquaintance"
# 	dv_sex_friend_fam	  "Person committing sexual violence: family friend"
# 	dv_sex_teacher		  "Person committing sexual violence: teacher"
# 	dv_sex_atwork		    "Person committing sexual violence: employer/someone at work"
# 	dv_sex_relig		    "Person committing sexual violence: priest or religious leader"
# 	dv_sex_police		    "Person committing sexual violence: police/soldier"
# 	dv_sex_stranger		  "Person committing sexual violence: stranger"
# 	dv_sex_other		    "Person committing sexual violence: other"
# 	dv_sex_missing		  "Person committing sexual violence: missing"			
# 	
# //SEEKING HELP AFTER VIOLENCE	
# 	dv_help_seek		    "Help-seeking behavior of women who ever experienced physical or sexual violence"
# 	dv_help_phy			    "Sources of help sought for physical violence among women who sought help"
# 	dv_help_sex			    "Sources of help sought for sexual violence among women who sought help"
# 	dv_help_phy_sex_all	"Sources of help sought for physical and sexual violence among women who sought help"
# 	dv_help_phy_sex_any	"Sources of help sought for physical or sexual violence among women who sought help"
# 	dv_help_fam 		    "Source of help: own family"
# 	dv_help_hfam 		    "Source of help: husband's family"
# 	dv_help_husb        "Source of help: husband"
# 	dv_help_bf 			    "Source of help: boyfriend"
# 	dv_help_friend 	    "Source of help: friend"
# 	dv_help_neighbor    "Source of help: neighbor"
# 	dv_help_relig 	    "Source of help: religious"
# 	dv_help_doc         "Source of help: doctor"
# 	dv_help_police      "Source of help: police"
# 	dv_help_lawyer      "Source of help: lawyer"
# 	dv_help_sw          "Source of help: social worker"
# 	dv_help_other       "Source of help: other"
#  ______________________________________________________________________________


# EXPERIENCED PHYSICAL VIOLENCE

# //Ever
IRdata <- IRdata %>%
  mutate(dv_phy =
           case_when(
             d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0 ~ 1, # violence by current partner
             d130a>0  ~ 1,  # violence by former partner
             d115y==0 ~ 1, # violence by anyone other than partner
             d118y==0 ~ 1, # violence during pregnancy 
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy = "Experienced physical violence since age 15")

# //In the last 12 months
IRdata <- IRdata %>%
  mutate(dv_phy_12m =
           case_when(
             d105a %in% c(1,2) | d105b %in% c(1,2) | d105c %in% c(1,2) | d105d%in% c(1,2) | 
             d105e %in% c(1,2) | d105f %in% c(1,2) | d105g %in% c(1,2) |d105j %in% c(1,2) ~ 1, 
             d117a==1 | d117a==2 | d130a==1 ~ 1,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_12m = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_12m = "Experienced physical violence in the past 12 mos")

# //In the last 12 months by frequency (often or sometimes)
IRdata <- IRdata %>%
  mutate(dv_phy_12m_f =
           case_when(
             d105a==1 | d105b==1 | d105c==1 | d105d==1 | d105e==1 | d105f==1 | d105g==1 |d105j==1 | d117a==1 ~ 1 ,
             d105a==2 | d105b==2 | d105c==2 | d105d==2 | d105e==2 | d105f==2 | d105g==2 |d105j==2 | d117a==2 ~ 2 ,
             v044==1  ~ 0 )) %>% 
  set_value_labels(dv_phy_12m_f = c("Sometimes"=2, "Often" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_12m_f = "Experienced physical violence in the past 12 mos, frequency")

#//Physical violence during pregnancy
IRdata <- IRdata %>%
  mutate(dv_phy_preg =
           case_when(
             d118y==0 ~ 1, 
             v044==1  & (v201>0 | v213==1 | v228==1) ~ 0 )) %>% 
  set_value_labels(dv_phy_preg = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_preg = "Experienced physical violence during pregnancy")

# **PERSONS COMMITTING PHYSICAL VIOLENCE ** #

# //Current partner
IRdata <- IRdata %>%
  mutate(dv_phy_hus_curr =
           case_when(
             v502==1 & (d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0 | d118a==1) ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_hus_curr = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_hus_curr = "Person committing physical violence: current husband/partner")
 
# //Former partner
IRdata <- IRdata %>%
  mutate(dv_phy_hus_form =
           case_when(
             v502>0 & (d115j==1 | d118j==1 | d130a>0) ~ 1, 
             v502==2 & (d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0) ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_hus_form = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_hus_form = "Person committing physical violence: former husband/partner")

# //Current boyfriend
IRdata <- IRdata %>%
  mutate(dv_phy_bf_curr =
           case_when(
             d115k==1 | d118k==1 ~ 1, 
             dv_phy==1 ~ 0 )) %>% 
  set_value_labels(dv_phy_bf_curr = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(dv_phy_bf_curr = "Person committing physical violence: current boyfriend")



colnames(IRdata)[colnames(IRdata) == 'dv_phy_bf_curr'] <- 'value'
return(IRdata)
}
