##'CP_BREG_C_REG
#' @param PRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CP_BREG_C_REG", year = 2018)
#' }
#'
#' @export
CP_BREG_C_REG <- function(PRdata){
# /*****************************************************************************************************
# Program: 			PH_POP.R
# Purpose: 			Code to compute population characteristics, birth registration, education levels, household composition, orphanhood, and living arrangments
# Data inputs: 		PR dataset
# Data outputs:		coded variables
# Author:				Shireen Assaf for population indicators, and Tom Pullum and Mahmoud Elkasabi for living arrangements and orphanhood indicators
# Date last modified: January 31, 2022 by Mahmoud Elkasabi 
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# 
# ph_pop_age			  "De facto population by five-year age groups"
# ph_pop_depend		  "De facto population by dependency age groups"
# ph_pop_cld_adlt		"De facto population by child and adult populations"
# ph_pop_adols		  "De factor population that are adolesents"
# 	
# ph_birthreg_cert	  "Child under 5 with registered birth and birth certificate"
# ph_birthreg_nocert	"Child under 5 with registered birth and no birth certificate"
# ph_birthreg			    "Child under 5 with registered birth"
# 
# ph_highest_edu		  "Highest level of schooling attended or completed among those age 6 or over"
# ph_median_eduyrs_wm "Median years of education among those age 6 or over - Females"
# ph_median_eduyrs_mn "Median years of education among those age 6 or over - Males"
# 
# ph_wealth_quint		  "Wealth quintile - dejure population"
# 
# ph_chld_liv_arrang	"Living arrangement and parents survival status for child under 18"
# ph_chld_liv_noprnt	"Child under 18 not living with a biological parent"
# ph_chld_orph		    "Child under 18 with one or both parents dead"
# 
# ph_hhhead_sex		    "Sex of household head"
# ph_num_members		  "Number of usual household members"
# 	
# ph_orph_double		  "Double orphans under age 18"
# ph_orph_single		  "Single orphans under age 18"
# ph_foster			      "Foster children under age 18"
# ph_orph_foster		  "Orphans and/or foster children under age 18"
# ----------------------------------------------------------------------------*/


# *** Population characteristics ***

PRdata[["ager"]] <- ifelse(PRdata[["hv103"]]==1 , as.integer(PRdata[["hv105"]]/5), NA) 

# //Five year age groups
PRdata[["ph_pop_age"]] <- ifelse(PRdata[["ager"]]>=0 & PRdata[["ager"]]<=15, PRdata[["ager"]], 
                                 ifelse((PRdata[["ager"]]>=16 & PRdata[["ager"]]<=18) | (PRdata[["hv105"]]==95 & PRdata[["hv103"]]==1), 16, 
                                        ifelse((PRdata[["ager"]]>=19 & PRdata[["ager"]]<=20), 98, NA))) 

PRdata <- PRdata %>%
  set_value_labels(ph_pop_age = c("<5"=0, "5-9"=1, "10-14"=2, "15-19"=3, "20-24"=4, "25-29"=5, "30-34"=6,
                                  "35-39"=7, "40-44"=8, "45-49"=9, "50-54"=10, "55-59"=11, "60-64"=12, 
                                  "65-69"=13, "70-74"=14, "75-79"=15, "80+"=16, "Don't know/missing"=98)) %>%
  set_variable_labels(ph_pop_age = "De facto population by five-year age groups")

# //Dependency age groups
PRdata <- PRdata %>%
  mutate(ph_pop_depend =
           case_when(
             inrange(ager,0,2) ~ 1,
             inrange(ager,3,12)  ~ 2,
             inrange(ager,13,18)  ~ 3,
             inrange(ager,19,20) ~ 98)) %>%
  set_value_labels(ph_pop_depend = c("0-14"=1, "15-64"=2, "65+"=3, "Don't know/missing"=98)) %>%
  set_variable_labels(ph_pop_depend = "De facto population by dependency age groups")

# //Child and adult populations
PRdata <- PRdata %>%
  mutate(ph_pop_cld_adlt =
           case_when(
             hv103==1 & hv105<18  ~ 1,
             hv103==1 & hv105>=18 & hv105<97  ~ 2,
             hv103==1 & hv105>=98 ~ 98)) %>%
  set_value_labels(ph_pop_cld_adlt = c("0-17"=1, "18+"=2, "Don't know/missing"=98)) %>%
  set_variable_labels(ph_pop_cld_adlt = "De facto population by child and adult populations")

# //Adolescent population
PRdata <- PRdata %>%
  mutate(ph_pop_adols =
           case_when(
             hv103==1 & hv105>=10 & hv105<20  ~ 1,
             hv103==1 & (hv105<10 | hv105>=20) ~ 0)) %>%
  set_value_labels(ph_pop_adols = c("Adolescents 10-19"=1, "Not adolescents"=0)) %>%
  set_variable_labels(ph_pop_adols = "De facto population that are adolescents")

# *** Birth registration ***
# 
# //Child registered and with birth certificate
PRdata <- PRdata %>%
  mutate(ph_birthreg_cert =
           case_when(
             hv102==1 & hv105<5 & hv140==1 ~ 1,
             hv102==1 & hv105<5 & hv140!=1 ~ 0)) %>%
  set_value_labels(ph_birthreg_cert = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(ph_birthreg_cert = "Child under 5 with registered birth and birth certificate")

# //Child registered and with no birth certificate
PRdata <- PRdata %>%
  mutate(ph_birthreg_nocert =
           case_when(
             hv102==1 & hv105<5 & hv140==2 ~ 1,
             hv102==1 & hv105<5 & hv140!=2 ~ 0)) %>%
  set_value_labels(ph_birthreg_nocert = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(ph_birthreg_nocert = "Child under 5 with registered birth and no birth certificate")

# //Child is registered
PRdata <- PRdata %>%
  mutate(ph_birthreg =
           case_when(
             hv102==1 & hv105<5 & hv140 %in% c(1,2) ~ 1,
             hv102==1 & hv105<5 & hv140 %in% c(0,8,9) ~ 0)) %>%
  set_value_labels(ph_birthreg = c("Yes"=1, "No"=0)) %>%
  set_variable_labels(ph_birthreg = "Child under 5 with registered birth")



colnames(PRdata)[colnames(PRdata) == 'ph_birthreg'] <- 'value'
return(PRdata)
}
