##'ED_MDIA_W_TLV
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "ED_MDIA_W_TLV", year = 2018)
#' }
#'
#' @export
ED_MDIA_W_TLV <- function(IRdata){
# ******************************************************************************
# Program: 			  RC_CHAR_WM.R
# Purpose: 		    Code to compute respondent characteristics of women  
# Data inputs: 		IR dataset
# Data outputs:		coded variables
# Author:				  Mahmoud Elkasabi
# Date last modified: April 01 2021 by Mahmoud Elkasabi
# ******************************************************************************
#The indicators are computed for age 15-49 in line 49. This can be commented out if the indicators are required for all women.
#Please check the note on health insurance. This can be country specific and also reported for specific populations. 
#Please check the variables available for smoking and tobacco and see notes for these variables. Variable names have changed and these indicators are country specific.
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# rc_edu				"Highest level of schooling attended or completed"
# rc_edu_median		"Median years of education"
# rc_litr_cats		"Level of literacy"
# rc_litr				"Literate - higher than secondary or can read part or whole sentence"
# rc_media_newsp		"Reads a newspaper at least once a week"
# rc_media_tv			"Watches television at least once a week"
# rc_media_radio		"Listens to radio at least once a week"
# rc_media_allthree	"Accesses to all three media at least once a week"
# rc_media_none		"Accesses none of the three media at least once a week"
# rc_intr_ever		"Ever used the internet"
# rc_intr_use12mo		"Used the internet in the past 12 months"
# rc_intr_usefreq		"Internet use frequency in the past month - among users in the past 12 months"
# rc_empl				"Employment status"
# rc_occup			"Occupation among those employed in the past 12 months"
# rc_empl_type		"Type of employer among those employed in the past 12 months"
# rc_empl_earn		"Type of earnings among those employed in the past 12 months"
# rc_empl_cont		"Continuity of employment among those employed in the past 12 months"
# rc_hins_ss			"Health insurance coverage - social security"
# rc_hins_empl		"Health insurance coverage - other employer-based insurance"
# rc_hins_comm		"Health insurance coverage - mutual health org. or community-based insurance"
# rc_hins_priv		"Health insurance coverage - privately purchased commercial insurance"
# rc_hins_other		"Health insurance coverage - other type of insurance"
# rc_hins_any			"Have any health insurance"
# rc_tobc_cig			"Smokes cigarettes"
# rc_tobc_other		"Smokes other type of tobacco"
# rc_tobc_snuffm		"Uses snuff smokeless tobacco by mouth"
# rc_tobc_snuffn		"Uses snuff smokeless tobacco by nose"
# rc_tobc_chew		"Chews tobacco"
# rc_tobv_betel		"Uses betel quid with tobacco"
# rc_tobc_osmkless	"Uses other type of smokeless tobacco"
# rc_tobc_anysmkless	"Uses any type of smokeless tobacco"
# rc_tobc_any			"Uses any type of tobacco - smoke or smokeless"
# -----------------------------------------------------------------------------#

IRdata <- IRdata %>%
  filter(v012 <= 49) %>%
  mutate(wt = v005/1000000) %>%
  mutate(rc_edu = v149,
         rc_edu = set_label(rc_edu, label = "Highest level of schooling attended or completed")) %>%
  mutate(eduyr = case_when(
    v133<=20 ~ v133, 
    v133>20 & v133<95 ~ 20, 
    v133>95 | v149>7 ~ 99)) %>%
  replace_with_na(replace = list(eduyr = c(99)))

IRdata <- IRdata %>%
  mutate(rc_litr_cats = case_when(
    !v106 == 3 & v155 == 2 ~ 1,
    !v106 == 3 & v155 == 1 ~ 2,
    !v106 == 3 & v155 == 0 ~ 3,
    !v106 == 3 & v155 == 3 ~ 4,
    !v106 == 3 & v155 == 4 ~ 5,
    v106 == 3 ~ 0),
  rc_litr_cats = add_labels(rc_litr_cats, labels = c("Higher than secondary education"=0, "Can read a whole sentence"=1,
                                      "Can read part of a sentence"=2, "Cannot read at all"=3,
                                      "No card with required language"=4, "Blind/visually impaired"=5)),
  rc_litr_cats = set_label(rc_litr_cats, label = "Level of literacy")) %>%
  mutate(rc_litr = case_when(
    v106==3 | v155==1 | v155==2 ~ 1, TRUE ~ 0),
    rc_litr = add_labels(rc_litr, labels = c("No"=0, "Yes"=1)),
    rc_litr = set_label(rc_litr, label = "Literate"))


# Media exposure

IRdata <- IRdata %>%
  mutate(rc_media_newsp = case_when(
    v157 == 2 | v157 == 3 ~ 1,
    v157 == 0 | v157 == 1 ~ 0),
  rc_media_newsp = add_labels(rc_media_newsp, labels = c("No"=0, "Yes"=1)),
  rc_media_newsp = set_label(rc_media_newsp, label = "Reads a newspaper at least once a week")) %>%
 
  mutate(rc_media_tv = case_when(
    v159 == 2 | v159 == 3 ~ 1,
    v159 == 0 | v159 == 1 ~ 0),
  rc_media_tv = add_labels(rc_media_tv, labels = c("No"=0, "Yes"=1)),
  rc_media_tv = set_label(rc_media_tv, label = "Watches television at least once a week")) 

colnames(IRdata)[colnames(IRdata) == 'rc_media_tv'] <- 'value'
return(IRdata)
}
