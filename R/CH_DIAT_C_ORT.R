##'CH_DIAT_C_ORT
#' @param KRdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
CH_DIAT_C_ORT <- function(KRdata){
# /*****************************************************************************************************
# Program: 			  CH_DIAR.R
# Purpose: 			  Code diarrhea variables.
# Data inputs: 		KR dataset
# Data outputs:		coded variables
# Author:				  Shireen Assaf
# Date last modified: Aug 2 2022 by Shireen Assaf 
# Notes:				      Check notes for diarrhea care and treatment variables which are country specific.
# *****************************************************************************************************/
# 
# /*----------------------------------------------------------------------------
# Variables created in this file:
# ch_diar				    "Diarrhea in the 2 weeks before the survey"
# ch_diar_care		  "Advice or treatment sought for diarrhea"
# 
# ch_diar_liq			  "Amount of liquids given for child with diarrhea"
# ch_diar_food		  "Amount of food given for child with diarrhea"
# 
# ch_diar_ors			  "Given oral rehydration salts for diarrhea"
# ch_diar_rhf			  "Given recommended homemade fluids for diarrhea"
# ch_diar_ors_rhf		"Given either ORS or RHF for diarrhea"
# ch_diar_zinc		  "Given zinc for diarrhea"
# ch_diar_zinc_ors	"Given zinc and ORS for diarrhea"
# ch_diar_ors_fluid	"Given ORS or increased fluids for diarrhea"
# ch_diar_ort			  "Given oral rehydration treatment and increased liquids for diarrhea"
# ch_diar_ort_feed	"Given ORT and continued feeding for diarrhea"
# ch_diar_antib		  "Given antibiotic drugs for diarrhea"
# ch_diar_antim		  "Given antimotility drugs for diarrhea"
# ch_diar_intra		  "Given Intravenous solution for diarrhea"
# ch_diar_other		  "Given home remedy or other treatment  for diarrhea"
# ch_diar_notrt		  "No treatment for diarrhea"
# 
# ch_diar_govh 		    "Diarrhea treatment sought from government hospital among children with diarrhea"
# ch_diar_govh_trt 	  "Diarrhea treatment sought from government hospital among children with diarrhea that sought treatment"
# ch_diar_govh_ors 	  "Diarrhea treatment sought from government hospital among children with diarrhea that received ORS"
# ch_diar_govcent 	  "Diarrhea treatment sought from government health center among children with diarrhea"
# ch_diar_govcent_trt "Diarrhea treatment sought from government health center among children with diarrhea that sought treatment"
# ch_diar_govcent_ors "Diarrhea treatment sought from government health center among children with diarrhea that received ORS"
# ch_diar_pclinc 		  "Diarrhea treatment sought from private hospital/clinic among children with diarrhea"
# ch_diar_pclinc_trt 	"Diarrhea treatment sought from private hospital/clinic among children with diarrhea that sought treatment"
# ch_diar_pclinc_ors 	"Diarrhea treatment sought from private hospital/clinic among children with diarrhea that received ORS"
# ch_diar_pdoc 		    "Diarrhea treatment sought from private doctor among children with diarrhea"
# ch_diar_pdoc_trt 	  "Diarrhea treatment sought from private doctor among children with diarrhea that sought treatment"
# ch_diar_pdoc_ors 	  "Diarrhea treatment sought from private doctor among children with diarrhea that received ORS"
# ch_diar_pharm 		  "Diarrhea treatment sought from a pharmacy among children with diarrhea"
# ch_diar_pharm_trt 	"Diarrhea treatment sought from a pharmacy among children with diarrhea that sought treatment"
# ch_diar_pharm_ors 	"Diarrhea treatment sought from a pharmacy among children with diarrhea that received ORS"
# ----------------------------------------------------------------------------*/

# weight variable 
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# //Diarrhea symptoms
KRdata <- KRdata %>%
  mutate(ch_diar = 
           case_when(
             (h11==1 | h11==2) & b5==1 ~ 1,
             b5==1 ~ 0  )) %>%
  set_value_labels(ch_diar = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar = "Diarrhea in the 2 weeks before the survey")

# //Diarrhea treatment	
# This is country specific and the footnote for the final table needs to be checked to see what sources are included. 
# The code below only excludes traditional practitioner (usually h12t). 
# The variable for traditional healer may be different for different surveys (you can check this checking all the h12* variables). 
# Some surveys also exclude pharmacies, shop, or other sources.
# If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove 
# h12k from the code below.

KRdata <- KRdata %>%
  mutate(ch_diar_care =
           case_when(
             ch_diar==1 & 
               (h12a == 1 | h12b == 1 | h12c == 1 | h12d == 1 | h12e == 1 | h12f == 1 |
                h12g == 1 | h12h == 1 | h12i == 1 | h12j == 1 | h12k == 1 | h12l == 1 |
                h12m == 1 | h12n == 1 | h12o == 1 | h12p == 1 | h12q == 1 | h12r == 1 |
                h12s == 1 |             h12u == 1 | h12v == 1 | h12w == 1 | h12x == 1 )  ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_care = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_care = "Advice or treatment sought for diarrhea")

# //Liquid intake
KRdata <- KRdata %>%
  mutate(ch_diar_liq =
           case_when(
             ch_diar==1 & h38==5  ~ 1 ,
             ch_diar==1 & h38==4  ~ 2 ,
             ch_diar==1 & h38==3  ~ 3 ,
             ch_diar==1 & h38==2  ~ 4 ,
             ch_diar==1 & h38==0  ~ 5 ,
             ch_diar==1 & (h38==8 | h38==9) ~ 9)) %>%
  set_value_labels(ch_diar_liq = c("More" = 1, "Same as usual"=2, "Somewhat less"=3, "Much less"=4, 
                                   "None"=5, "Don't know/missing"=9 )) %>%
  set_variable_labels(ch_diar_liq = "Amount of liquids given for child with diarrhea")

# //Food intake
KRdata <- KRdata %>%
  mutate(ch_diar_food =
           case_when(
             ch_diar==1 & h39==5  ~ 1 ,
             ch_diar==1 & h39==4  ~ 2 ,
             ch_diar==1 & h39==3  ~ 3 ,
             ch_diar==1 & h39==2  ~ 4 ,
             ch_diar==1 & h39==0  ~ 5 ,
             ch_diar==1 & h39==1  ~ 6 ,
             ch_diar==1 & (h39==8 | h39==9) ~ 9)) %>%
  set_value_labels(ch_diar_food = c("More" = 1, "Same as usual"=2, "Somewhat less"=3, "Much less"=4, 
                                    "None"=5, "Never gave food"=6, "Don't know/missing"=9 )) %>%
  set_variable_labels(ch_diar_food = "Amount of food given for child with diarrhea")


# //ORS
KRdata <- KRdata %>%
  mutate(ch_diar_ors =
           case_when(
             ch_diar==1 & (h13==1 | h13==2 | h13b==1)  ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ors = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ors = "Given oral rehydration salts for diarrhea")

# //RHF
KRdata <- KRdata %>%
  mutate(ch_diar_rhf =
           case_when(
             ch_diar==1 & (h14==1 | h14==2) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_rhf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_rhf = "Given recommended homemade fluids for diarrhea")

# //ORS or RHF
KRdata <- KRdata %>%
  mutate(ch_diar_ors_rhf =
           case_when(
             ch_diar==1 & (h13==1 | h13==2 | h13b==1 | h14==1 | h14==2) ~ 1 ,
             ch_diar==1 ~ 0)) %>%
  set_value_labels(ch_diar_ors_rhf = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_diar_ors_rhf = "Given either ORS or RHF for diarrhea")



colnames(KRdata)[colnames(KRdata) == 'ch_diar_ors_rhf'] <- 'value'
return(KRdata)
}
