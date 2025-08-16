##'HA_AFSY_W_A18
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "HA_AFSY_W_A18", year = 2018)
#' }
#'
#' @export
HA_AFSY_W_A18 <- function(IRdata){
# /*******************************************************************************************************************************
# Program: 				HK_BHV_YNG.R
# Data inputs: 		IR or MR datasets
# Data outputs:		coded variables for sexual behaviors among young people
# Author:				  Shireen Assaf for code share project
# Translated to R: Courtney Allen
# Date last modified: September 2022 by Courtney Allen 
# Note:				The indicators are computed for all men. No age selection is made here. 
# ******************************************************************************

# Variables created in this file -----------------------------------------------
# hk_sex_15			"Had sexual intercourse before age 15 among those age 15-24"
# hk_sex_18			"Had sexual intercourse before age 18 among those age 18-24"
# hk_nosex_youth		"Never had sexual intercourse among never-married age 15-24"
# hk_sex_youth_test	"Had sexual intercourse in the past 12 months and received HIV test and results among those age 15-24"


# SETUP ------------------------------------------------------------------------

# Create yes and no category labels
yesno <- c("Yes" = 1, "No" = 0)


# SEXUAL BEHAVIORS AMONG YOUNG PEOPLE (GIRLS AND WOMEN) ------------------------
# //Sex before 15
IRdata <- IRdata %>% mutate(hk_sex_15 = case_when(
  v531 %in% 1:14  & v012 <24 ~ 1,
  v012 < 24 ~ 0)) %>%
  set_value_labels(hk_sex_15 = yesno) %>%
  set_variable_labels(hk_sex_15 = "Had sexual intercourse before 15 among age 15-24")

# //Sex before 18
IRdata <- IRdata %>% mutate(hk_sex_18 = case_when(
  v531 %in% 1:17 & v012>=18 & v012<=24 ~ 1,
  v012>=18 & v012<=24 ~ 0)) %>%
  set_value_labels(hk_sex_18 = yesno) %>%
  set_variable_labels(hk_sex_18 = "Had sexual intercourse before 18 among age 18-24")



colnames(IRdata)[colnames(IRdata) == 'hk_sex_18'] <- 'value'
return(IRdata)
}
