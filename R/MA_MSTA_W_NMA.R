##'MA_MSTA_W_NMA
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
MA_MSTA_W_NMA <- function(IRdata){
# ******************************************************************************
# Program: 			  MS_MAR.do
# Purpose: 			  Code to create marital indicators
# Data inputs: 		IR and MR survey list
# Data outputs:		coded variables and scalars
# Author:				  Courtney Allen for code share project
# Date last modified: August 2022 by Courtney Allen 
# ******************************************************************************


# Variables created in this file: ----------------------------------------------
#  
# ms_mar_stat		"Current marital status"
# ms_mar_union	"Currently in union"
# ms_mar_never	"Never in union"
# ms_afm_15		"First marriage by age 15"
# ms_afm_18		"First marriage by age 18"
# ms_afm_20		"First marriage by age 20"
# ms_afm_22		"First marriage by age 22"
# ms_afm_25		"First marriage by age 25"
#
# ONLY IN IR FILES:
# ms_cowives_num	"Number of co-wives"
# ms_cowives_any	"One or more co-wives"
#
#
# ONLY IN MR FILES:
# ms_wives_num	"Number of wives"
# 
# Datafiles created
#
# median_mar              datafile with median age at first marriage (MAFM) by 5-year age groups among women
# median_mar_subgroup     datafile with median age at first marriage (MAFM) by subgroup characteristics among women 20-49 and 25-49
# median_mar_men          datafile with median age at first marriage (MAFM) by 5-year age groups among men
# median_mar_subgroup_men datafile with median age at first marriage (MAFM) by subgroup characteristics among men 25-59 and 30-59 (age range may vary in survey)



# MEDIAN AGE FUNCTION ----------------------------------------------------------
calc_median_age <-function() {

  # create a age at first marriage (afm) dataframe with cumulative proportions by each age, use survey weights
  median_df <- data.frame(prop_cumulative = unclass(round(cumsum(prop.table(svytable(~temp_df$ms_age, design=dhssvy2))),4)))
  median_df$age <- as.numeric(row.names(median_df))
  
  # find age groups before and after the cumulative 50% 
  median_df <- median_df %>%
    mutate(age_before50 = case_when(prop_cumulative<0.5 & lead(prop_cumulative>0.5) ~ 1, TRUE ~ 0),
           age_after50 = case_when(prop_cumulative>=0.5 & lag(prop_cumulative<0.5) ~ 1, TRUE ~ 0))
  
  # use equation for interpolated median for completed time periods (see https://www.dhsprogram.com/data/Guide-to-DHS-Statistics.cfm)
  
  # age group before the cumulative 50%
  m1 <- median_df$age[median_df$age_before50==1]

  # cumulative proportion for the age group before the cumulative 50%
  p1 <- median_df$prop_cumulative[median_df$age_before50==1]
  
  # cumulative proportion for the age group after the cumulative 50%
  p2 <- median_df$prop_cumulative[median_df$age_after50==1]
  
 # calculate median age
  median_age <- round((m1 + ((0.5-p1)/(p2-p1)) + 1),1)
  
  # replace with NA if 50% of subgroup has not been married before start of subgroup
  median_age <- ifelse(median_age > beg_age, "NA", median_age)
  print(median_age)
}


		
# MARITAL STATUS (WOMEN)--------------------------------------------------------

# Create yes and no category labels
yesno = c("Yes" = 1, "No" = 0)

# Current marital status
IRdata <- IRdata %>%
  mutate(ms_mar_stat = v501) 
  
# Currently in union
IRdata <- IRdata %>%
  mutate(ms_mar_union = case_when(v502==1 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_mar_union = c("In union" = 1, "Not in union" = 0)) %>%
  set_variable_labels(ms_mar_union = "Currently in union")
           
# Never in union
IRdata <- IRdata %>%
  mutate(ms_mar_never = case_when(v501==0 ~ 1, TRUE ~ 0)) %>%
  set_value_labels(ms_mar_never = c("Not in union" = 1, "In union" = 0)) %>%
  set_variable_labels(ms_mar_never = "Never in union")





colnames(IRdata)[colnames(IRdata) == 'ms_mar_never'] <- 'value'
return(IRdata)
}
