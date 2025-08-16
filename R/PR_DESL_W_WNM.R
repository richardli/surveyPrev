##'PR_DESL_W_WNM
#' @param IRdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "PR_DESL_W_WNM", year = 2018)
#' }
#'
#' @export
PR_DESL_W_WNM <- function(IRdata){
# ******************************************************************************
# Program: 		FF_PREF.R
# Purpose: 		Code to compute fertility preferences in women  
# Data inputs: 		IR or MR dataset
# Data outputs:		coded variables
# Author:		Mahmoud Elkasabi
# Date last modified: September 03 2021 by Mahmoud Elkasabi
# ******************************************************************************
#The five indicators below can be computed for women. 
#For men the indicator is computed for age 15-49 in line 56. This can be commented out if the indicators are required for all men.
# -----------------------------------------------------------------------------#
# # Variables created in this file:
# ff_want_type		"Type of desire for children"
# ff_want_nomore	"Want no more children"
# ff_ideal_num		"Ideal number of children"
# ff_ideal_mean_all	"Mean ideal number of children for all"
# ff_ideal_mean_mar	"Mean ideal number of children for married"
# -----------------------------------------------------------------------------#

# indicators from IR file
IRdata <- IRdata %>%
  filter(v012 <= 49) %>%
  mutate(wt = v005/1000000) %>%
  mutate(ff_want_type = case_when( # //Desire for children
    v502 == 1 ~ v605,
    v502 != 1 ~ 99)) %>%
  replace_with_na(replace = list(ff_want_type = c(99))) %>%
  mutate(ff_want_type = set_label(ff_want_type, label = "Type of desire for children")) %>%
  mutate(ff_want_nomore = case_when( # //Want no more
    v502 == 1 & (v605==5 | v605==6) ~ 1, 
    v502 == 1 & (v605!=5 & v605!=6) ~ 0),
    ff_want_nomore = add_labels(ff_want_nomore, labels = c("No"=0, "Yes"=1)),
    ff_want_nomore = set_label(ff_want_nomore, label = "Want no more children")) 

colnames(IRdata)[colnames(IRdata) == 'ff_want_nomore'] <- 'value'
return(IRdata)
}
