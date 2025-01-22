##'WS_WTRT_P_SOL
#' @param WASHdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
WS_WTRT_P_SOL <- function(WASHdata){
# /*****************************************************************************
# Program: 			PH_WATER.do
# Purpose: 			creates variable for binary improved water source according to JSTOR standard 
# Data inputs: 		hr or pr file
# Data outputs:		none
# Author of do file:	04/08/2018	Courtney Allen
# Date last modified: 07/20/2023	Courtney Allen - for codeshare project
# Note:				These indicators can also be computed using the HR or PR file.
# 					If you want to produce estimates for households, use the HR file.
# 					If you want to produce estimates for the de jure population, 
# 					use the PR file and select for dejure household memebers using
# 					hv102==1. Please see the Guide to DHS Statistics.  
# 					
# 					*****
# *****************************************************************************
# 
# NOTES AND VARIABLE LIST ------------------------------------------------------
# This do file can be run on any loop of countries indicating the dataset name 
# with variable called filename. Code should be same for pr or hr files.
# 
# VARIABLES CREATED
# 	ph_wtr_trt_boil		"Treated water by boiling before drinking"
# 	ph_wtr_trt_chlor	"Treated water by adding bleach or chlorine before drinking"
# 	ph_wtr_trt_cloth	"Treated water by straining through cloth before drinking"
# 	ph_wtr_trt_filt		"Treated water by ceramic, sand, or other filter before drinking"
# 	ph_wtr_trt_solar	"Treated water by solar disinfection before drinking"
# 	ph_wtr_trt_stand	"Treated water by letting stand and settle before drinking"
# 	ph_wtr_trt_other	"Treated water by other means before drinking"
# 	ph_wtr_trt_none		"Did not treat water before drinking"
# 	ph_wtr_trt_appr		"Appropriately treated water before drinking"
# 	ph_wtr_source 		"Source of drinking water"
# 	ph_wtr_improve 		"Improved drinking water" 
# 	ph_wtr_time			"Round trip time to obtain drinking water"
# 	ph_wtr_basic		"Basic water service"
# 	ph_wtr_avail		"Availability of water among those using piped water or water from tube well or borehole"
#   interview season  "Interview in dry or rainy season"
# 
# NOTE: 
# STANDARD CATEGORIES FOR WATER SOURCE BY IMPROVED/UNIMPROVED
# 	0-unimproved 
# 		30	 well - protection unspecified	
# 		32	 unprotected well
# 		40	 spring - protection unspecified
# 		42	 unprotected spring
# 		43	 surface water (river/dam/lake/pond/stream/canal/irrigation channel)
# 		96	 other	
# 	1-improved
# 		11	 piped into dwelling
# 		12	 piped to yard/plot
# 		13	 public tap/standpipe
# 		14	 piped to neighbor
# 		15	 piped outside of yard/lot
# 		21	 tube well or borehole
# 		31	 protected well
# 		41	 protected spring
# 		51	 rainwater
# 		61	 tanker truck	
# 		62	 cart with small tank, cistern, drums/cans
# 		65	 purchased water	
# 		71	 bottled water
# 		72	 purified water, filtration plant
# 		73	 satchet water
# 	
# *****************************************************************************



# create water treatment indicators --------------------------------------------


# treated water by boiling
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_boil = case_when(
  hv237a>=8 ~ 0,
  TRUE ~ hv237a)) %>%
  set_value_labels(ph_wtr_trt_boil = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_boil =	"Treated water by boiling before drinking")

#	treated water by adding bleach or chlorine
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_chlor = case_when(
  hv237b>=8 ~ 0,
  TRUE ~ hv237b)) %>%
  set_value_labels(ph_wtr_trt_chlor = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_chlor = "Treated water by adding bleach or chlorine before drinking")

#	treated water by straining through cloth
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_cloth = case_when(
  hv237c>=8 ~ 0,
  TRUE ~ hv237c)) %>%
  set_value_labels(ph_wtr_trt_cloth = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_cloth = "Treated water by straining through cloth before drinking")

# treated water by ceramic, sand, or other filter
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_filt = case_when(
  hv237d>=8 ~ 0,
  TRUE ~ hv237d)) %>%
  set_value_labels(ph_wtr_trt_filt = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_filt = "Treated water by ceramic, sand, or other filter before) drinking")

# treated water by solar disinfection
WASHdata <- WASHdata %>% mutate(ph_wtr_trt_solar = case_when(
  hv237e>=8 ~ 0,
  TRUE ~ hv237e)) %>%
  set_value_labels(ph_wtr_trt_solar = c("yes" = 1, "no" = 0)) %>%
  set_variable_labels(ph_wtr_trt_solar ="Treated water by solar disinfection")



colnames(WASHdata)[colnames(WASHdata) == 'ph_wtr_trt_solar'] <- 'value'
return(WASHdata)
}
