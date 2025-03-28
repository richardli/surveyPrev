##'RH_DELP_C_DHF
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
RH_DELP_C_DHF<- function(Rdata){

  #Manually added function by Qianyu
  #Validated by Zambia 2018 admin 1 level(Five years preceding the survey)


  # /*****************************************************************************************************
  # Program: 			RH_DEL.R
  # Purpose: 			Code Delivery Care indicators
  # Data inputs: 	BR dataset
  # Data outputs:	coded variables
  # Author:			  Shireen Assaf
  # Date last modified: Sept 10, 2021 by Shireen Assaf
  # *****************************************************************************************************/
  #
  # /*----------------------------------------------------------------------------//
  # Variables created in this file:
  # rh_del_place		"Live births by place of delivery"
  # rh_del_pltype		"Live births by type of place"
  # rh_del_pv			  "Person providing assistance during birth"
  # rh_del_pvskill	"Skilled provider providing assistance during birth"
  # rh_del_ces			"Live births delivered by cesarean"
  # rh_del_cestime	"Timing of decision to have Cesarean"
  # rh_del_stay			"Duration of stay following recent birth"
  # /----------------------------------------------------------------------------*/
  #
  BRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # period and age of child
  # choose reference period, last 2 years (24 months) or last 5 years (60 months)
  # Using a period of the last 2 years will not match final report but would provide more recent information.
  BRdata <- BRdata %>%
    mutate(period = 60)

  # age of child. If b19 is not available in the data use v008 - b3
  if ("TRUE" %in% (!("b19" %in% names(BRdata))))
    BRdata [[paste("b19")]] <- NA
  if ("TRUE" %in% all(is.na(BRdata$b19)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    BRdata <- BRdata %>%
      mutate(age = b19)
  } else {
    BRdata <- BRdata %>%
      mutate(age = v008 - b3)
  }


  # //Place of delivery
  # Note: please check the categories of m15 especially for older surveys. The category values may differ.
  BRdata <- BRdata %>%
    mutate(rh_del_place =
             case_when(
               m15 >=20 & m15<40   ~ 1 ,
               m15 >=10 & m15<20   ~ 2,
               m15 >=40 & m15<99   ~ 3 ,
               m15 == 99 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_place = c(99))) %>%
    set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
    set_variable_labels(rh_del_place = "Live births by place of delivery")


  BRdata <- BRdata %>%
    mutate(RH_DELP_C_DHF =
             case_when(
               rh_del_place==1 ~ 1,
               rh_del_place %in% c(2,3,9)   ~ 0 )) %>%
    set_value_labels(RH_DELP_C_DHF = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(RH_DELP_C_DHF = "Live births by place of delivery: Health facility")

  colnames(BRdata)[colnames(BRdata) == 'RH_DELP_C_DHF'] <- 'value'
  return(BRdata)
}
