##'RH_DELP_C_HOT
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "RH_DELP_C_DHT", year = 2018)
#' }
#'
#' @export
RH_DELP_C_HOT<- function(Rdata){

  #Manually added function by Qianyu
  #RH_DELP_C_HOT is made up of the two year version for RH_DELP_C_HOM(five year version/ official id name)

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
    mutate(period = 24)

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

  # ** NEW STEP: Explicitly filter the data to keep only recent births **
  # This directly changes the number of rows (the sample size).
  BRdata <- BRdata %>%
    filter(age < period)
  # Labels:
  #   value                                             label
  # 10                                              home
  # 11                                 respondent's home
  #   12                                        other home
  #   13                                      tba premises
  #   14                        on the way to the hospital
  #   20                                     public sector
  #   21        public national/zonal/specialised hospital
  #   22                 public regional referral hospital
  #   23                          public regional hospital
  #   24                          public district hospital
  #   25                              public health centre
  #   26                                 public dispensary
  #   27                                     public clinic
  #   30                            private medical sector
  #   31                      private specialised hospital
  #   32                            private other hospital
  #   33                             private health centre
  #   34                                private dispensary
  #   35                                    private clinic
  #   36                      other private medical sector
  #   40                         ---religious/voluntary---
  #   41 religious/voluntary referral/specialised hospital
  #   42             religious/voluntary district hospital
  #   43                religious/voluntary other hospital
  #   44                 religious/voluntary health centre
  #   45                    religious/voluntary dispensary
  #   46                        religious/voluntary clinic
  #   47                  other religious/voluntary sector
  #   96                                             other





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


  if( BRdata$v000[1] %in% c("TZ8")){ #Includes woman’s home, another home, and premises of a traditional birth attendant //exact the same for   m15 >=10 and   m15 >=11
    BRdata <- BRdata %>%
      mutate(rh_del_place =
               case_when(
                 m15 >=20 & m15<40   ~ 1 ,
                 m15 >=10 & m15<14   ~ 2,
                 m15 >=50 & m15<99   ~ 3 ,
                 m15 == 99 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_place = c(99))) %>%
      set_value_labels(rh_del_place = c("Health facility" = 1, "Home"=2, "Other"=3, "Missing"=9  )) %>%
      set_variable_labels(rh_del_place = "Live births by place of delivery")


  }




  BRdata <- BRdata %>%
    mutate(RH_DELP_C_HOT =
             case_when(
               rh_del_place==2 ~ 1,
               rh_del_place %in% c(1,3,9)   ~ 0 )) %>%
    set_value_labels(RH_DELP_C_HOT = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(RH_DELP_C_HOT = "Live births by place of delivery: Home")

  colnames(BRdata)[colnames(BRdata) == 'RH_DELP_C_HOT'] <- 'value'
  return(BRdata)
}
