##'RH_PCMT_W_NON  No Mother's postnatal checkup in 42 days
##' IR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "RH_PCMT_W_NON", year = 2018)
#' }
#'
#' @export
RH_PCMT_W_NON<- function(Rdata){


  IRdata <- Rdata%>%
    mutate(wt = v005/1000000)
  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $b19_01)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }


  # ** For surveys 2005 or after, postnatal care was asked for both institutional and non-institutional births.
  # ** surveys before 2005 only ask PNC for non-institutional births but assumed women received PNC if they delivered at health facilities
  # ** This is checked using variable m51_1 which was used in older surveys
  # ** If the code does not run, perhaps it is because you need to use m51a_1. Uncomment the next line in that case.
  # 	*cap gen m51_1=m51a_1


  # ** To check if survey has m51_1, which was in the surveys before 2005.
  if ("TRUE" %in% (!("m51_1" %in% names(IRdata))))
    IRdata [[paste("m51_1")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $m51_1)))
  { m51_included <- 0} else { m51_included <- 1}

  ### *** Mother's PNC *** ###

  if (m51_included==1) {
    # //PNC timing for mother
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_timing =
               case_when(
                 (m51_1 >=242 & m51_1<=297) | (m51_1>=306 & m51_1<=397) | (m50_1==0 | m50_1==9) | (m52_1>29 & m52_1<97) | is.na(m52_1)  ~ 0 ,
                 m51_1  %in% c(100, 101, 102, 103)  ~ 1 ,
                 (m51_1 >=104 & m51_1<=123) | m51_1 == 200  ~ 2 ,
                 (m51_1 >=124 & m51_1<=171) | m51_1 %in% c(201,202)~ 3,
                 (m51_1 >=172 & m51_1<=197) | m51_1 %in% c(203,204,205,206) ~ 4,
                 (m51_1 >=207 & m51_1<=241) | (m51_1 >=301 & m51_1<=305)  ~ 5,
                 m51_1  %in% c(198, 199, 298, 299, 398, 399, 998, 999)  ~ 9))

    IRdata[["rh_pnc_wm_timing"]] <- ifelse(IRdata[["bidx_01"]]!=1 | IRdata[["age"]]>=24, NA, IRdata[["rh_pnc_wm_timing"]])

    IRdata <- IRdata %>%
      set_value_labels(rh_pnc_wm_timing = c("No PNC" = 0, "<4hr" = 1, "4-23hrs"=2, "1-2days"=3, "3-6days"=4, "7-41days"=5, "don't know/missing"=9)) %>%
      set_variable_labels(rh_pnc_wm_timing = "Timing after delivery for mother's PNC check")


    # # //PNC within 2days for mother
    # IRdata <- IRdata %>%
    #   mutate(rh_pnc_wm_2days =
    #            case_when(
    #              rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
    #              rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
    #              bidx_01!=1 | age>=24 ~ 99 )) %>%
    #   replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) %>%
    #   set_value_labels(rh_pnc_wm_2days = c("No Visit w/in 2 days" = 0, "visit w/in 2 days" = 1 )) %>%
    #   set_variable_labels(rh_pnc_wm_2days = "PNC check within two days for mother")
    #
    # //PNC provider for mother
    # This is country specific and could be different for different surveys, please check footnote of the table for this indicator in the final report.
    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_pv =
               case_when(
                 ((age<24 & rh_pnc_wm_2days==1) & m52_1==0) | (age<24 & rh_pnc_wm_2days==0)  ~ 0,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1 ==11 ~ 1,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1 %in% c(12,13) ~ 2,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1 %in% c(14,15) ~ 3,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1>=16 & m52_1<=90 ~ 4,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1==96 ~ 5 ,
                 (age<24 & rh_pnc_wm_2days==1) & m52_1>96 ~ 9)) %>%
      set_value_labels(rh_pnc_wm_pv = c("No check" = 0, "Doctor" = 1, "Nurse/Midwife"=2, "Other skilled provider"=3, "Non-skilled provider"=4, "Other"=5, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_pnc_wm_pv = "Provider for mother's PNC check")

  }

  if (m51_included==0) {
    # //PNC timing for mother
    # 	*did the mother have any check
    IRdata <- IRdata %>%
      mutate(momcheck =
               case_when(
                 (m62_1!=1 & m66_1!=1) & age<24 ~ 0,
                 (m62_1==1 | m66_1==1) & age<24 ~ 1,
                 age<24 ~ 0))

    IRdata <- IRdata %>%
      mutate(pnc_wm_time =
               case_when(
                 (m64_1 >= 11 & m64_1 <= 29) & age<24 ~ as.numeric(m63_1),
                 age<24 & momcheck == 1 ~ 999)) %>%
      mutate(pnc_wm_time1000 =
               case_when(
                 pnc_wm_time<1000 ~ 1)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time1000==1 & (m64_1 > 30 & m64_1 < 100) & age<24 ~ 0,
                 TRUE ~ pnc_wm_time )) %>%
      mutate(pnc_wm_time999 =
               case_when(
                 pnc_wm_time==999 ~ 1)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time999==1 & (m68_1 >= 11 & m68_1 <= 29) & age<24 ~ m67_1,
                 TRUE ~ pnc_wm_time ))  %>%
      mutate(pnc_wm_time0 =
               case_when(
                 m67_1 < 1000 & m68_1 > 30 & m68_1 < 100 & age<24 ~ 1,
                 TRUE ~ 0)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time0==1 ~ 0,
                 pnc_wm_time0==0 ~ as.numeric(pnc_wm_time))) %>%
      mutate(pnc_wm_time00 =
               case_when(
                 momcheck==0 & age<24 ~ 1)) %>%
      mutate(pnc_wm_time =
               case_when(
                 pnc_wm_time00==1 ~ 0,
                 TRUE ~ as.numeric(pnc_wm_time)))

    IRdata <- IRdata %>%
      mutate(rh_pnc_wm_timing =
               case_when(
                 (pnc_wm_time >=242 & pnc_wm_time<=299) | (pnc_wm_time>=306 & pnc_wm_time<900) | pnc_wm_time==0  ~ 0 ,
                 pnc_wm_time  %in% c(100, 101, 102, 103)  ~ 1 ,
                 (pnc_wm_time >=104 & pnc_wm_time<=123) | pnc_wm_time==200 ~ 2 ,
                 (pnc_wm_time >=124 & pnc_wm_time<=171) | pnc_wm_time %in% c(201,202)~ 3,
                 (pnc_wm_time >=172 & pnc_wm_time<=197) | pnc_wm_time %in% c(203,204,205,206) ~ 4,
                 (pnc_wm_time >=207 & pnc_wm_time<=241) | (pnc_wm_time >=301 & pnc_wm_time<=305)  ~ 5,
                 pnc_wm_time  %in% c(198, 199, 298, 299, 298, 399, 998, 999)  ~ 9 ,
                 bidx_01!=1 | age>=24 ~ 99 )) %>%
      replace_with_na(replace = list(rh_pnc_wm_timing = c(99))) %>%
      set_value_labels(rh_pnc_wm_timing = c("No check or past 41 days" = 0, "<4hr" = 1, "4-23hrs"=2, "1-2days"=3, "3-6days"=4, "7-41days"=5, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_pnc_wm_timing = "Timing after delivery for mother's PNC check")


    # # //PNC within 2days for mother
    # IRdata <- IRdata %>%
    #   mutate(rh_pnc_wm_2days =
    #            case_when(
    #              rh_pnc_wm_timing %in% c(1,2,3) ~ 1,
    #              rh_pnc_wm_timing %in% c(0,4,5,9) ~ 0,
    #              bidx_01!=1 | age>=24 ~ 99 )) %>%
    #   replace_with_na(replace = list(rh_pnc_wm_2days = c(99))) %>%
    #   set_value_labels(rh_pnc_wm_2days = c("Not in 2 days" = 0, "Within 2 days" = 1 )) %>%
    #   set_variable_labels(rh_pnc_wm_2days = "PNC check within two days for mother")
    #

  }



  # //PNC within 2days for mother
  IRdata <- IRdata %>%
    mutate(RH_PCMT_W_NON =
             case_when(
               rh_pnc_wm_timing %in% c( 0 ) ~ 1,
               rh_pnc_wm_timing %in% c(1,2,3,4,5,9) ~ 0,
               bidx_01!=1 | age>=24 ~ 99 )) %>%
    replace_with_na(replace = list(RH_PCMT_W_NON = c(99))) %>%
    set_value_labels(RH_PCMT_W_NON = c("No check or past 41 days" = 1, "check or past 41 days" = 0 )) %>%
    set_variable_labels(RH_PCMT_W_NON = "No check or past 41 days")



  colnames(IRdata)[colnames(IRdata) == 'RH_PCMT_W_NON'] <- 'value'
  return(IRdata)
}
