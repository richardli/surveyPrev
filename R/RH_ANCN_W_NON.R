#'RH_ANCN_W_N4P
#'IRdata
#'Antenatal visits for pregnancy: no visits TWO year
#'
#'
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "RH_ANCN_W_NON",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::RH_ANCN_W_NON)
#' }
#' @export
#'
RH_ANCN_W_NON<- function(Rdata){
  IRdata <- Rdata%>%
    mutate(wt = v005/1000000) %>%
    mutate(period = 24)#5 year 60(using RH_ANCN_W_N4F), 2 year 24, 3 year 36


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

  # ** NEW STEP: Explicitly filter the data to keep only recent births **
  # This directly changes the number of rows (the sample size).
  IRdata <- IRdata %>%
    filter(age < period)

  # //Number of ANC visits in 4 categories that match the table in the final report
  IRdata <- IRdata %>%
    mutate(rh_anc_numvs =
             case_when(
               m14_1 == 0 ~ 0 ,
               m14_1 == 1 ~ 1 ,
               m14_1  %in% c(2,3)   ~ 2 ,
               m14_1>=4 & m14_1<=90  ~ 3 ,
               m14_1>90  ~ 9
               # ,
               # age>=period ~ 99
             )) %>%
    replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
    set_value_labels(rh_anc_numvs = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) %>%
    set_variable_labels(rh_anc_numvs = "Number of ANC visits")
  IRdata <- IRdata %>%
    mutate(RH_ANCN_W_NON =
             case_when(
               rh_anc_numvs==0 ~ 1,
               rh_anc_numvs %in% c( 3,1,2,9)   ~ 0 )) %>%
    set_value_labels(RH_ANCN_W_NON = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(RH_ANCN_W_NON = "Attended 0 ANC visits")

  colnames(IRdata)[colnames(IRdata) == 'RH_ANCN_W_NON'] <- "value"
  return(IRdata)
}
