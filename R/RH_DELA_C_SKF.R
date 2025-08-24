#'RH_DELA_C_SKF
#'BRdata
#'Assistance during delivery from a skilled provider
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
#'                                  indicator = "RH_DELA_C_SKF",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::RH_DELA_C_SKF)
#' }
#' @export
#'
RH_DELA_C_SKF <- function(Rdata){

  BRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # period and age of child
  # choose reference period, last 2 years (24 months) or last 5 years (60 months)
  # this is fiver year
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

  # ** NEW STEP: Explicitly filter the data to keep only recent births **
  # This directly changes the number of rows (the sample size).
  BRdata <- BRdata %>%
    filter(age < period)


  BRdata <- BRdata %>%
    mutate(rh_del_pv =
             case_when(
               m3a == 1   ~ 1 ,
               m3b == 1 ~ 2,
               m3c == 1 | m3d == 1 | m3e == 1 | m3f == 1~ 3 ,
               m3g == 1 ~ 4 ,
               m3h == 1 | m3i == 1 | m3j == 1 | m3k == 1 | m3l == 1 | m3m == 1 ~ 5 ,
               m3n ==1 ~ 6,
               m3a ==8 | m3a==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pv = c(99))) %>%
    set_value_labels(rh_del_pv = c("Doctor" = 1, "Nurse/midwife"=2, "Country specific health professional"=3, "Traditional birth attendant"=4, "Relative/other"=5, "No one"=6, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_pv = "Person providing assistance during delivery")

  # //Skilled provider during delivery
  # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
  BRdata <- BRdata %>%
    mutate(rh_del_pvskill =
             case_when(
               rh_del_pv %in% c(1,2)   ~ 1 ,
               rh_del_pv %in% c(3,4,5) ~ 2,
               rh_del_pv ==6 ~ 3 ,
               rh_del_pv==9 ~ 9 ,
               age>=period ~ 99)) %>%
    replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
    set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
    set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery")

  if (BRdata$v000[1] %in% c("NG7")) {
    # //Skilled provider during delivery -- ****SPECIFIC FOR NIGERIA*******
    # ** Note: Please check the final report for this indicator to determine what provider is considered skilled.
    BRdata <- BRdata %>%
      mutate(rh_del_pvskill =
               case_when(
                 #m3c==1 ~ 1,
                 rh_del_pv %in% c(1,2) | m3c==1   ~ 1 , # ADD AUXILIARY MIDWIFE HERE FOR NIGERIA (M3C==1)
                 rh_del_pv %in% c(3, 4,5) ~ 2,
                 rh_del_pv ==6 ~ 3 ,
                 rh_del_pv==9 ~ 9 ,
                 age>=period ~ 99)) %>%
      replace_with_na(replace = list(rh_del_pvskill = c(99))) %>%
      set_value_labels(rh_del_pvskill = c("Skilled provider" = 1, "Unskilled provider"=2, "No one"=3, "Don't know/missing"=9  )) %>%
      set_variable_labels(rh_del_pvskill = "Skilled assistance during delivery in Nigeria")
  }

  BRdata$rh_del_pvskill= ifelse( BRdata$rh_del_pvskill == 1, 1, 0)
  colnames(BRdata)[colnames(BRdata) == 'rh_del_pvskill'] <- "value"
  return(BRdata)
}
