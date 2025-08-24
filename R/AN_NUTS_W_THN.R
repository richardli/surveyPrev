#'AN_NUTS_W_THN
#'IRdata
#'Women who are thin according to BMI (<18.5)
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
#'                                  indicator = "AN_NUTS_W_THN",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::AN_NUTS_W_THN)
#' }
#' @export
#'
AN_NUTS_W_THN<-function(Rdata){

  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)
  # * age of most recent child
  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata$b19_01)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }

  # //Thin
  IRdata <- IRdata %>%
    mutate(nt_wm_thin =
             case_when(
               v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
               v445>= 1850   ~ 0,
               v445>=1200 & v445<1850  ~ 1 )) %>%
    replace_with_na(replace = list(nt_wm_thin = c(99))) %>%
    set_value_labels(nt_wm_thin = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_wm_thin = "Thin BMI - women")

  colnames(IRdata)[colnames(IRdata) == 'nt_wm_thin'] <- "value"
  return(IRdata)
}
