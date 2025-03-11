#'FP_CUSM_W_MOD
#'IRdata
#'Modern contraceptive prevalence rate (Married women currently using any modern method of contraception)
#'
#'
#'
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "FP_CUSA_W_MOD",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::fp_cruse_mod)
#' }
#' @export
#'
FP_CUSM_W_MOD <- function(Rdata){
  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)
  IRdata=IRdata[IRdata$v501 %in% c(1,2),]

  IRdata <- IRdata %>%
    mutate(fp_cruse_mod =
             ifelse(v313 == 3, 1, 0)) %>%
    set_value_labels(fp_cruse_mod = c(yes = 1, no = 0)) %>%
    set_variable_labels(fp_cruse_mod ="Currently used any modern method")
  colnames(IRdata)[colnames(IRdata) == 'fp_cruse_mod'] <- "value"
  return(IRdata)
}
