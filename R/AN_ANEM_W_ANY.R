#'AN_ANEM_W_ANY
#'IRdata
#'Percentage of women aged 15-49 classified as having any anemia
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
#'                                  indicator = "AN_ANEM_W_ANY",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::AN_ANEM_W_ANY)
#' }
#' @export
#'
AN_ANEM_W_ANY <- function(Rdata){

  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  IRdata <- IRdata %>%
    mutate(nt_wm_any_anem =
             case_when(
               v042==1 & v457<4 ~ 1 ,
               v042==1 &  v455==0 ~ 0)) %>%
    set_value_labels(nt_wm_any_anem = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(nt_wm_any_anem = "Any anemia - women")

  colnames(IRdata)[colnames(IRdata) == 'nt_wm_any_anem'] <- "value"
  return(IRdata)
}
