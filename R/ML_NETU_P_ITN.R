#'ML_NETU_P_ITN
#'PRdata
#'Percentage of the de facto household population who slept under an insecticide treated net the night before the survey
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
#'
ML_NETU_P_ITN <- function(Rdata){
  PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)

  PRdata <- PRdata %>%
    mutate(ml_netcat = case_when(
      hml12==0  ~ 0,
      hml12==1|hml12==2  ~ 1,
      hml12==3 ~ 2),
      ml_netcat = set_label(ml_netcat, label = "Mosquito net categorization"))
  # Slept under an ITN last night
  PRdata <- PRdata %>%
    mutate(ML_NETU_P_ITN = case_when(
      ml_netcat==1  ~ 1,
      TRUE ~ 0),
      ML_NETU_P_ITN = set_label(ML_NETU_P_ITN, label = "Slept under an ITN last night"))

  colnames(PRdata)[colnames(PRdata) == 'ML_NETU_P_ITN'] <- "value"



  return(PRdata)
}
