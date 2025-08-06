#'ML_NETC_C_ITN
#'PRdata
#'Percentage of children under age five who slept under an insecticide treated net (ITN) the night before the survey
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
ML_NETC_C_ITN <- function(Rdata){
  PRdata <- Rdata %>%
    mutate(wt = hv005/1000000)

  # PRdata <- PRdata %>%
  #   mutate(nt_ch_any_anem =
  #            case_when(
  #              hv103==1 & hc1>5 & hc1<60 & hc56<110 ~ 1 ,
  #              hv103==1 & hc1>5 & hc1<60 & hc56>=110 ~ 0)) %>%
  #   set_value_labels(nt_ch_any_anem = c("Yes" = 1, "No"=0  )) %>%
  #   set_variable_labels(nt_ch_any_anem = "Any anemia - child 6-59 months")
  # colnames(PRdata)[colnames(PRdata) == 'nt_ch_any_anem'] <- "value"
  #

  PRdata <- PRdata %>%
    mutate(ml_netcat = case_when(
      hml12==0  ~ 0 ,
      hml12==1|hml12==2  ~ 1,
      hml12==3 ~ 2),
      ml_netcat = set_label(ml_netcat, label = "Mosquito net categorization"))
  # Slept under an ITN last night
  PRdata <- PRdata %>%
    mutate(ML_NETC_C_ITN = case_when(
      ml_netcat==1  ~ 1,
      TRUE ~ 0),
      ML_NETC_C_ITN = set_label(ML_NETC_C_ITN, label = "Slept under an ITN last night"))
  PRdata <- PRdata %>%
    subset(hc1<60)


  colnames(PRdata)[colnames(PRdata) == 'ML_NETC_C_ITN'] <- "value"


  # pr_data_renamed <- PRdata %>%
  #   rename(
  #     v001 = hv001,
  #     v002 = hv002,
  #     b16 = hvidx,
  #   ) %>%
  #   select(v001, v002, b16, value)
  #
  #
  # KRdata <- left_join(KRdata, pr_data_renamed, by = c("v001", "v002", "b16"))
  #
  # KRdata

  return(PRdata)
}
