#'ML_PMAL_C_RDT
#'PRdata
#'Malaria prevalence according to RDT
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
#'                                  indicator = "ML_PMAL_C_RDT",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::ML_PMAL_C_RDT)
#' }
#' @export
#'
ML_PMAL_C_RDT<-function(Rdata){
  # Number of ITNs per household
  # Tested for Parasitemia via RDT
  PRdata <- Rdata %>%
    mutate(ml_test_rdtmal = case_when(
      hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & !(hml35==0 | hml35==1)  ~ 0,
      hv103==1 & hc1>=6 & hc1<=59 & hv042==1 & (hml35==0 | hml35==1)  ~ 1),
      ml_test_rdtmal = set_label(ml_test_rdtmal, label = "Tested for Parasitemia (via RDT) in children 6-59 months"))

  colnames(PRdata)[colnames(PRdata) == 'ml_test_rdtmal'] <- "value"
  return(PRdata)

}
