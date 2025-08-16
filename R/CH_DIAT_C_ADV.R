##'CH_DIAT_C_ADV Treatment of diarrhea: Advice or treatment was sought;
##' ch_diar_care in github
##' KR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CH_DIAT_C_ADV", year = 2018)
#' }
#'
#' @export
CH_DIAT_C_ADV<- function(Rdata){

  # weight variable
  KRdata <- Rdata %>%
    mutate(wt = v005/1000000)

  # //Diarrhea symptoms
  KRdata <- KRdata %>%
    mutate(ch_diar =
             case_when(
               (h11==1 | h11==2) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_diar = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_diar = "Diarrhea in the 2 weeks before the survey")

  # //Diarrhea treatment
  # This is country specific and the footnote for the final table needs to be checked to see what sources are included.
  # The code below only excludes traditional practitioner (usually h12t).
  # The variable for traditional healer may be different for different surveys (you can check this checking all the h12* variables).
  # Some surveys also exclude pharmacies, shop, or other sources.
  # If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove
  # h12k from the code below.

  KRdata <- KRdata %>%
    mutate(CH_DIAT_C_ADV =
             case_when(
               ch_diar==1 &
                 (h12a == 1 | h12b == 1 | h12c == 1 | h12d == 1 | h12e == 1 | h12f == 1 |
                    h12g == 1 | h12h == 1 | h12i == 1 | h12j == 1 | h12k == 1 | h12l == 1 |
                    h12m == 1 | h12n == 1 | h12o == 1 | h12p == 1 | h12q == 1 | h12r == 1 |
                    h12s == 1 |             h12u == 1 | h12v == 1 | h12w == 1 | h12x == 1 )  ~ 1 ,
               ch_diar==1 ~ 0)) %>%
    set_value_labels(CH_DIAT_C_ADV = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(CH_DIAT_C_ADV = "Advice or treatment sought for diarrhea")

   colnames(KRdata)[colnames(KRdata) == 'CH_DIAT_C_ADV'] <- 'value'


  return(KRdata)
}
