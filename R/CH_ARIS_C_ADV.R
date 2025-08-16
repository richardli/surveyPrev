##'CH_ARIS_C_ADV Children with ARI for whom advice or treatment was sought;
##' ch_ari_care in github
##' KR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' dhsData <- getDHSdata(country = "Zambia", indicator = "CH_ARIS_C_ADV", year = 2018)
#' }
#'
#' @export
CH_ARIS_C_ADV<- function(Rdata){



# weight variable
KRdata <- Rdata %>%
  mutate(wt = v005/1000000)

# ** ARI indicators ***
# age of child. If b19 is not available in the data use v008 - b3
if ("TRUE" %in% (!("b19" %in% names(KRdata))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  KRdata <- KRdata %>%
    mutate(age = b19)
} else {
  KRdata <- KRdata %>%
    mutate(age = v008 - b3)
}

# //ARI symptoms
# ARI definition differs by survey according to whether h31c is included or not
if ("TRUE" %in% (!("h31c" %in% names(KRdata))))
  KRdata [[paste("h31c")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$h31c)))
{ h31c_included <- 0} else { h31c_included <- 1}

if (h31c_included==1) {
  KRdata <- KRdata %>%
    mutate(ch_ari =
             case_when(
               h31b==1 & (h31c==1 | h31c==3) & b5==1 ~ 1,
               b5==1 ~ 0  )) %>%
    set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
} else {
  KRdata <- KRdata %>%
    mutate(ch_ari =
             case_when(
               h31b==1 & (h31==2) & b5==1 ~ 1 ,
               b5==1 ~ 0 )) %>%
    set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
}


# survey specific changes
# if survey is "IAKR23" or "PHKR31"
# KRdata <- KRdata %>%
#   mutate(ch_ari =
#            case_when(
#              h31b==1 & (h31==2|h31==1) ~ 1 ,
#              b5==1 ~ 0 )) %>%
#   set_value_labels(ch_ari = c("Yes" = 1, "No"=0)) %>%
#   set_variable_labels(ch_ari = "ARI symptoms in the 2 weeks before the survey")
#

# //ARI care-seeking
# This is country specific and the footnote for the final table needs to be checked to see what sources are included.
# The code below only excludes traditional practitioner (usually h32t).
# The variable for traditional healer may be different for different surveys (you can check this by checking all the h32* variables).
# Some surveys also exclude pharmacies, shop, or other sources.
# If you want to also remove pharmacy for example as a source of treatment (country specific condition) you can remove
# h32k from the code below.

KRdata <- KRdata %>%
  mutate(CH_ARIS_C_ADV =
           case_when(
             (ch_ari==1 &  b5==1) &
               (h32a == 1 | h32b == 1 | h32c == 1 | h32d == 1 | h32e == 1 | h32f == 1 |
                  h32g == 1 | h32h == 1 | h32i == 1 | h32j == 1 | h32k == 1 | h32l == 1 |
                  h32m == 1 | h32n == 1 | h32o == 1 | h32p == 1 | h32q == 1 | h32r == 1 |
                  h32s == 1 |             h32u == 1 | h32v == 1 | h32w == 1 | h32x == 1 )  ~ 1 ,
             b5==1 & ch_ari==1 ~ 0)) %>%
  set_value_labels(CH_ARIS_C_ADV = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(CH_ARIS_C_ADV = "Advice or treatment sought for ARI symptoms")

colnames(KRdata)[colnames(KRdata) == 'CH_ARIS_C_ADV'] <- 'value'


return(KRdata)
}
