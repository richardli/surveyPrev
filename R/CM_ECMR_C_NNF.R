##' CM_ECMR_C_NNF NMR five years prior to survey.
##'
##' BR
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
#'
CM_ECMR_C_NNF<- function(Rdata){
  nmr.year=5
  BRdata <- Rdata%>%
    mutate(wt = v005/1000000)%>%
    mutate(bo10 = Rdata$v008-12*nmr.year-b3)%>%
    filter(bo10<0)

  BRdata$value<- ifelse(BRdata$b7==0, 1, 0)
  BRdata$value[is.na( BRdata$value)] <- 0

  return(BRdata)
}

