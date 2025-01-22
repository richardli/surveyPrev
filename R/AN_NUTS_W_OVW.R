##' AN_NUTS_W_OVW Women who are overweight according to BMI (25.0-29.9)
##' nt_wm_ovwt in github
##' IR
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
AN_NUTS_W_OVW<- function(Rdata){
  IRdata <- Rdata
  # *** Anthropometry indicators ***

  # * age of most recent child
  # age of child. If b19_01 is not available in the data use v008 - b3_01
  if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
    IRdata [[paste("b19_01")]] <- NA
  if ("TRUE" %in% all(is.na(IRdata $b19_01)))
  { b19_included <- 0} else { b19_included <- 1}

  if (b19_included==1) {
    IRdata <- IRdata %>%
      mutate(age = b19_01)
  } else {
    IRdata <- IRdata %>%
      mutate(age = v008 - b3_01)
  }

  # //Overweight
  IRdata <- IRdata %>%
    mutate(AN_NUTS_W_OVW =
             case_when(
               v445<1200 | v445>6000 | v213==1 | age<2 ~ 99,
               v445< 2500  | v445>=3000 ~ 0,
               v445>=2500 & v445<3000  ~ 1 )) %>%
    replace_with_na(replace = list(AN_NUTS_W_OVW = c(99))) %>%
    set_value_labels(AN_NUTS_W_OVW = c("Yes" = 1, "No"=0  )) %>%
    set_variable_labels(AN_NUTS_W_OVW = "Overweight BMI - women")

  colnames(IRdata)[colnames(IRdata) == 'AN_NUTS_W_OVW'] <- 'value'

  return(IRdata)
}
