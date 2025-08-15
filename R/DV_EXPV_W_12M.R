##' DV_EXPV_W_12M  Percentage of women who have experienced physical violence in the past 12 months often or sometimes
##' dv_phy_12m in github
##' IR
#' @param Rdata  data.frame from surveyPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in surveyPrev::getDHSindicator. The whole function can be used as a parameter in surveyPrev::getDHSindicator
#'
#' @author Miaolei Bao, Yunhan Wu, Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
DV_EXPV_W_12M<- function(Rdata){


  Rdata$v005=Rdata$d005

  # EXPERIENCED PHYSICAL VIOLENCE

  # //Ever
  IRdata <- Rdata %>%
    mutate(dv_phy =
             case_when(
               d105a>0 | d105b>0 | d105c>0 | d105d>0 | d105e>0 | d105f>0 | d105g>0 |d105j>0 ~ 1, # violence by current partner
               d130a>0  ~ 1,  # violence by former partner
               d115y==0 ~ 1, # violence by anyone other than partner
               d118y==0 ~ 1, # violence during pregnancy
               v044==1  ~ 0 )) %>%
    set_value_labels(dv_phy = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(dv_phy = "Experienced physical violence since age 15")

  # //In the last 12 months
  IRdata <- IRdata %>%
    mutate(DV_EXPV_W_12M =
             case_when(
               d105a %in% c(1,2) | d105b %in% c(1,2) | d105c %in% c(1,2) | d105d%in% c(1,2) |
                 d105e %in% c(1,2) | d105f %in% c(1,2) | d105g %in% c(1,2) |d105j %in% c(1,2) ~ 1,
               d117a==1 | d117a==2 | d130a==1 ~ 1,
               v044==1  ~ 0 )) %>%
    set_value_labels(DV_EXPV_W_12M = c("Yes" = 1, "No"=0)) %>%
    set_variable_labels(DV_EXPV_W_12M = "Experienced physical violence in the past 12 mos")

  colnames(IRdata)[colnames(IRdata) == 'DV_EXPV_W_12M'] <- 'value'

  return(IRdata)
}
