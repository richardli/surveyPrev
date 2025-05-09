#'FP_NADM_W_UNT
#'#unmet_family
#'IRdata
#'Married women with an unmet need for family planning for spacing and limiting, line 17 manually added by Qianyu
#'
#' @param Rdata  data.frame from survryPrev::getDHSdata
#'
#' @return A partially processed data.frame that will be used in  survryPrev::getDHSindicator. The whole function can be used as a parameter in survryPrev::getDHSindicator
#'
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#' @export
#'
FP_NADM_W_UNT<- function(Rdata){
  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)
  IRdata=IRdata[IRdata$v501 %in% c(1,2),]

  IRdata <- IRdata %>%
    mutate(
      # fp_unmet_space  = ifelse(v626a==1, 1, 0),
      # fp_unmet_limit  = ifelse(v626a==2, 1, 0),
      fp_unmet_tot    = ifelse(v626a==1|v626a==2, 1, 0),
      # fp_met_space    = ifelse(v626a==3, 1, 0),
      # fp_met_limit    = ifelse(v626a==4, 1, 0),
      # fp_met_tot      = ifelse(v626a==3|v626a==4, 1, 0),
      # fp_demand_space = ifelse(v626a==1|v626a==3, 1, 0),
      # fp_demand_limit = ifelse(v626a==2|v626a==4, 1, 0),
      # fp_demand_tot   = ifelse(fp_unmet_tot|fp_met_tot, 1, 0),
      # fp_demsat_mod   = ifelse(fp_demand_tot, ifelse(fp_met_tot & v313==3, 1, 0), NA),
      # fp_demsat_any   = ifelse(fp_demand_tot, ifelse(fp_met_tot, 1, 0), NA),
    )

  colnames(IRdata)[colnames(IRdata) == 'fp_unmet_tot'] <- "value"
  return(IRdata)
}
