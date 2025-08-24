#'FP_NADA_W_UNT
#'IRdata
#'Women with an unmet need for family planning for spacing and limiting
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
#'                                  indicator = "FP_NADA_W_UNT",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = NULL,
#'                          FUN = surveyPrev::FP_NADA_W_UNT)
#' }
#' @export
FP_NADA_W_UNT<- function(Rdata){
  IRdata <- Rdata %>%
    mutate(wt = v005/1000000)
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
