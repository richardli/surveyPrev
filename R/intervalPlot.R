#' Get scatter plot for any two model results
#'
#' This function return scatter plot at admin 1 level for any two model results
#'
#' @param res  model result  using surveyPrev
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level.
#' @import ggplot2
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' 
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' data(ZambiaPopWomen)
#' cluster.info <- clusterInfo(geo = geo, 
#'                             poly.adm1 = ZambiaAdm1, 
#'                             poly.adm2 = ZambiaAdm2)
#'
#' dhsData <-surveyPrev:: getDHSdata(country = "Zambia", 
#'                                  indicator = "ancvisit4+", 
#'                                  year = 2018)
#' 
#' data <- getDHSindicator(dhsData, indicator = indicator)
#' admin.info2 <- adminInfo(geo = ZambiaAdm2, 
#'                         admin = 2,
#'                         agg.pop =ZambiaPopWomen$admin2_pop,
#'                         proportion = ZambiaPopWomen$admin2_urban)
#' cl_res_ad2 <- clusterModel(data = data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   stratification = FALSE,
#'                   model = "bym2",
#'                   admin = 2, 
#'                   aggregation = TRUE,
#'                   CI = 0.95)
#'
#' head(cl_res_ad2$res.admin2)
#' head(cl_res_ad2$agg.admin1)
#' plots <- intervalPlot(cl_res_ad2)
#' plots[["Central"]]
#' 
#' library(patchwork)
#' wrap_plots(plots, ncol = 5)
#'
#' }
#'
#' @export

intervalPlot<-function(res){

  # TODO: refer by variable name and make it work with both admin 1 and 2 cases
  data<-res[[1]]
  linedata<-res[[2]]
  line2<-res[[3]]$value

  plot_fun <- function(dat) {
    line1 = linedata[unique(dat$admin1.name),"value"]

    ggplot(dat, aes(x = admin2.name, y = value)) +
        geom_point( position = position_dodge(width = 0.5),size = .8) +
        geom_errorbar(aes(ymin = lower , ymax = upper), alpha = 0.5,position = position_dodge(width = 0.5), width = 0.2) +
        scale_color_manual(values = c("red", "blue", "darkgreen"))+
        geom_hline(  aes(yintercept =line1,linetype = "dashed"),color="royalblue",size = .6) +
        geom_hline( aes(yintercept =line2, linetype = "solid"),color="royalblue",size = .6) + 
       # geom_ribbon(data = line2data, aes(x =xx, ymin = yymin, ymax = yymax), fill = "grey70") +
      labs(title = unique(dat$admin1.name)) +
      xlab("") + ylab("") + 
      scale_linetype_manual(values = c("dashed", "solid"),
                            labels = c("admin1","national")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  }

 plots <- NULL
 for (adm1 in unique(data$admin1.name)){
  g <- plot_fun(subset(data, admin1.name == adm1))
  plots[[adm1]] <- g
 }

  return(plots)

}

