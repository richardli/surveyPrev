#' Get scatter plot for any two model results
#'
#' This function return scatter plot at admin 1 level for any two model results
#'
#' @param res  model result  using surveyPrev
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level
#'   \item {plot
#' }
#' @import ggplot2
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export

intervalPlot<-function(res){

  data<-res[[1]]
  linedata<-res[[2]]
  line2<-res[[3]]$value
  plot_fun <- function(dat) {
    line1 = linedata[unique(dat$admin1.name),"value"]

    ggplot(dat, aes(x = admin2.name, y = value)) +
        geom_point( aes(type = type), position = position_dodge(width = 0.5)) +
        geom_errorbar(aes(ymin = quant025 , ymax = quant975,color = type), alpha = 0.5,position = position_dodge(width = 0.5)) +
        scale_color_manual(values = c("red", "blue", "darkgreen"))+
        geom_hline(  aes(yintercept =line1,linetype = "dashed"),color="grey30",size = 1) +
        geom_hline( aes(yintercept =line2, linetype = "solid"),color="grey30",size = 1) +
       # geom_ribbon(data = line2data, aes(x =xx, ymin = yymin, ymax = yymax), fill = "grey70") +
      labs(title = unique(dat$admin1.name))+
      scale_linetype_manual(values = c("dashed", "solid"),
                            labels = c("admin1","national")) +
      theme(axis.text.x = element_text(angle = 45))
  }

  plots <- lapply(unique(data$admin1.name), function(x) {
    data <- data %>% filter(admin1.name == x)
    plot_fun(data)
  })



  return(plots)

}

