#' Get scatter plot for any two model results
#'
#' This function return scatter plot at admin 1 level for any two model results
#'
#' @param res1  model result 1 using surveyPrev
#' @param nameres1 name of model 1
#' @param res2  model result 2 using surveyPrev
#' @param nameres2 name of model 2
#'
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

scatterPlot<-function(scatter,res1,nameres1,res2,nameres2){

  if(scatter=="estimate"){
    if(nameres1=="dir.admin1"){
      res1<-res1[[1]]
      colnames(res1)[colnames(res1) == 'value'] <- 'dir.est'
    } else if(nameres1=="dir.admin2"){
      res1<-res1[[2]]
      colnames(res1)[colnames(res1) == 'weighted_avg'] <- 'agg.dir.est'
    } else if(nameres1=="smth.ad1"){
      res1<-res1[[1]]
      colnames(res1)[colnames(res1) == 'value'] <- 'smth.est'
    }

    if(nameres2=="dir.admin1"){
      res2<-res2[[1]]
      colnames(res2)[colnames(res2) == 'value'] <- 'dir.est'
    }else if(nameres2=="dir.admin2"){
      res2<-res2[[2]]
      colnames(res2)[colnames(res2) == 'weighted_avg'] <- 'agg.dir.est'
    }else if(nameres2=="smth.ad1"){
      res2<-res2[[1]]
      colnames(res2)[colnames(res2) == 'value'] <- 'smth.est'
    }
    xandy<-c(colnames(res1)[2], colnames(res2)[2])


  }else if (scatter=="se"){
    if(nameres1=="dir.admin1"){
      res1<-res1[[1]]
      res1$dir.est<-res1$se

    } else if(nameres1=="smth.ad1"){
      res1<-res1[[1]]
      res1$dir.est<-sqrt(res1$var)

    }
    # } else if(nameres1=="dir.admin2"){
    #   res1<-res1[[2]]
    #   colnames(res1)[colnames(res1) == 'weighted_avg'] <- 'agg.dir.est'

    if(nameres2=="dir.admin1"){
      res2<-res2[[1]]
      # colnames(res2)[colnames(res2) == 'var'] <- 'dir.est'
      res2$dir.est<-res2$se
      }else if(nameres2=="smth.ad1"){
      res2<-res2[[1]]
      # colnames(res2)[colnames(res2) == 'var'] <- 'smth.est'
      res2$smth.est<-sqrt(res2$var)}
    # }else if(nameres2=="dir.admin2"){
    #   res2<-res2[[2]]
    #   colnames(res2)[colnames(res2) == 'weighted_avg'] <- 'agg.dir.est'
    xandy<-c(colnames(res1)[length(colnames(res1))], colnames(res2)[length(colnames(res2))])

  }


  df<- merge(x = res1, y = res2, by="admin1.name" )
  # df
  lim <- range(c(df[,c(xandy[1])], df[,c(xandy[2])]), na.rm = TRUE)
  ggplot(df, aes(x=df[,c(xandy[1])],y=df[,c(xandy[2])])) +
    geom_point(alpha = 0.5,aes())+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
    labs(title = paste0(scatter, ":", xandy[1]," ", "versus", " " ,xandy[2]))+
    xlab(paste0(xandy[1]))+
    ylab(paste0(xandy[2]))+
  xlim(lim) + ylim(lim)

}

