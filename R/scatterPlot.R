#' Get scatter plot for any two model results
#'
#' This function return scatter plot at admin 1 level for any two model results
#' @param admin admin level of the plot
#' @param scatter "estimate" or "se"
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

scatterPlot<-function(admin,scatter,res1,nameres1,res2,nameres2){

if(admin==1){
  if(scatter=="estimate"){
    if(nameres1=="dir.admin1"){
      res1<-res1[[1]]
      name1<-'dir.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    } else if(nameres1=="dir.admin2"){
      res1<-res1[[2]]
      name1<- 'agg.dir.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    } else if(nameres1=="smth.ad1"){
      res1<-res1[[1]]
      name1<- 'smth.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    }else if(nameres1=="T1"){
      res1<-res1[[1]][21:30,]
      name1<- 'strata.est'
      colnames(res1)[colnames(res1) == 'value'] <-name1
    }else if(nameres1=="T2"){
      res1<-res1[[2]]
      name1<- 'agg.strata.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    }else if(nameres1=="F1"){
      res1<-res1[[1]]
      name1<- 'cluster.est'
      colnames(res1)[colnames(res1) == 'value'] <-name1
    }else if(nameres1=="F2"){
      res1<-res1[[2]]
      name1<-'agg.cluster.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    }

    if(nameres2=="dir.admin1"){
      res2<-res2[[1]]
      name2<- 'dir.est'
      colnames(res2)[colnames(res2) == 'value'] <- name2
    }else if(nameres2=="dir.admin2"){
      res2<-res2[[2]]
      name2<- 'agg.dir.est'
      colnames(res2)[colnames(res2) == 'value'] <-name2
    }else if(nameres2=="smth.ad1"){
      res2<-res2[[1]]
      name2<- 'smth.est'
      colnames(res2)[colnames(res2) == 'value'] <- name2
    }else if(nameres2=="T1"){
      res2<-res2[[1]][21:30,]
      name2 <- 'strata.est'
      colnames(res2)[colnames(res2) == 'value']<- name2
    }else if(nameres2=="T2"){
      res2<-res2[[2]]
      name2<- 'agg.strata.est'
      colnames(res2)[colnames(res2) == 'value'] <- name2
    }else if(nameres2=="F1"){
      res2<-res2[[1]]
      name2 <- 'cluster.est'
      colnames(res2)[colnames(res2) == 'value']<- name2
    }else if(nameres2=="F2"){
      res2<-res2[[2]]
      name2<-'agg.cluster.est'
      colnames(res2)[colnames(res2) == 'value'] <- name2
    }




    xandy<-c(name1, name2)
    # print(xandy)

  }else if (scatter=="se"){
    if(nameres1=="dir.admin1"){
      res1<-res1[[1]]
      name1<- 'direct.sd'
      colnames(res1)[colnames(res1) == 'sd'] <-name1

    }else if(nameres1=="smth.ad1"){
      res1<-res1[[1]]
      res1$smth.dir.sd<-sqrt(res1$var)
      name1<-"smth.dir.sd"
    }else if(nameres1=="T1"){
      res1<-res1[[1]][21:30,]
      name1<- 'strata.sd'
      colnames(res1)[colnames(res1) == 'sd'] <-name1
    }else if(nameres1=="T2"){
      res1<-res1[[2]]
      name1<- 'agg.strata.sd'
      colnames(res1)[colnames(res1) == 'sd'] <- name1
    }else if(nameres1=="F1"){
      res1<-res1[[1]]
      name1<- 'cluster.sd'
      colnames(res1)[colnames(res1) == 'sd'] <-name1
    }else if(nameres1=="F2"){
      res1<-res1[[2]]
      name1<-'agg.cluster.sd'
      colnames(res1)[colnames(res1) == 'sd'] <- name1
    }
    # } else if(nameres1=="dir.admin2"){
    #   res1<-res1[[2]]
    #   colnames(res1)[colnames(res1) == 'value'] <- 'agg.dir.est'

    if(nameres2=="dir.admin1"){
      res2<-res2[[1]]
      name2<- 'direct.sd'
      colnames(res2)[colnames(res2) == 'sd'] <-name2

    }else if(nameres2=="smth.ad1"){
      res2<-res2[[1]]
      res2$smth.dir.sd<-sqrt(res2$var)
      name2<-'smth.dir.sd'
    }else if(nameres2=="T1"){
      res2<-res2[[1]][21:30,]
      name2<- 'strata.sd'
      colnames(res2)[colnames(res2) == 'sd'] <-name2
    }else if(nameres2=="T2"){
      res2<-res2[[2]]
      name2<- 'agg.strata.sd'
      colnames(res2)[colnames(res2) == 'sd'] <- name2
    }else if(nameres2=="F1"){
      res2<-res2[[1]]
      name2<- 'cluster.sd'
      colnames(res2)[colnames(res2) == 'sd'] <-name2
    }else if(nameres2=="F2"){
      res2<-res2[[2]]
      name2<-'agg.cluster.sd'
      colnames(res2)[colnames(res2) == 'sd'] <- name2
    }

    xandy<-c(name1, name2)
    # xandy<-c(colnames(res1)[length(colnames(res1))], colnames(res2)[length(colnames(res2))])

  }

  df<- merge(x = res1, y = res2, by="admin1.name" )
  # print(df)

  lim <- range(c(df[,c(xandy[1])], df[,c(xandy[2])]), na.rm = TRUE)
  ggplot(df, aes(x=df[,c(xandy[1])],y=df[,c(xandy[2])])) +
    geom_point(alpha = 0.5,aes())+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
    labs(title = paste0(scatter, ":", xandy[1]," ", "versus", " " ,xandy[2]))+
    xlab(paste0(xandy[1]))+
    ylab(paste0(xandy[2]))+
  xlim(lim) + ylim(lim)



}else if(admin==2){

  if(scatter=="estimate"){


    if(nameres1=="dir.admin2"){
      res1<-res1[[1]]
      name1<- 'direct.est'
      colnames(res1)[colnames(res1) == 'value'] <-name1
    }else if(nameres1=="smth.est2"){
      res1<-res1[[2]]
      name1<- 'smth.dir.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
     }else if(nameres1=="T2"){
      res1<-res1[[1]]
      res1<- res1[which(res1$type =="aggregated"),]
      name1<- 'strata.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    }else if(nameres1=="F2"){
      res1<-res1[[1]]
      name1<-'cluster.est'
      colnames(res1)[colnames(res1) == 'value'] <- name1
    }

      if(nameres2=="dir.admin2"){
        res2<-res2[[1]]
        name2<- 'direct.est'
        colnames(res2)[colnames(res2) == 'value'] <-name2
      }else if(nameres2=="smth.est2"){
        res2<-res2[[1]]
        name2<- 'smth.dir.est'
        colnames(res1)[colnames(res1) == 'value'] <- name2
      }else if(nameres2=="T2"){
        res2<-res2[[1]]
        res2<-res2[which(res2$type=="aggregated"),]
        name2<- 'strata.est'
        colnames(res2)[colnames(res2) == 'value'] <- name2
      }else if(nameres2=="F2"){
        res2<-res2[[1]]
        name2<-'cluster.est'
        colnames(res2)[colnames(res2) == 'value'] <- name2
      }




    xandy<-c(name1, name2)
    # print(xandy)

  }else if (scatter=="se"){

    if(nameres1=="dir.admin2"){
      res1<-res1[[1]]
      name1<- 'direct.sd'
      colnames(res1)[colnames(res1) == 'sd'] <-name1
    }else if(nameres1=="T2"){
      res1<-res1[[1]]
      res1<-res1[which(res1$type=="aggregated"),]
      name1<- 'strata.sd'
      colnames(res1)[colnames(res1) == 'sd'] <- name1
    }else if(nameres1=="F2"){
      res1<-res1[[1]]
      name1<-'cluster.sd'
      colnames(res1)[colnames(res1) == 'sd'] <- name1
    }
    # } else if(nameres1=="dir.admin2"){
    #   res1<-res1[[2]]
    #   colnames(res1)[colnames(res1) == 'value'] <- 'agg.dir.est'

    if(nameres2=="dir.admin2"){
      res2<-res2[[1]]
      name2<- 'direct.sd'
      colnames(res2)[colnames(res2) == 'sd'] <-name2
    }else if(nameres2=="T2"){
      res2<-res2[[1]]
      res2<-res2[which(res2$type=="aggregated"),]
      name2<- 'strata.sd'
      colnames(res2)[colnames(res2) == 'sd'] <- name2
    }else if(nameres2=="F2"){
      res2<-res2[[1]]
      name2<-'cluster.sd'
      colnames(res2)[colnames(res2) == 'sd'] <- name2
    }

    xandy<-c(name1, name2)
    # xandy<-c(colnames(res1)[length(colnames(res1))], colnames(res2)[length(colnames(res2))])

  }

  df<- merge(x = res1, y = res2, by="admin2.name" )
  # print(df)

  lim <- range(c(df[,c(xandy[1])], df[,c(xandy[2])]), na.rm = TRUE)
  ggplot(df, aes(x=df[,c(xandy[1])],y=df[,c(xandy[2])])) +
    geom_point(alpha = 0.5,aes())+
    geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
    labs(title = paste0(scatter, ":", xandy[1]," ", "versus", " " ,xandy[2]))+
    xlab(paste0(xandy[1]))+
    ylab(paste0(xandy[2]))+
    xlim(lim) + ylim(lim)


}


}

