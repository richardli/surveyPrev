#' Get scatter plot for any two model results
#'
#' This function return scatter plot at admin 1 level for any two model results
#' @param title title
#' @param label1 label for x axis
#' @param label2 label for y axis
#' @param res1  model result 1 using surveyPrev
#' @param value1 value1
#' @param res2  model result 2 using surveyPrev
#' @param value2 value2
#' @param by.res1  by.res1
#' @param by.res2  by.res2
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level

#' @import ggplot2
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' data(ZambiaPopWomen)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#'
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#'
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4+")
#' admin.info1 <- adminInfo(poly.adm = ZambiaAdm1,
#'                         admin = 1,
#'                         agg.pop =ZambiaPopWomen$admin1_pop,
#'                         proportion = ZambiaPopWomen$admin1_urban)
#' smth_res_ad1 <- fhModel(data,
#'                        cluster.info = cluster.info,
#'                        admin.info = admin.info1,
#'                        admin = 1,
#'                        model = "bym2",
#'                        aggregation = F)
#'
#' admin.info2 <- adminInfo(poly.adm = ZambiaAdm2,
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
#' scatterPlot(
#'      res1 = smth_res_ad1,
#'      res2 = cl_res_ad2$agg.admin1,
#'      value1 = "value",
#'      value2 = "value",
#'      by.res1 = "admin1.name",
#'      by.res2 = "admin1.name",
#'      title = "Aggregated cluster model v.s. Fay–Herriot",
#'      label1 = "Fay–Herriot",
#'      label2 = "Aggregated cluster model")
#'
#' }
#'
#'
#' @export

scatterPlot<-function(res1,value1,res2,value2,label1,label2,by.res1,by.res2,title){

  ## TODO: add a better check for consistency, as sometimes estimates can be missing rows
  # if(dim(res1)[1]!=dim(res2)[1]){
    # stop("Two results are not in the same area level")  }

  res1$value_x=res1[,value1]
  res2$value_y=res2[,value2]


    df<- merge(res1, res2, by.x =by.res1, by.y =by.res2)

    if(length(res1$value_x)<length(res2$value_y)){
    missing <- subset(res2, !res2$admin2.name.full %in% res1$admin2.name.full)
      missing$value_x=rep(min(c(df$value_x, df$value_y)),dim(missing)[1])
    }else if( length( res1$value_x)>length(res2$value_y)){
      missing<-subset(res1, !res1$admin2.name.full %in% res2$admin2.name.full)
      missing$value_y=rep(min(c(df$value_x, df$value_y)),dim(missing)[1])
    }else if(length(res1$value_x)==length(res2$value_y)){

    }
    if(length(res1$value_x)==length(res2$value_y)){
      lim <- range(c(df$value_x, df$value_y), na.rm = TRUE)
      ggplot(df, aes(x=value_x,y=value_y)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.5, color = "royalblue") +
        # geom_point(data = missing, aes(x=value_x,y=value_y), color = "red", shape=17)+
        labs(title = title)+
        xlab(label1)+
        ylab(label2)+
        xlim(lim) + ylim(lim) + theme_bw()

    }else{
      lim <- range(c(df$value_x, df$value_y), na.rm = TRUE)
      ggplot(df, aes(x=value_x,y=value_y)) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
        geom_point(alpha = 0.5, color = "royalblue") +
        # annotate("text", x = 5, y = 0, label = "This is a text box", size = 5, hjust = 0, vjust = 0, color = "red")
        geom_point(data = missing, aes(x=value_x,y=value_y), color = "red", shape=17)+
        labs(title = title)+
        xlab(label1)+
        ylab(label2)+
        xlim(lim) + ylim(lim) + theme_bw()

    }





# if(admin==1){
#   if(scatter=="estimate"){
#     if(nameres1=="dir.admin1"){
#       res1<-res1[[1]]
#       name1<-'dir.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     } else if(nameres1=="dir.admin2"){
#       res1<-res1[[2]]
#       name1<- 'agg.dir.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     } else if(nameres1=="smth.ad1"){
#       res1<-res1[[1]]
#       name1<- 'smth.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     }else if(nameres1=="T1"){
#       res1<-res1[[1]][21:30,]
#       name1<- 'strata.est'
#       colnames(res1)[colnames(res1) == 'value'] <-name1
#     }else if(nameres1=="T2"){
#       res1<-res1[[2]]
#       name1<- 'agg.strata.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     }else if(nameres1=="F1"){
#       res1<-res1[[1]]
#       name1<- 'cluster.est'
#       colnames(res1)[colnames(res1) == 'value'] <-name1
#     }else if(nameres1=="F2"){
#       res1<-res1[[2]]
#       name1<-'agg.cluster.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     }
#
#     if(nameres2=="dir.admin1"){
#       res2<-res2[[1]]
#       name2<- 'dir.est'
#       colnames(res2)[colnames(res2) == 'value'] <- name2
#     }else if(nameres2=="dir.admin2"){
#       res2<-res2[[2]]
#       name2<- 'agg.dir.est'
#       colnames(res2)[colnames(res2) == 'value'] <-name2
#     }else if(nameres2=="smth.ad1"){
#       res2<-res2[[1]]
#       name2<- 'smth.est'
#       colnames(res2)[colnames(res2) == 'value'] <- name2
#     }else if(nameres2=="T1"){
#       res2<-res2[[1]][21:30,]
#       name2 <- 'strata.est'
#       colnames(res2)[colnames(res2) == 'value']<- name2
#     }else if(nameres2=="T2"){
#       res2<-res2[[2]]
#       name2<- 'agg.strata.est'
#       colnames(res2)[colnames(res2) == 'value'] <- name2
#     }else if(nameres2=="F1"){
#       res2<-res2[[1]]
#       name2 <- 'cluster.est'
#       colnames(res2)[colnames(res2) == 'value']<- name2
#     }else if(nameres2=="F2"){
#       res2<-res2[[2]]
#       name2<-'agg.cluster.est'
#       colnames(res2)[colnames(res2) == 'value'] <- name2
#     }
#
#
#
#
#     xandy<-c(name1, name2)
#     # print(xandy)
#
#   }else if (scatter=="se"){
#     if(nameres1=="dir.admin1"){
#       res1<-res1[[1]]
#       name1<- 'direct.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <-name1
#
#     }else if(nameres1=="smth.ad1"){
#       res1<-res1[[1]]
#       res1$smth.dir.sd<-sqrt(res1$var)
#       name1<-"smth.dir.sd"
#     }else if(nameres1=="T1"){
#       res1<-res1[[1]][21:30,]
#       name1<- 'strata.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <-name1
#     }else if(nameres1=="T2"){
#       res1<-res1[[2]]
#       name1<- 'agg.strata.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <- name1
#     }else if(nameres1=="F1"){
#       res1<-res1[[1]]
#       name1<- 'cluster.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <-name1
#     }else if(nameres1=="F2"){
#       res1<-res1[[2]]
#       name1<-'agg.cluster.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <- name1
#     }
#     # } else if(nameres1=="dir.admin2"){
#     #   res1<-res1[[2]]
#     #   colnames(res1)[colnames(res1) == 'value'] <- 'agg.dir.est'
#
#     if(nameres2=="dir.admin1"){
#       res2<-res2[[1]]
#       name2<- 'direct.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <-name2
#
#     }else if(nameres2=="smth.ad1"){
#       res2<-res2[[1]]
#       res2$smth.dir.sd<-sqrt(res2$var)
#       name2<-'smth.dir.sd'
#     }else if(nameres2=="T1"){
#       res2<-res2[[1]][21:30,]
#       name2<- 'strata.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <-name2
#     }else if(nameres2=="T2"){
#       res2<-res2[[2]]
#       name2<- 'agg.strata.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <- name2
#     }else if(nameres2=="F1"){
#       res2<-res2[[1]]
#       name2<- 'cluster.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <-name2
#     }else if(nameres2=="F2"){
#       res2<-res2[[2]]
#       name2<-'agg.cluster.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <- name2
#     }
#
#     xandy<-c(name1, name2)
#     # xandy<-c(colnames(res1)[length(colnames(res1))], colnames(res2)[length(colnames(res2))])
#
#   }
#
#   df<- merge(x = res1, y = res2, by="admin1.name" )
#   # print(df)
#
#   lim <- range(c(df[,c(xandy[1])], df[,c(xandy[2])]), na.rm = TRUE)
#   ggplot(df, aes(x=df[,c(xandy[1])],y=df[,c(xandy[2])])) +
#     geom_point(alpha = 0.5,aes())+
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
#     labs(title = paste0(scatter, ":", xandy[1]," ", "versus", " " ,xandy[2]))+
#     xlab(paste0(xandy[1]))+
#     ylab(paste0(xandy[2]))+
#   xlim(lim) + ylim(lim)
#
#
#
# }else if(admin==2){
#
#   if(scatter=="estimate"){
#
#
#     if(nameres1=="dir.admin2"){
#       res1<-res1[[1]]
#       name1<- 'direct.est'
#       colnames(res1)[colnames(res1) == 'value'] <-name1
#     }else if(nameres1=="smth.est2"){
#       res1<-res1[[2]]
#       name1<- 'smth.dir.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#      }else if(nameres1=="T2"){
#       res1<-res1[[1]]
#       res1<- res1[which(res1$type =="aggregated"),]
#       name1<- 'strata.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     }else if(nameres1=="F2"){
#       res1<-res1[[1]]
#       name1<-'cluster.est'
#       colnames(res1)[colnames(res1) == 'value'] <- name1
#     }
#
#       if(nameres2=="dir.admin2"){
#         res2<-res2[[1]]
#         name2<- 'direct.est'
#         colnames(res2)[colnames(res2) == 'value'] <-name2
#       }else if(nameres2=="smth.est2"){
#         res2<-res2[[1]]
#         name2<- 'smth.dir.est'
#         colnames(res1)[colnames(res1) == 'value'] <- name2
#       }else if(nameres2=="T2"){
#         res2<-res2[[1]]
#         res2<-res2[which(res2$type=="aggregated"),]
#         name2<- 'strata.est'
#         colnames(res2)[colnames(res2) == 'value'] <- name2
#       }else if(nameres2=="F2"){
#         res2<-res2[[1]]
#         name2<-'cluster.est'
#         colnames(res2)[colnames(res2) == 'value'] <- name2
#       }
#
#
#
#
#     xandy<-c(name1, name2)
#     # print(xandy)
#
#   }else if (scatter=="se"){
#
#     if(nameres1=="dir.admin2"){
#       res1<-res1[[1]]
#       name1<- 'direct.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <-name1
#     }else if(nameres1=="T2"){
#       res1<-res1[[1]]
#       res1<-res1[which(res1$type=="aggregated"),]
#       name1<- 'strata.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <- name1
#     }else if(nameres1=="F2"){
#       res1<-res1[[1]]
#       name1<-'cluster.sd'
#       colnames(res1)[colnames(res1) == 'sd'] <- name1
#     }
#     # } else if(nameres1=="dir.admin2"){
#     #   res1<-res1[[2]]
#     #   colnames(res1)[colnames(res1) == 'value'] <- 'agg.dir.est'
#
#     if(nameres2=="dir.admin2"){
#       res2<-res2[[1]]
#       name2<- 'direct.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <-name2
#     }else if(nameres2=="T2"){
#       res2<-res2[[1]]
#       res2<-res2[which(res2$type=="aggregated"),]
#       name2<- 'strata.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <- name2
#     }else if(nameres2=="F2"){
#       res2<-res2[[1]]
#       name2<-'cluster.sd'
#       colnames(res2)[colnames(res2) == 'sd'] <- name2
#     }
#
#     xandy<-c(name1, name2)
#     # xandy<-c(colnames(res1)[length(colnames(res1))], colnames(res2)[length(colnames(res2))])
#
#   }
#
#   df<- merge(x = res1, y = res2, by="admin2.name" )
#   # print(df)
#
#   lim <- range(c(df[,c(xandy[1])], df[,c(xandy[2])]), na.rm = TRUE)
#   ggplot(df, aes(x=df[,c(xandy[1])],y=df[,c(xandy[2])])) +
#     geom_point(alpha = 0.5,aes())+
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
#     labs(title = paste0(scatter, ":", xandy[1]," ", "versus", " " ,xandy[2]))+
#     xlab(paste0(xandy[1]))+
#     ylab(paste0(xandy[2]))+
#     xlim(lim) + ylim(lim)
#
#
# }


}

