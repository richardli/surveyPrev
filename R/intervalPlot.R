#' Get scatter plot for any two model results
#'
#' This function return scatter plot at admin 1 level for any two model results
#'
#' @param model  list of model results  using surveyPrev
#' @param admin  level of plot
#' @param group plot by group or not
#' @param compare plot for compare multiple plot or not
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
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#'
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4+")
#' admin.info2 <- adminInfo(geo = ZambiaAdm2,
#'                         admin = 2,
#'                         agg.pop =ZambiaPopWomen$admin2_pop,
#'                         proportion = ZambiaPopWomen$admin2_urban)
#  # unstratified model
#' cl_res_ad2_unstrat <- clusterModel(data = data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   stratification = FALSE,
#'                   model = "bym2",
#'                   admin = 2,
#'                   aggregation = TRUE,
#'                   CI = 0.95)
#'
#' head(cl_res_ad2_unstrat$res.admin2)
#' head(cl_res_ad2_unstrat$agg.admin1)
#' plots <- intervalPlot(cl_res_ad2_unstrat)
#' plots[["Central"]]
#'
#  # unstratified model
#' cl_res_ad2 <- clusterModel(data = data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   stratification = TRUE,
#'                   model = "bym2",
#'                   admin = 2,
#'                   aggregation = TRUE,
#'                   CI = 0.95)
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
#'


intervalPlot <- function(admin= 0, compare=F, model=list( list("name 1"= fit1, "name 1"= fit2)), group=F){


  if(compare){

  if(admin==0){

    dt=data.frame(mean=NA, lower=NA, upper=NA, model=NA,group=NA)




    for (i in 1:length(model)) {

      if(colnames(model[[i]]$agg.natl[1])=="direct.est"){
        colnames(model[[i]]$agg.natl)[colnames(model[[i]]$agg.natl) == 'direct.est'] <- 'mean'
        colnames(model[[i]]$agg.natl)[colnames(model[[i]]$agg.natl) == 'direct.lower'] <- 'lower'
        colnames(model[[i]]$agg.natl)[colnames(model[[i]]$agg.natl) == 'direct.upper'] <- 'upper'
      }

      model[[i]]$agg.natl$model= names(model[i])
      dt[i,]=model[[i]]$agg.natl[,c("mean","lower","upper","model")]
    }


    if(group){
      for (i in 1:length(model)) {
        dt[i,]$group= model[[i]]$group
      }
    }

    rownames(dt)<-dt$model


    if(group){
      ggplot(dt, aes(y = model, x = mean,group = group)) +
        geom_point(aes(shape=group)) +
        geom_errorbarh(aes(xmin = lower , xmax = upper), alpha = 0.5)

    }else{
      ggplot(dt, aes(y = model, x = mean)) +
        geom_point(aes()) +
        geom_errorbarh(aes(xmin = lower , xmax = upper), alpha = 0.5)

    }




  }else if (admin==1){

    dt=data.frame(admin1.name=rep(NA,500),mean=rep(NA,500), lower=rep(NA,500), upper=rep(NA,500), model=rep(NA,500),group=rep(NA,500))

    for (i in 1:length(model)) {




      if( is.null(model[[i]]$agg.admin1)){

        if(colnames(model[[i]]$res.admin1)[2]=="direct.est"){
          colnames(model[[i]]$res.admin1)[colnames(model[[i]]$res.admin1) == 'direct.est'] <- 'mean'
          colnames(model[[i]]$res.admin1)[colnames(model[[i]]$res.admin1) == 'direct.lower'] <- 'lower'
          colnames(model[[i]]$res.admin1)[colnames(model[[i]]$res.admin1) == 'direct.upper'] <- 'upper'
        }
        model[[i]]$res.admin1$model= names(model[i])

        if( !is.null(model[[i]]$res.admin1$type)){
          model[[i]]$res.admin1=model[[i]]$res.admin1[model[[i]]$res.admin1$type=="full",]
        }


        dd=dim(model[[i]]$res.admin1)[1]


        dt[(1+ (i-1)*dd):(i*dd),]= model[[i]]$res.admin1[,c("admin1.name","mean","lower","upper","model")]
        if(group){
          dt[(1+ (i-1)*dd):(i*dd),]$group= model[[i]]$group
        }
      }else{

        if(colnames(model[[i]]$agg.admin1)[2]=="direct.est"){
          colnames(model[[i]]$agg.admin1)[colnames(model[[i]]$agg.admin1) == 'direct.est'] <- 'mean'
          colnames(model[[i]]$agg.admin1)[colnames(model[[i]]$agg.admin1) == 'direct.lower'] <- 'lower'
          colnames(model[[i]]$agg.admin1)[colnames(model[[i]]$agg.admin1) == 'direct.upper'] <- 'upper'
        }
        model[[i]]$agg.admin1$model= names(model[i])

        if( !is.null(model[[i]]$agg.admin1$type)){
          model[[i]]$agg.admin1=model[[i]]$agg.admin1[model[[i]]$agg.admin1$type=="full",]
        }


        dd=dim(model[[i]]$agg.admin1)[1]
        dt[(1+ (i-1)*dd):(i*dd),]= model[[i]]$agg.admin1[,c("admin1.name","mean","lower","upper","model")]
        if(group){
          dt[(1+ (i-1)*dd):(i*dd),]$group= model[[i]]$group
        }
      }
    }

    dt=na.omit(dt)




    if(group){

      ggplot(dt, aes(x = admin1.name, y = mean, group = model, color = group  ,shape=model)) +
        geom_point( position = position_dodge(width = 0.8)) +
        scale_shape_manual(values = c(0:5, 15:19)) +
        geom_errorbar(aes(ymin = lower,
                          ymax = upper, group = model), alpha = 0.8, position = position_dodge(width = 0.8))+
        # scale_color_manual(values = pal_npg("nrc")(10) )+
        # scale_color_manual(values = pal_npg("nrc")(10) )+
        scale_color_brewer(palette="Set1") +

        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "", x = "Region", y = "value")+
        theme(legend.title = element_text(size=10), # Increase legend title size
              legend.text = element_text(size=10), # Increase legend text size
              legend.key.size = unit(1.5, 'lines'), # Increase legend key size
              axis.text.x = element_text(size=10), # Increase x axis text size
              axis.text.y = element_text(size=10))




    }else{
      ggplot(dt, aes(x = admin1.name, y = mean, group = model, color = model)) +
        geom_point( position = position_dodge(width = 0.8)) +
        scale_shape_manual(values = c(0:5, 15:19)) +
        geom_errorbar(aes(ymin = lower,
                          ymax = upper, group = model), alpha = 0.8, position = position_dodge(width = 0.8))+
        scale_color_brewer(palette="Set1") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "", x = "Region", y = "value")+
        theme(legend.title = element_text(size=10), # Increase legend title size
              legend.text = element_text(size=10), # Increase legend text size
              legend.key.size = unit(1.5, 'lines'), # Increase legend key size
              axis.text.x = element_text(size=10), # Increase x axis text size
              axis.text.y = element_text(size=10))

    }

  }else{







  }


  }else{


     res=model[[1]]
     data<-res[[1]]
     linedata<-res[[2]]
     line2<-res[[3]]$mean

     plot_fun <- function(dat) {
       line1 = linedata[unique(dat$admin1.name),"mean"]
       g <- ggplot(dat)

       if("type" %in% colnames(dat)){
         g <- g + aes(x = admin2.name, y = mean, color = type)
       }else{
         g <- g + aes(x = admin2.name, y = mean)
       }
       g <- g +
           geom_point( position = position_dodge(width = 0.5),size = .8) +
           geom_hline(  aes(yintercept =line1,linetype = "dashed"),color="#d95f02",linewidth = .8) +
           geom_hline( aes(yintercept =line2, linetype = "solid"),color="#d95f02",linewidth = .8) +
           geom_errorbar(aes(ymin = lower , ymax = upper), alpha = 0.7,position = position_dodge(width = 0.5), width = 0.2) +
           scale_color_brewer(palette = "Set1") +
         labs(title = unique(dat$admin1.name)) +
         xlab("") + ylab("") +
         scale_linetype_manual(values = c("dashed", "solid"),
                               labels = c("admin1","national")) +
         theme_bw() +
         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
     }

    plots <- NULL
    for (adm1 in unique(data$admin1.name)){
     g <- plot_fun(subset(data, admin1.name == adm1))
     plots[[adm1]] <- g
    }

     return(plots)

  }










}

