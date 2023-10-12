#' Calculate cluster model estimates using beta binomial model
#'
#' This function calculate smoothed direct estimates at given admin level.
#'
#' @param data  dataframe that contains the indicator of interests
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admin.info dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#' @param spatialmodel model for area-level random effects, "iid" or "bym2.
#' @param stata whether or not to include urban/rural stratum.
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
#'   \item { clusterModel.result
#' }
#' @import dplyr
#' @importFrom survey svydesign svyby
#' @importFrom data.table data.table
#' @importFrom utils tail
#' @importFrom stats sd quantile rnorm
#' 
#' @importFrom SUMMER smoothSurvey
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export


clusterModel<-function(data,cluster.info,admin,admin.info,spatialmodel,stata){

  admin.mat <- admin.info$admin.mat
  admin.info <- admin.info$admin.info
  modt<- left_join(data,cluster.info$cluster.info,by="cluster")
  modt<- modt[!(is.na(modt$LONGNUM)), ]
  # modt$strata.full <- paste(modt$admin1.name, modt$strata)

  c.dat.tmp <- modt %>%
    group_by(cluster) %>%
    mutate(n = length(cluster)) %>%
    mutate(value = sum(value,na.rm = T)) %>%
    ungroup()%>%
    distinct( cluster, .keep_all = TRUE)


  admin_name_table<-admin.info
  nregion <- dim(admin_name_table)[1]
  admin_name_table$sID <- 1: dim(admin_name_table)[1]

  # add new rows corresponding to each region
  c.dat.tmp[(dim(c.dat.tmp)[1]+1):(dim(c.dat.tmp)[1]+nregion),paste0("admin",admin,".name")] <- admin_name_table[,which(colnames(admin_name_table)== paste0("admin",admin,".name"))]
  c.dat.tmp$ID <- 1:dim(c.dat.tmp)[1]
  c.dat.tmp$sID <-admin_name_table$sID[match(as.data.frame(c.dat.tmp)[,which(colnames(c.dat.tmp)== paste0("admin",admin,".name"))],
                                             admin_name_table[,which(colnames(admin_name_table)== paste0("admin",admin,".name"))])]





  ## MODEL setup
  if(stata==F){
    if(spatialmodel=="iid"){
      pc.u = 1
      pc.alpha = 0.01
      formula <- value ~ 1 + f(sID, model = spatialmodel,graph = admin.mat, hyper = list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha))))
    }else if(spatialmodel=="bym2"){

      pc.u = 1
      pc.alpha = 0.01
      pc.u.phi <- 0.5
      pc.alpha.phi <- 2/3
      formula <- value ~ 1+
        f(sID, model = spatialmodel, graph = admin.mat,
          hyper = list(
            prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)),
            phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))

    }

  }else if(stata==T){
    if(spatialmodel=="iid"){
      pc.u = 1
      pc.alpha = 0.01
      formula <- value ~ 1 + strata+ f(sID, model = spatialmodel,graph = admin.mat, hyper = list(prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha))))
    }else if(spatialmodel=="bym2"){

      pc.u = 1
      pc.alpha = 0.01
      pc.u.phi <- 0.5
      pc.alpha.phi <- 2/3
      formula <- value ~ 1  + strata +
        f(sID, model = spatialmodel, graph = admin.mat,
          hyper = list(
            prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)),
            phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))

    }


  }





  ## MODELING:

  imod<- INLA::inla(formula,
                    family="betabinomial",
                    data=c.dat.tmp,
                    Ntrials=n,
                    control.predictor = list(compute=TRUE, link = 1),
                    control.compute = list(config = TRUE))


  nsamp <- 1000
  samp <- INLA::inla.posterior.sample(n = nsamp, result = imod, intern = TRUE)


  if(stata==F){

    draw.all <- matrix(NA, nsamp, nregion)
    for(i in 1:length(samp)){
      tmp <- samp[[i]]$latent
      s.effect <- tmp[paste0("sID:", 1:nregion), 1]
      intercept <- tmp["(Intercept):1", 1]
      draw.all[i, ] <- SUMMER::expit(s.effect + intercept)
    }
  }else if(stata==T){
    draw.u <- matrix(NA, nsamp, nregion)
    draw.r <- matrix(NA, nsamp, nregion)
    draw.all <- matrix(NA, nsamp, nregion)


    # nregion=115
    for(i in 1:length(samp)){
      tmp <- samp[[i]]$latent
      s.effect <- tmp[paste0("sID:", 1:nregion), 1]
      intercept <- tmp["(Intercept):1", 1]
      str.effect <- tmp["stratarural:1", 1]
      draw.u[i, ] <- expit(s.effect + intercept)
      draw.r[i, ] <- expit(s.effect + intercept + str.effect)
      draw.all[i, ] <- draw.u[i, ] * admin.info$urban +
        draw.r[i, ] * (1 - admin.info$urban)
    }

  }

  if(admin==1){
    if(stata==F){
      admin1.bb.res<- data.frame(cbind(
        admin1.name=admin.info$admin1.name[tail(c.dat.tmp$sID,n=nregion)],
        value= tail(imod$summary.fitted.values,n=nregion)[,1],
        sd= tail(imod$summary.fitted.values,n=nregion)[,2],
        quant025= tail(imod$summary.fitted.values,n=nregion)[,3],
        quant975= tail(imod$summary.fitted.values,n=nregion)[,5]))

      admin1.bb.res$value<-as.numeric (admin1.bb.res$value)
      admin1.bb.res$sd<-as.numeric (admin1.bb.res$sd)
      admin1.bb.res$quant025<-as.numeric (admin1.bb.res$quant025)
      admin1.bb.res$quant975<-as.numeric (admin1.bb.res$quant975)

    }else if(stata==T){
      post.u <- apply(draw.u, 2, mean)
      post.r <- apply(draw.r, 2, mean)
      post.all <- apply(draw.all, 2, mean)

      post.u.sd <- apply(draw.u, 2, sd)
      post.r.sd <- apply(draw.r, 2, sd)
      post.all.sd <- apply(draw.all, 2, sd)


      post.u.ci <- apply(draw.u, 2, quantile, probs = c(0.025,0.975))
      post.r.ci <- apply(draw.r, 2,  quantile, probs = c(0.025,0.975))
      post.all.ci <- apply(draw.all, 2,  quantile, probs = c(0.025,0.975))

      admin1.bb.res <- data.frame(value = c(post.u, post.r, post.all),
                                  sd = c(post.u.sd, post.r.sd, post.all.sd),
                                  quant025=c(post.u.ci[1,], post.r.ci[1,], post.all.ci[1,]),
                                  quant975=c(post.u.ci[2,], post.r.ci[2,], post.all.ci[2,]),
                                  type = c(rep("urban", nregion), rep("rural", nregion),
                                           rep("aggregated", nregion)))
      admin1.bb.res$admin1.name <- rep(admin.info$admin1.name, 3)

    }
    #admin.bb.res

    #agg.nationl
    post.all <- draw.all%*% admin.info$population/sum(admin.info$population)
    agg.natl <- data.frame(value = mean(post.all),
                           sd = sd(post.all),
                           quant025=quantile(post.all, probs = c(0.025,0.975))[1],
                           quant975=quantile(post.all, probs = c(0.025,0.975))[2])

    return(list(admin1.bb.res=admin1.bb.res,agg.natl=agg.natl,inla=imod,
                admin1_post=draw.all,nation_post=post.all))

  }else if(admin==2){

    if(stata==F){

      admin2.bb.res<- data.frame(cbind(
        DistrictName=admin.info$DistrictName[tail(c.dat.tmp$sID,n=nregion)],
        value= tail(imod$summary.fitted.values,n=nregion)[,1],
        sd= tail(imod$summary.fitted.values,n=nregion)[,2],
        quant025= tail(imod$summary.fitted.values,n=nregion)[,3],
        quant975= tail(imod$summary.fitted.values,n=nregion)[,5]))

      admin2.bb.res$value<-as.numeric (admin2.bb.res$value)
      admin2.bb.res$sd<-as.numeric (admin2.bb.res$sd)
      admin2.bb.res$quant025<-as.numeric (admin2.bb.res$quant025)
      admin2.bb.res$quant975<-as.numeric (admin2.bb.res$quant975)
      admin2.bb.res<-left_join(admin2.bb.res,distinct(admin.info),by="DistrictName")

    }else if(stata==T){
      post.u <- apply(draw.u, 2, mean)
      post.r <- apply(draw.r, 2, mean)
      post.all <- apply(draw.all, 2, mean)

      post.u.sd <- apply(draw.u, 2, sd)
      post.r.sd <- apply(draw.r, 2, sd)
      post.all.sd <- apply(draw.all, 2, sd)


      post.u.ci <- apply(draw.u, 2, quantile, probs = c(0.025,0.975))
      post.r.ci <- apply(draw.r, 2,  quantile, probs = c(0.025,0.975))
      post.all.ci <- apply(draw.all, 2,  quantile, probs = c(0.025,0.975))

      admin2.bb.res <- data.frame(value = c(post.u, post.r, post.all),
                                  sd = c(post.u.sd, post.r.sd, post.all.sd),
                                  quant025=c(post.u.ci[1,], post.r.ci[1,], post.all.ci[1,]),
                                  quant975=c(post.u.ci[2,], post.r.ci[2,], post.all.ci[2,]),
                                  type = c(rep("urban", nregion), rep("rural", nregion),
                                           rep("aggregated", nregion)))
      admin2.bb.res$DistrictName <- rep(admin.info$DistrictName, 3)
      admin2.bb.res<-left_join(admin2.bb.res,distinct(admin.info),by="DistrictName")

    }



    #agg admin1
    weight=admin.info$population/admin.info$population1

    # post.all <-  data.table(weight*draw.all)
    post.all <-  data.table::data.table(t(weight*t(draw.all)))#nrow=legth(weight)
    colnames(post.all) <- admin.info$admin1.name
    subgroups<-split.default(post.all, names(post.all))

    sums_list <- lapply(subgroups, function(subgroup) {
      rowSums(subgroup)
    })
    admin1.samp <- do.call(cbind, sums_list)


    agg.admin1 <- data.frame(value = colMeans(admin1.samp),
                             sd =  apply(admin1.samp, 2, sd),
                             quant025= apply(admin1.samp, 2,  quantile, probs = c(0.025,0.975))[1,],
                             quant975= apply(admin1.samp, 2,  quantile, probs = c(0.025,0.975))[2,]
                            )
    agg.admin1$admin1.name=rownames(agg.admin1)
    #agg national
    unique( admin.info$population1)/sum(unique( admin.info$population1))

    post.all <- admin1.samp%*% unique( admin.info$population1)/sum(unique( admin.info$population1))
    agg.natl <- data.frame(value = mean(post.all),
                           sd = sd(post.all),
                           quant025=quantile(post.all, probs = c(0.025,0.975))[1],
                           quant975=quantile(post.all, probs = c(0.025,0.975))[2])


    return(list(admin2.bb.res=admin2.bb.res,agg.admin1=agg.admin1,agg.natl=agg.natl,inla=imod,
                admin2_post=draw.all,admin1_post=admin1.samp,nation_post=post.all))



  }






}
