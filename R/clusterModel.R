#' Calculate cluster model estimates using beta binomial model
#'
#' This function calculate smoothed direct estimates at given admin level.
#'
#' @param data dataframe that contains the indicator of interests(column name is value), output of getDHSindicator function
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admin.info dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#' @param X dataframe that contains areal covariates, the first column should be the same admin name as in admin.info$data.
#' @param CI Credible interval to be used. Default to 0.95.
#' @param model  smoothing model used in the random effect. Options are independent ("iid") or spatial ("bym2").
#' @param stratification whether or not to include urban/rural stratum.
#' @param aggregation whether or not report aggregation results.
#' @param nested whether or not to fit a nested model.
#' @param overdisp.mean prior mean for logit(d), where d is the intracluster correlation.
#' @param overdisp.prec prior precision for logit(d), where d is the intracluster correlation.
#' @param pc.u pc prior u for iid or bym2 precision.
#' @param pc.alpha pc prior alpha for iid or bym2 precision.
#' @param pc.u.phi pc prior u for bym2 mixing paramete.
#' @param pc.alpha.phi pc prior u for bym2 mixing paramete.
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
#' @import dplyr
#' @importFrom survey svydesign svyby
#' @importFrom data.table data.table
#' @importFrom utils tail
#' @importFrom stats sd quantile rnorm
#' @importFrom matrixStats colMedians
#' @importFrom stats median na.omit
#' @importFrom SUMMER smoothSurvey
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
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4")
#' admin.info1 <- adminInfo(poly.adm = ZambiaAdm1,
#'                         admin = 1,
#'                         agg.pop =ZambiaPopWomen$admin1_pop,
#'                         proportion = ZambiaPopWomen$admin1_urban)
#' cl_res_ad1 <- clusterModel(data=data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info1,
#'                   stratification = FALSE,
#'                   model = "bym2",
#'                   admin = 1,
#'                   aggregation = TRUE,
#'                   CI = 0.95)
#' cl_res_ad1$res.admin1
#'
#' # compare with the DHS direct estimates
#' dhs_table <- get_api_table(country = "ZM",
#'                            survey = "ZM2018DHS",
#'                            indicator = "RH_ANCN_W_N4P",
#'                            simplify = TRUE)
#' subset(dhs_table, ByVariableLabel == "Five years preceding the survey")
#'
#' }
#'
#' @export

clusterModel<-function(data,cluster.info, admin.info, X=NULL ,admin, CI = 0.95, model = c("bym2", "iid"),
                       stratification = FALSE, aggregation = FALSE,nested=FALSE,
                       overdisp.mean=0, overdisp.prec=0.4 , pc.u = 1,  pc.alpha = 0.01, pc.u.phi=0.5,pc.alpha.phi=2/3){

  if (sum(is.na(data$value)) > 0) {
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }


  # if(is.null(admin.info) && model != "iid"){
  #   message("No admin.info supplied. Using IID random effects.")
  #   model <- "iid"
  # }

  if (is.null(X)==FALSE && dim(X)[1]!=dim(admin.info$data)[1]) {

    message("Not valid covariates format. No covariates model is fitted instead")
    X=NULL
  }



  if( unique(is.na(admin.info$data$urban)) && stratification==T){
    message("No urban/rural proportion found. Unstratified model is fitted instead")
    stratification = FALSE
  }




  if( unique(is.na(admin.info$data$population)) && aggregation==T){
    message("No population found")
    aggregation = FALSE
  }


  if(model == "bym2"){
    Amat <- admin.info$mat
  }else{
    Amat <- NULL
  }


  admin.mat <- Amat# Amat is sorted by admin1.name then admin2.name alphabetically for admin2
  admin.info <- admin.info$data
  modt<- left_join(data,cluster.info$data,by="cluster")
  modt<- modt[!(is.na(modt$LONGNUM)), ]
  # modt$strata.full <- paste(modt$admin1.name, modt$strata)



  # nest model adjustment 1)nest  adjacency matrix
  if(nested&admin>1){
    #for nested model, make admin2.mat adjacency matrix within ad2


    # Convert admin1.name to a numeric vector based on the unique regions
    admin.info$admin1<- as.numeric(factor(admin.info$admin1.name))
    admin.info$admin2<- as.numeric(factor(admin.info$admin2.name.full))

    admin.key <- as.data.frame(admin.info[,c('admin1.name','admin2.name.full')])
    admin.key <- admin.key[order(admin.key$admin2.name.full),]


    admin2.mat.nested <- admin.mat

    for(i in 1:nrow(admin2.mat.nested)){

      admin2.mat.nested[i,which(admin.key$admin1.name!=admin.key[admin.key$admin2.name.full==row.names(admin2.mat.nested)[i],]$admin1.name)] <- 0
      if(sum(admin2.mat.nested[i,])>0){
        admin2.mat.nested[i,] <- admin2.mat.nested[i,]/sum(admin2.mat.nested[i,])
      }
    }


    # checking nesting was done correctly
    for(area in 1:nrow(admin2.mat.nested)){
      neighbors <- which(admin2.mat.nested[area,]!=0)
      # are all neighbors in the same admin1?
      if(unique(admin.key[admin.key$admin2.name.full %in% rownames(as.data.frame(neighbors)),]$admin1.name) != admin.key[admin.key$admin2.name.full==colnames(admin2.mat.nested)[area],]$admin1.name)
        message('Error in adjacency matrix, unnested model is fitted')
        nested==FALSE
    }



    # checking nesting was done correctly
    # for(area in 1:nrow(admin2.mat)){
    #   neighbors <- which(admin2.mat.nested[area,]!=0)
    #   # are all neighbors in the same admin1?
    #   if(unique(admin.key[admin.key$admin2 %in% neighbors,]$admin1) != admin.key[admin.key$admin2==area,]$admin1)
    #     stop('Error')
    # }

    admin.mat=admin2.mat.nested


  }else if(nested&admin==1){
    message("Nested model is designed for Admin 2 or finer level. An Admin 1 model will be fitted")
    nested==FALSE
  }

  c.dat.tmp <- modt %>%
    group_by(cluster) %>%
    mutate(n = length(cluster)) %>%
    mutate(value = sum(value,na.rm = T)) %>%
    ungroup()%>%
    distinct( cluster, .keep_all = TRUE)



  admin_name_table<-admin.info
  nregion <- dim(admin_name_table)[1]
  admin_name_table$sID <- 1: dim(admin_name_table)[1] #sID is sorted by admin1.name then admin2.name alphabetically.

  # add new rows corresponding to each region

 if(admin==1){
   c.dat.tmp[(dim(c.dat.tmp)[1]+1):(dim(c.dat.tmp)[1]+nregion),paste0("admin",admin,".name")] <- admin_name_table[,which(colnames(admin_name_table)== paste0("admin",admin,".name"))]
   c.dat.tmp$ID <- 1:dim(c.dat.tmp)[1]
   c.dat.tmp$sID <-admin_name_table$sID[match(as.data.frame(c.dat.tmp)[,which(colnames(c.dat.tmp)== paste0("admin",admin,".name"))],
                                              admin_name_table[,which(colnames(admin_name_table)== paste0("admin",admin,".name"))])]

   #adding area level covariates
   if(is.null(X)==FALSE){
     c.dat.tmp<-left_join(c.dat.tmp,X,by="admin1.name")
   }

 }else{
   c.dat.tmp[(dim(c.dat.tmp)[1]+1):(dim(c.dat.tmp)[1]+nregion),"admin2.name.full"] <- admin_name_table[,which(colnames(admin_name_table)=="admin2.name.full")]
   c.dat.tmp$ID <- 1:dim(c.dat.tmp)[1]
   c.dat.tmp$sID <-admin_name_table$sID[match(as.data.frame(c.dat.tmp)[,which(colnames(c.dat.tmp)== "admin2.name.full")],
                                              admin_name_table[,which(colnames(admin_name_table)== "admin2.name.full")])]
   #adding area level covariates


   if(nested){
     c.dat.tmp[(dim(c.dat.tmp)[1]-(nregion-1)):(dim(c.dat.tmp)[1]),"admin1.name"] <- admin_name_table[,which(colnames(admin_name_table)== "admin1.name")]

   }
   if(is.null(X)==FALSE){
     c.dat.tmp<-left_join(c.dat.tmp,X,by="admin2.name.full")
   }

 }



  ## MODEL setup
  if(stratification==F){
    if(model=="iid"){
      # pc.u = 1
      # pc.alpha = 0.01
      # formula <- value ~ 1 + f(sID, model = model,graph = admin.mat,
      #                          hyper = list(prec = list(prior = "pc.prec",
      #                                                   param = c(pc.u , pc.alpha))))


      if(is.null(X)==FALSE){
        cvrt <- paste(" + ",colnames(X)[2:dim(X)[2]], collapse = "")
      }else{
        cvrt=NULL
      }
      sptl <- "+ f(sID, model = model, graph = admin.mat, hyper = list(prec = list(prior = \"pc.prec\", param = c(pc.u , pc.alpha))))"
      formula_string <- paste("value ~ 1", cvrt,sptl)
      formula <- as.formula(formula_string)



    }else if(model=="bym2"){

      # pc.u = 1
      # pc.alpha = 0.01
      # pc.u.phi <- 0.5
      # pc.alpha.phi <- 2/3

       # formula <- value ~ 1+
       # f(sID, model = model, graph = admin.mat,
       #   hyper = list( prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)),
       #                 phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))


      if(is.null(X)==FALSE){
        cvrt <- paste(" + ",colnames(X)[2:dim(X)[2]], collapse = "")
      }else{
        cvrt=NULL
      }

      ### for nested model,addscale.model=T, constr=T, adjust.for.con.comp=T,
      if(nested){
        sptl <- "+ f(sID, model = model, graph = admin.mat,scale.model=T, constr=T, adjust.for.con.comp=T, hyper = list( prec = list(prior = \"pc.prec\", param = c(pc.u , pc.alpha)), phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))"
        formula_string <- paste("value ~ -1+ admin1.name", cvrt,sptl)
      }else{
        sptl <- "+ f(sID, model = model, graph = admin.mat,  hyper = list( prec = list(prior = \"pc.prec\", param = c(pc.u , pc.alpha)), phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))"
        formula_string <- paste("value ~ 1", cvrt,sptl)
      }


      formula <- as.formula(formula_string)








    }

  }else if(stratification){ # + strata
    if(model=="iid"){
      # pc.u = 1
      # pc.alpha = 0.01
      # formula <- value ~ 1 + f(sID, model = model,graph = admin.mat,
      #                                  hyper = list(prec = list(prior = "pc.prec",
      #                                                           param = c(pc.u , pc.alpha))))


      if(is.null(X)==FALSE){
        cvrt <- paste(" + ",colnames(X)[2:dim(X)[2]], collapse = "")
      }else{
        cvrt=NULL
      }
      sptl <- "+ f(sID, model = model, graph = admin.mat, hyper = list(prec = list(prior = \"pc.prec\", param = c(pc.u , pc.alpha))))"
      formula_string <- paste("value ~ 1 + strata", cvrt,sptl)
      formula <- as.formula(formula_string)


    }else if(model=="bym2"){

      # pc.u = 1
      # pc.alpha = 0.01
      # pc.u.phi <- 0.5
      # pc.alpha.phi <- 2/3
      # formula <- value ~ 1  + strata +
      #   f(sID, model = model, graph = admin.mat,
      #     hyper = list(
      #       prec = list(prior = "pc.prec", param = c(pc.u , pc.alpha)),
      #       phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))



      if(is.null(X)==FALSE){
        cvrt <- paste(" + ",colnames(X)[2:dim(X)[2]], collapse = "")
      }else{
        cvrt=NULL
      }

      if(nested){
        sptl <- "+ f(sID, model = model, graph = admin.mat,scale.model=T, constr=T, adjust.for.con.comp=T, hyper = list( prec = list(prior = \"pc.prec\", param = c(pc.u , pc.alpha)), phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))"
        formula_string <- paste("value ~ -1+ admin1.name + strata", cvrt,sptl)
      }else{
        sptl <- "+ f(sID, model = model, graph = admin.mat,  hyper = list( prec = list(prior = \"pc.prec\", param = c(pc.u , pc.alpha)), phi = list(prior = 'pc', param = c(pc.u.phi , pc.alpha.phi))))"
        formula_string <- paste("value ~ 1+ strata", cvrt,sptl)
      }

      formula <- as.formula(formula_string)



    }
  }
  if(stratification && sum(!unique(c.dat.tmp$strata) %in% c('urban', 'rural', NA))){
    stop("The variable strata in the input data can only be 'urban' or 'rural' currently.")
  }


  ## MODELING:
  overdisp.mean=overdisp.mean
  overdisp.prec=overdisp.prec
  control.family <- list(hyper = list(rho = list(param = c(overdisp.mean, overdisp.prec), initial = overdisp.mean)))


  imod<- INLA::inla(formula,
                    family="betabinomial",
                    data=c.dat.tmp,
                    Ntrials=n,
                    control.predictor = list(compute=TRUE, link = 1),
                    control.compute = list(config = TRUE,waic = TRUE),
                    control.family = control.family)


  nsamp <- 1000
  samp <- INLA::inla.posterior.sample(n = nsamp, result = imod, intern = TRUE)

  #draw posterior samples



  draw.u <- matrix(NA, nsamp, nregion)
  draw.r <- matrix(NA, nsamp, nregion)
  draw.all <- matrix(NA, nsamp, nregion)

  for(i in 1:length(samp)){
    tmp <- samp[[i]]$latent

    # step 1： random effect: any model has this
    s.effect <- data.frame(s.effect=tmp[paste0("sID:", 1:nregion), 1])
    s.effect$sID <- as.integer(sub("sID:(.*)", "\\1", rownames(s.effect)))
    l.com=left_join(admin_name_table,s.effect,by="sID")



    # step 2: intercept:  1 intercept when unnested, number of admin1 intercepts when nested
    if(nested){
      NN<- admin.info %>%
        group_by(admin1.name) %>%
        mutate(N = length(admin2.name.full)) %>%
        select(c("admin1.name","N")) %>%
        arrange(admin1.name)%>%
        distinct( admin1.name, .keep_all = TRUE)

      # n is the number of Admin 1 areas.
      n <- dim(NN)[1]

      # Set row names for fixed effect
      rows_to_extract <- paste0("admin1.name", NN$admin1.name, ":1")

      # Subset the data based on the calculated rows
      intercept.fix <- as.data.frame(tmp[rows_to_extract, , drop = FALSE])
      intercept.fix$admin1.name <- sub("admin1.name(.*):1", "\\1", rownames(intercept.fix))
      colnames(intercept.fix)[colnames(intercept.fix) == 'V1'] <- 'intercept'

      l.com=left_join(l.com,intercept.fix,by="admin1.name")


    }else{
      l.com$intercept <- tmp["(Intercept):1", 1]
    }

    # step 3 : covariates: 0 when no covariates
    if(is.null(X)==FALSE){
      covariates=as.matrix(X[,2:dim(X)[2]])%*% tail(tmp,n=(dim(X)[2]-1))[,1]
    }else{
      l.com$covariates=0
    }


    if(stratification){
    # step 4:  strata
    if("stratarural:1" %in% rownames(tmp)){
      str.effect <- tmp["stratarural:1", 1]
      str.effect.u <- 0
    }else{
      str.effect <- 0
      str.effect.u <- tmp["strataurban:1", 1]
    }
     }

    if(stratification==FALSE){
      draw.all[i, ] <- SUMMER::expit(l.com$s.effect + l.com$intercept+l.com$covariates)

    }else if(stratification){

      draw.u[i, ] <- SUMMER::expit(l.com$s.effect +  l.com$intercept + str.effect.u +l.com$covariates)
      draw.r[i, ] <- SUMMER::expit(l.com$s.effect +  l.com$intercept + str.effect + l.com$covariates)
      draw.all[i, ] <- draw.u[i, ] * admin.info$urban +
        draw.r[i, ] * (1 - admin.info$urban)
    }
  }




  #aggregation


  if(admin==1){
    if(stratification==F){
      admin1.bb.res<- data.frame(cbind(
        admin1.name=admin.info$admin1.name[tail(c.dat.tmp$sID,n=nregion)],
        mean= tail(imod$summary.fitted.values,n=nregion)[,1],
        median=colMedians(draw.all),
       sd= tail(imod$summary.fitted.values,n=nregion)[,2],
        var = tail(imod$summary.fitted.values,n=nregion)[,2]^2,
        lower= tail(imod$summary.fitted.values,n=nregion)[,3],
        upper= tail(imod$summary.fitted.values,n=nregion)[,5]))

      admin1.bb.res$mean<-as.numeric (admin1.bb.res$mean)
      admin1.bb.res$median<-as.numeric (admin1.bb.res$median)
      admin1.bb.res$sd<-as.numeric (admin1.bb.res$sd)
      admin1.bb.res$var<-admin1.bb.res$sd^2
      admin1.bb.res$lower<-as.numeric (admin1.bb.res$lower)
      admin1.bb.res$upper<-as.numeric (admin1.bb.res$upper)
      admin1.bb.res$cv=admin1.bb.res$sd/admin1.bb.res$mean

    }else if(stratification){
      post.u <- apply(draw.u, 2, mean)
      post.r <- apply(draw.r, 2, mean)
      post.all <- apply(draw.all, 2, mean)

      post.u.sd <- apply(draw.u, 2, sd)
      post.r.sd <- apply(draw.r, 2, sd)
      post.all.sd <- apply(draw.all, 2, sd)

      post.u.median <- apply(draw.u, 2, median)
      post.r.median<- apply(draw.r, 2, median)
      post.all.median <- apply(draw.all, 2, median)


      post.u.ci <- apply(draw.u, 2, quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
      post.r.ci <- apply(draw.r, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
      post.all.ci <- apply(draw.all, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))

      admin1.bb.res <- data.frame(
                                  admin1.name=rep(admin.info$admin1.name, 3),
                                  mean = c(post.u, post.r, post.all),
                                  median = c(post.u.median, post.r.median, post.all.median),
                                  sd= c(post.u.sd, post.r.sd, post.all.sd),
                                  var = c(post.u.sd^2, post.r.sd^2, post.all.sd^2),
                                  lower=c(post.u.ci[1,], post.r.ci[1,], post.all.ci[1,]),
                                  upper=c(post.u.ci[2,], post.r.ci[2,], post.all.ci[2,]),
                                  cv=c(post.u.sd/post.u,post.r.sd/post.r,post.all.sd/post.all),
                                  type = c(rep("urban", nregion), rep("rural", nregion),
                                           rep("full", nregion))
                                  )

    }
    #admin.bb.res

    #agg.nationl

    if(aggregation==T){
    post.all <- draw.all%*% admin.info$population/sum(admin.info$population)
    agg.natl <- data.frame(mean = mean(post.all),
                           median = median(post.all),
                          sd= sd(post.all),
                           var = var(post.all),
                           lower=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[1],
                           upper=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[2])
    agg.natl$cv=agg.natl$sd/agg.natl$mean
    rownames(agg.natl)=NULL

}

    if(aggregation==F){

      # return(list(res.admin1=admin1.bb.res,inla=imod ))

      cm=list(res.admin1=admin1.bb.res,inla=imod,admin1_post=draw.all,urban_post=draw.u,rural_post=draw.r)
      attr(cm,"class")="clusterModel"
      attr(cm,"domain.names") <- admin.info$admin1.name
      return(cm)



    }else{

      # return(list(res.admin1=admin1.bb.res,agg.natl=agg.natl,inla=imod,
      #             admin1_post=draw.all,nation_post=post.all))
      #
      cm=list(res.admin1=admin1.bb.res,agg.natl=agg.natl,inla=imod,
          admin1_post=draw.all,nation_post=post.all,urban_post=draw.u,rural_post=draw.r)
      attr(cm,"class")="clusterModel"
      attr(cm,"domain.names") <- admin.info$admin1.name
      return(cm)

    }
  }else if(admin==2){

    if(stratification==F){

      admin2.bb.res<- data.frame(cbind(
      admin2.name.full=admin.info$admin2.name.full[tail(c.dat.tmp$sID,n=nregion)],
        mean= tail(imod$summary.fitted.values,n=nregion)[,1],
        median=colMedians(draw.all),
       sd= tail(imod$summary.fitted.values,n=nregion)[,2],
        var = tail(imod$summary.fitted.values,n=nregion)[,2]^2,
        lower= tail(imod$summary.fitted.values,n=nregion)[,3],
        upper= tail(imod$summary.fitted.values,n=nregion)[,5]))

      admin2.bb.res$mean<-as.numeric (admin2.bb.res$mean)
      admin2.bb.res$median<-as.numeric (admin2.bb.res$median)
      admin2.bb.res$sd<-as.numeric (admin2.bb.res$sd)
      admin2.bb.res$var <- admin2.bb.res$sd^2
      admin2.bb.res$lower<-as.numeric (admin2.bb.res$lower)
      admin2.bb.res$upper<-as.numeric (admin2.bb.res$upper)
      admin2.bb.res$cv=admin2.bb.res$sd/admin2.bb.res$mean

      admin2.bb.res<-left_join(admin2.bb.res,distinct(admin.info),by="admin2.name.full")

    }else if(stratification){
      post.u <- apply(draw.u, 2, mean)
      post.r <- apply(draw.r, 2, mean)
      post.all <- apply(draw.all, 2, mean)

      post.u.sd <- apply(draw.u, 2, sd)
      post.r.sd <- apply(draw.r, 2, sd)
      post.all.sd <- apply(draw.all, 2, sd)

      post.u.median <- apply(draw.u, 2, median)
      post.r.median<- apply(draw.r, 2, median)
      post.all.median <- apply(draw.all, 2, median)

      post.u.ci <- apply(draw.u, 2, quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
      post.r.ci <- apply(draw.r, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
      post.all.ci <- apply(draw.all, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))

      admin2.bb.res <- data.frame(admin2.name.full= rep(admin.info$admin2.name.full, 3),
        mean = c(post.u, post.r, post.all),
                                  median = c(post.u.median, post.r.median, post.all.median),
                                 sd= c(post.u.sd, post.r.sd, post.all.sd),
                                  var = c(post.u.sd^2, post.r.sd^2, post.all.sd^2),
                                  lower=c(post.u.ci[1,], post.r.ci[1,], post.all.ci[1,]),
                                  upper=c(post.u.ci[2,], post.r.ci[2,], post.all.ci[2,]),
                                  cv=c(post.u.sd/post.u,post.r.sd/post.r,post.all.sd/post.all),
                                  type = c(rep("urban", nregion), rep("rural", nregion),
                                           rep("full", nregion)))
      admin2.bb.res<-left_join(admin2.bb.res,distinct(admin.info),by="admin2.name.full")

    }



    if(aggregation==T){
    #agg admin1
    weight=admin.info$population/admin.info$population.admin1

    # post.all <-  data.table(weight*draw.all)
    post.all <-  data.table::data.table(t(weight*t(draw.all)))#nrow=legth(weight)
    colnames(post.all) <- admin.info$admin1.name
    subgroups<-split.default(post.all, names(post.all))

    sums_list <- lapply(subgroups, function(subgroup) {
      rowSums(subgroup)
    })
    admin1.samp <- do.call(cbind, sums_list)


    agg.admin1 <- data.frame(
                             mean = colMeans(admin1.samp),
                             median=colMedians(admin1.samp),
                             sd= apply(admin1.samp, 2, sd),
                             var =  apply(admin1.samp, 2, var),
                             lower= apply(admin1.samp, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[1,],
                             upper= apply(admin1.samp, 2,  quantile, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[2,],
                             cv=apply(admin1.samp, 2, sd)/colMeans(admin1.samp)
                            )
    agg.admin1$admin1.name=rownames(agg.admin1)
    rownames(agg.admin1)=NULL
    agg.admin1 <- agg.admin1 %>% select("admin1.name","mean", "median","sd","var","lower","upper","cv")

    #agg national
    unique( admin.info$population.admin1)/sum(unique( admin.info$population.admin1))

    post.all <- admin1.samp%*% unique( admin.info$population.admin1)/sum(unique( admin.info$population.admin1))
    agg.natl <- data.frame(mean = mean(post.all),
                           median=median(post.all),
                           sd= sd(post.all),
                           var = var(post.all),
                           lower=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[1],
                           upper=quantile(post.all, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))[2])
     agg.natl$cv=agg.natl$sd/agg.natl$mean
     rownames(agg.natl)=NULL

}


    # colnames(admin2.bb.res)[colnames(admin2.bb.res) == 'admin2.name.full'] <- 'admin2.name.full'


    if(aggregation==F){

      # return(list(res.admin2=admin2.bb.res, inla=imod,
      #             admin2_post=draw.all))

      cm=list(res.admin2=admin2.bb.res, inla=imod,
              admin2_post=draw.all,urban_post=draw.u,rural_post=draw.r)
      attr(cm,"class")="clusterModel"
      attr(cm,"domain.names") <- admin.info$admin2.name.full

      return(cm)



    }else{
#
#     return(list(res.admin2=admin2.bb.res,agg.admin1=agg.admin1,agg.natl=agg.natl,inla=imod,
#                 admin2_post=draw.all,admin1_post=admin1.samp,nation_post=post.all))
      cm=list(res.admin2=admin2.bb.res,agg.admin1=agg.admin1,agg.natl=agg.natl,inla=imod,
              admin2_post=draw.all,admin1_post=admin1.samp,nation_post=post.all,urban_post=draw.u,rural_post=draw.r)
      attr(cm,"class")="clusterModel"
      attr(cm,"domain.names") <- admin.info$admin2.name.full

      return(cm)


}

  }






}
