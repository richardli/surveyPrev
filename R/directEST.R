#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param data dataframe that contains the indicator of interests, output of getDHSindicator function
#' @param cluster.info list contains data and wrong.points. data contains admin 1 and admin 2 information and coordinates for each cluster. wrong.points. contains cluster id for cluster without coordinates or admin 1 information. Output of getDHSindicator function
#' @param admin.info list contains data and mat, data contains population and urban/rural proportion at specific admin level and mat is the adjacency matrix, output of adminInfo function
#' @param admin admin level for the model.
#' @param strata use only urban or rural data, only for national level model
#' @param CI Credible interval to be used. Default to 0.95.
#' @param weight the weight used for aggregating result, "population" or "survey"
#' @param aggregation whether or not report aggregation results.
#' @param alt.strata the variable name in the data frame that correspond to the stratification variable. Most of the DHS surveys are stratified by admin 1 area crossed with urban/rural, which is the default stratification variable created by the function (when \code{alt.strata = NULL}). When a different set of strata is used. The stratification variable should be included in the data and \code{alt.strata} should be set to the column name.
#' @param var.fix Whether to add phantom cluster to fix admin 2 direct estimate with variance close to 0.
#' @param threshold the threshold of variance to implement the variance fix method.
#' @param ... Additional arguments passed on to the `smoothSurvey` function
#'
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,

#' @import dplyr
#' @importFrom SUMMER smoothSurvey expit
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#'
#' ##
#' ## Direct estimation of ANC visit 4+ proportion
#' ##
#'
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' data(ZambiaPopWomen)
#' cluster.info<-clusterInfo(geo=geo, poly.adm1=ZambiaAdm1, poly.adm2=ZambiaAdm2,
#' by.adm1 = "NAME_1",by.adm2 = "NAME_2")
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#'
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4+")
#' res_ad1 <- directEST(data = data,
#'                   cluster.info = cluster.info,
#'                   admin = 1,
#'                   aggregation = FALSE)
#' res_ad1
#' # compare with the DHS direct estimates
#' dhs_table <- get_api_table(country = "ZM",
#'                            survey = "ZM2018DHS",
#'                            indicator = "RH_ANCN_W_N4P",
#'                            simplify = TRUE)
#' subset(dhs_table, ByVariableLabel == "Five years preceding the survey")
#'
#' ##
#' ## Changing customized stratification variable
#' ##
#'
#' data_alt <- data
#' # Assuming the stratification is done with only admin1 area
#' # and not stratified by urban and rural
#' # Note that this is not the correct stratification, but we use
#' #  this as an illustration to create user-specified strata variable
#' data_alt$new_strata <- data_alt$v024
#' res_ad1_wrong <- directEST(data = data_alt,
#'                   cluster.info = cluster.info,
#'                   admin = 1,
#'                   aggregation = FALSE,
#'                   alt.strata = "new_strata")
#' res_ad1_wrong
#' }
#'
#' @export

directEST <- function(data, cluster.info, admin, strata="all", CI = 0.95, weight = c("population", "survey")[1], admin.info = NULL, aggregation = FALSE, alt.strata = NULL,var.fix = FALSE, threshold=1e-12, ...){


  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")

  if(!is.null(admin.info)){
    admin.info.output=admin.info
  }else{
    admin.info.output=NULL
  }


 if(sum(is.na(data$value))>0){
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }

 if(!is.null(admin.info)){
   admin.info=admin.info$data
 }

  if(admin==2){
    if(strata != "all"){
      message("Subnational stratum-specific direct estimates are not implemented yet. Only overall estimates are computed")
    }
    #prepare data
    modt<- dplyr::left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$admin2.name)), ]
    modt$strata.full <- factor(paste(modt$admin1.name, modt$strata))
    if(!is.null(alt.strata)){
        modt$strata.full <- factor(modt[, alt.strata])
    }
    modt<-  modt[order(modt$admin1.name,modt$admin2.name), ]

    smoothSurvey_res<-SUMMER::smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "admin2.name.full",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = CI,
                                   is.unit.level=FALSE,
                                   smooth=FALSE,...)
    admin2_res<-as.data.frame(smoothSurvey_res$HT)
    admin2_res$direct.se<-sqrt(admin2_res$HT.var)

    colnames(admin2_res)[colnames(admin2_res) == 'region'] <- 'admin2.name.full' # region is the col name in smoothSurvey_res$HT

    colnames(admin2_res)[colnames(admin2_res) == 'HT.est'] <- 'direct.est'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.var'] <- 'direct.var'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.logit.est'] <- 'direct.logit.est'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.logit.var'] <- 'direct.logit.var'
    colnames(admin2_res)[colnames(admin2_res) == 'HT.logit.prec'] <- 'direct.logit.prec'


    admin2_res$direct.lower <- expit(admin2_res$direct.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin2_res$direct.logit.var))
    admin2_res$direct.upper <- expit(admin2_res$direct.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin2_res$direct.logit.var))
    admin2_res$cv.mean <- admin2_res$direct.se / admin2_res$direct.est
    admin2_res$cv.star <- pmax(admin2_res$direct.se/ admin2_res$direct.est, admin2_res$direct.se/ (1-admin2_res$direct.est) )


    a<-strsplit(admin2_res$admin2.name.full,"_")
    admin2_res$admin2.name<-matrix(unlist(a),ncol =2, byrow =T)[,2]#needed in mapplot()
    admin2_res$admin1.name<-matrix(unlist(a),ncol =2, byrow =T)[,1]


    res.admin2=admin2_res
    fixed_areas=c()


    ################## Variance Fix ##################
    if (var.fix == TRUE) {
      # =========================
      # 1) Preparation: National stratum Hajék prior means, Admin1×stratum average cluster weights
      # =========================

      # === MOD: unify strata and use actual strata levels ===
      modt <- modt %>%
        dplyr::mutate(strata = tolower(trimws(strata)))
      strata_levels <- sort(unique(modt$strata))

      # National stratum Hajék (phantom prior mean p_h_nat)
      nat_stratum_hj <- modt %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(
          num = sum(weight * value, na.rm = TRUE),
          den = sum(weight,         na.rm = TRUE),
          p_h_nat = dplyr::if_else(den > 0, num/den, NA_real_),
          .groups = "drop"
        ) %>%
        tidyr::complete(strata = strata_levels) %>%         # === MOD
        tidyr::replace_na(list(p_h_nat = 0.5)) %>%          # tolerance
        dplyr::select(strata, p_h_nat)

      # Total cluster weight v_hc· = sum_k weight
      tw_by_cluster <- modt %>%
        dplyr::group_by(admin1.name, strata, cluster) %>%
        dplyr::summarise(v_hc_dot = sum(weight, na.rm = TRUE), .groups = "drop")

      # Admin1×stratum average cluster weight (phantom v_ph = tw_ph)
      avg_tw_admin1_stratum <- tw_by_cluster %>%
        dplyr::group_by(admin1.name, strata) %>%
        dplyr::summarise(tw_ph = mean(v_hc_dot, na.rm = TRUE), .groups = "drop")

      # National stratum average cluster weight (fallback)
      avg_tw_nat_stratum <- tw_by_cluster %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(tw_ph_nat = mean(v_hc_dot, na.rm = TRUE), .groups = "drop")

      # Robust admin2 -> admin1 mapping
      ad2_admin1_from_modt <- modt %>%
        dplyr::distinct(admin2.name.full, admin1.name)

      ad2_admin1_fallback <- admin2_res %>%
        dplyr::transmute(
          admin2.name.full,
          admin1.name_fallback = sub("_.*$", "", admin2.name.full)
        )

      ad2_admin1 <- ad2_admin1_from_modt %>%
        dplyr::full_join(ad2_admin1_fallback, by = "admin2.name.full") %>%
        dplyr::mutate(admin1.name = dplyr::coalesce(admin1.name, admin1.name_fallback)) %>%
        dplyr::select(admin2.name.full, admin1.name)

      # Key table: prepare (p_h_nat, tw_ph) for each admin2 × strata
      key <- admin2_res %>%
        dplyr::select(admin2.name.full) %>%
        dplyr::left_join(ad2_admin1, by = "admin2.name.full") %>%
        tidyr::crossing(strata = strata_levels) %>%                            # === MOD
        dplyr::left_join(nat_stratum_hj,        by = "strata") %>%             # p_h_nat
        dplyr::left_join(avg_tw_admin1_stratum, by = c("admin1.name","strata")) %>%
        dplyr::left_join(avg_tw_nat_stratum,    by = "strata") %>%
        dplyr::mutate(
          tw_ph  = dplyr::coalesce(tw_ph,  tw_ph_nat),
          p_h_nat= dplyr::coalesce(p_h_nat, 0.5)
        ) %>%
        dplyr::select(admin2.name.full, admin1.name, strata, p_h_nat, tw_ph)

      # =========================
      # 2) Core: Unnormalized implementation of formula (9)
      # =========================
      phantom_fix_admin2_exact <- function(dat_one_adm2, key_rows){
        # 2.1 Each real cluster’s v_hc· and p_hc (Hajék)
        clus <- dat_one_adm2 %>%
          dplyr::group_by(strata, cluster) %>%
          dplyr::summarise(
            v_hc_dot = sum(weight, na.rm = TRUE),
            y_w      = sum(weight * value, na.rm = TRUE),
            p_hc     = dplyr::if_else(v_hc_dot > 0, y_w / v_hc_dot, 0),
            .groups  = "drop"
          )

        # 2.2 Stratum totals
        layer_real <- clus %>%
          dplyr::group_by(strata) %>%
          dplyr::summarise(
            n_h         = dplyr::n(),
            v_h_dotdot  = sum(v_hc_dot, na.rm = TRUE),
            num_h       = sum(v_hc_dot * p_hc, na.rm = TRUE),
            .groups     = "drop"
          )

        # 2.3 Merge phantom parameters
        layer <- key_rows %>%
          dplyr::left_join(layer_real, by = "strata") %>%
          dplyr::mutate(
            n_h            = dplyr::coalesce(n_h, 0L),
            v_h_dotdot     = dplyr::coalesce(v_h_dotdot, 0),
            num_h          = dplyr::coalesce(num_h, 0),
            n_h_aug        = n_h + 1L,
            v_h_dotdot_aug = v_h_dotdot + tw_ph,
            num_h_aug      = num_h + tw_ph * p_h_nat,
            p_h_aug        = dplyr::if_else(v_h_dotdot_aug > 0, num_h_aug / v_h_dotdot_aug, 0)
          )

        # === MOD: Sum with na.rm to avoid NA propagation ===
        v_dotdotdot_aug <- sum(layer$v_h_dotdot_aug, na.rm = TRUE)
        p_aug <- if (v_dotdotdot_aug > 0) {
          sum(layer$v_h_dotdot_aug * layer$p_h_aug, na.rm = TRUE) / v_dotdotdot_aug
        } else 0

        join_cols <- dplyr::select(layer, strata, n_h_aug, v_h_dotdot_aug, p_h_aug)

        clus_aug_real <- clus %>%
          dplyr::left_join(join_cols, by = "strata") %>%
          dplyr::mutate(
            B_c  = .data$v_hc_dot * (.data$p_hc - p_aug) -
              (1 / .data$n_h_aug) * .data$v_h_dotdot_aug * (.data$p_h_aug - p_aug),
            term = B_c^2
          )

        clus_aug_ph <- layer %>%
          dplyr::transmute(
            strata, n_h_aug, v_h_dotdot_aug,
            v_hc_dot = tw_ph,
            p_hc     = p_h_nat,
            p_h_aug  = p_h_aug
          ) %>%
          dplyr::mutate(
            B_c  = .data$v_hc_dot * (.data$p_hc - p_aug) -
              (1 / .data$n_h_aug) * .data$v_h_dotdot_aug * (.data$p_h_aug - p_aug),
            term = B_c^2
          )

        var_h <- dplyr::bind_rows(clus_aug_real, clus_aug_ph) %>%
          dplyr::group_by(strata, n_h_aug) %>%
          dplyr::summarise(sum_term = sum(.data$term, na.rm = TRUE), .groups = "drop") %>%
          dplyr::mutate(v_h = dplyr::if_else(.data$n_h_aug > 1,
                                             (.data$n_h_aug / (.data$n_h_aug - 1)) * .data$sum_term, 0))

        var_aug <- if (v_dotdotdot_aug > 0) sum(var_h$v_h, na.rm = TRUE) / (v_dotdotdot_aug^2) else 0
        list(p_aug = p_aug, var_aug = max(var_aug, 0))
      }

      # =========================
      # 3) Apply fix for each Admin2: broader trigger + lower bound protection
      # =========================
      eps <- threshold          # === MOD: very small threshold
      floor_var <- 1e-12    # === MOD: variance floor (avoid writing as 0)

      admin_areas <- admin2_res$admin2.name.full

      for (i in admin_areas) {
        dat_tmp <- modt %>% dplyr::filter(admin2.name.full == i, !is.na(value))
        v_raw   <- admin2_res$direct.var[admin2_res$admin2.name.full == i]

        # === MOD: broader trigger condition
        if (!is.finite(v_raw) || v_raw < eps) {

          fixed_areas <- c(fixed_areas, i)


          key_rows <- key %>% dplyr::filter(admin2.name.full == i)
          if (nrow(key_rows) == 0) {
            # Fallback: national stratum tw and p_h_nat
            admin1_i <- (modt %>% dplyr::filter(admin2.name.full == i) %>%
                           dplyr::pull(admin1.name) %>% unique())[1]
            key_rows <- tibble::tibble(
              admin2.name.full = i,
              admin1.name = admin1_i,
              strata = strata_levels
            ) %>%
              dplyr::left_join(nat_stratum_hj,     by = "strata") %>%
              dplyr::left_join(avg_tw_nat_stratum, by = "strata") %>%
              dplyr::mutate(tw_ph = tw_ph_nat)
          }

          aug <- phantom_fix_admin2_exact(dat_one_adm2 = dat_tmp, key_rows = key_rows)

          p.i <- aug$p_aug
          v.i <- aug$var_aug

          # === MOD: prevent replacing tiny positive variance with 0; fallback if estimation fails
          if (!is.finite(p.i)) {
            p.i <- with(dat_tmp, sum(weight * value, na.rm = TRUE) / sum(weight, na.rm = TRUE))
          }
          if (!is.finite(v.i) || v.i <= eps) {
            v.i <- if (is.finite(v_raw) && v_raw > eps) v_raw else floor_var
          }

          res.admin2$direct.est [res.admin2$admin2.name.full == i] <- p.i
          res.admin2$direct.var [res.admin2$admin2.name.full == i] <- v.i
          res.admin2$direct.se  [res.admin2$admin2.name.full == i] <- sqrt(v.i)

          ht   <- qlogis(p.i)
          htv  <- if (p.i > 0 && p.i < 1) v.i / (p.i^2 * (1 - p.i)^2) else 0
          res.admin2$direct.logit.est [res.admin2$admin2.name.full == i] <- ifelse(is.finite(ht), ht, ifelse(p.i >= 1, 36, -36))
          res.admin2$direct.logit.var [res.admin2$admin2.name.full == i] <- htv
          res.admin2$direct.logit.prec[res.admin2$admin2.name.full == i] <- ifelse(htv > 0, 1/htv, 0)

          res.admin2$direct.lower <- expit(res.admin2$direct.logit.est +
                                             stats::qnorm((1 - CI)/2) * sqrt(res.admin2$direct.logit.var))
          res.admin2$direct.upper <- expit(res.admin2$direct.logit.est +
                                             stats::qnorm(1 - (1 - CI)/2) * sqrt(res.admin2$direct.logit.var))
          res.admin2$cv.mean <- res.admin2$direct.se / res.admin2$direct.est
          res.admin2$cv.star<- pmax(res.admin2$direct.se/ res.admin2$direct.est, res.admin2$direct.se/ (1-res.admin2$direct.est) )





        }
      }
    }

    #### message for aggregation=T but missing some components and return results without aggregation
    if(aggregation==FALSE){
    }else{
      if((is.null(admin.info)||sum(is.na(admin.info$population))>0)|| is.null(weight=="population")){
        message("Need population information for aggregation")
        aggregation=FALSE
      }

    }





    ####message for aggregation=T but missing some components and return results without aggregation
    if(aggregation==FALSE){
    }else{
      if((is.null(admin.info)||sum(is.na(admin.info$population))>0)|| is.null(weight=="population")){
        message("Need population information for aggregation")
        aggregation=FALSE
      }

    }

   if(aggregation==FALSE){


     ##aggregation
     # admin2_res<-na.omit(admin2_res)# exclude NA when weighted mean to admin1

     # make direct.logit.est to 36 or -36 for HT=1 or 0.

     admin2_res=res.admin2
     for (i in 1:dim(admin2_res)[1]) {

       if(is.na(admin2_res[i,]$direct.logit.est)&& round(admin2_res[i,]$direct.est,digits = 8)==1 ){
         admin2_res[i,]$direct.logit.est=36
       }
       if(is.na(admin2_res[i,]$direct.logit.est)&&admin2_res[i,]$direct.est==0 ){
         admin2_res[i,]$direct.logit.est=-36
       }

       if(is.na(admin2_res[i,]$direct.logit.var)){
         admin2_res[i,]$direct.logit.var=0
       }
     }


     dd=data.frame(admin2.name.full=admin2_res$admin2.name.full,value=admin2_res$direct.logit.est,sd=sqrt(admin2_res$direct.logit.var))   #dd$value has <0 bc it's direct.logit.est
     draw.all=  expit(apply(dd[,2:3], 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))
     colnames(draw.all)=admin2_res$admin2.name.full
     # colnames(res.admin2)[colnames(res.admin2) == 'admin2.name.full'] <- 'admin2.name.full'
     res.admin2=list(res.admin2=res.admin2,
                    admin2_post= draw.all,
                    admin.info=admin.info.output,
                    fixed_areas=fixed_areas,
                    admin=admin)
   }else{

     admin2_res=res.admin2

        ##aggregation
        # admin2_res<-na.omit(admin2_res)# exclude NA when weighted mean to admin1

     # make direct.logit.est to 36 or -36 for HT=1 or 0.
        for (i in 1:dim(admin2_res)[1]) {

          if(is.na(admin2_res[i,]$direct.logit.est)&& round(admin2_res[i,]$direct.est,digits = 8)==1 ){
            admin2_res[i,]$direct.logit.est=36
          }
          if(is.na(admin2_res[i,]$direct.logit.est)&&admin2_res[i,]$direct.est==0 ){
            admin2_res[i,]$direct.logit.est=-36
          }

          if(is.na(admin2_res[i,]$direct.logit.var)){
            admin2_res[i,]$direct.logit.var=0
          }
}


        dd=data.frame(admin2.name.full=admin2_res$admin2.name.full,value=admin2_res$direct.logit.est,sd=sqrt(admin2_res$direct.logit.var))   #dd$value has <0 bc it's direct.logit.est
        draw.all=  expit(apply(dd[,2:3], 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))
        colnames(draw.all)=admin2_res$admin2.name.full

        ##
        ## TODO: Similar to above, using distinct() can create problems. Instead, use both admin1 and admin2 names to join the two dataset.
        ## 1/8/24: joined use admin2.name.full which is admin1_admin2


        ##If J-th admin2 nested within the i-th admin 1, and the k-th region has no data,
        ##the admin 1 estimate is $p_i = \sum_{j\neq k}^J p_{ij} * n_{ij} /\sum_{j\neq k}^J n_{ij} )$.
        ##And the national estimate is p = $p_i *(\sum_{j=1}^J n_{ij} )/ n$ + other admin1 estimates weighted by the admin1 pop fraction)

        ### ### ### ### ### ### ### ### ### ###
        ### admin2 to admin1 for admin2 result
        ### ### ### ### ### ### ### ### ### ###

        ####aggregation for variance
        if(weight=="population"){
          #weight using worldpop
          weight_dt<-left_join(dd, distinct(admin.info), by="admin2.name.full")%>%
            group_by(admin1.name)%>%
            mutate(prop=round(population/sum(population),digits = 4))
        }else{
          #weight using dhs sampling weight (modt$weight
          weight_dt<- modt%>%group_by(admin2.name.full)%>%
            mutate(sumweight2=sum(weight),digits = 4)%>%
            distinct(admin2.name.full,sumweight2,admin1.name,admin2.name)%>%
            group_by(admin1.name)%>%
            mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))%>%
            left_join(dd, by="admin2.name.full")
        }
        weight_dt <- weight_dt[match(admin2_res$admin2.name.full, weight_dt$admin2.name.full), ]



       admin1.list <- sort(unique(weight_dt$admin1.name))
       admin1.samp <- matrix(NA, 10000, length(admin1.list))
       for(i in 1:length(admin1.list)){
          which.admin2 <- which(weight_dt$admin1.name == admin1.list[i])
          admin1.samp[, i] <- apply(draw.all[, which.admin2, drop = FALSE], 1, function(x, w){sum(x * w)}, weight_dt$prop[which.admin2])
       }
       colnames(admin1.samp) <- admin1.list



       ## aggregation for mean
       if(weight=="population"){
         #weight using worldpop
         weight_dt_mean<-left_join(admin2_res[,c("admin2.name.full","direct.est")],distinct(admin.info), by="admin2.name.full")%>%
           group_by(admin1.name)%>%
           mutate(prop=round(population/sum(population),digits = 4))%>%
           mutate(value1=prop*direct.est)
       }else{
         #weight using dhs sampling weight (modt$weight
         weight_dt_mean<- modt%>%group_by(admin2.name.full)%>%
           mutate(sumweight2=sum(weight),digits = 4)%>%
           distinct(admin2.name.full,sumweight2,admin1.name,admin2.name)%>%
           group_by(admin1.name)%>%
           mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))%>%
           left_join(admin2_res[,c("admin2.name.full","direct.est")], by="admin2.name.full")%>%
           mutate(value1=prop*direct.est)

       }



       admin1_agg <- data.frame(admin1.name= colnames(admin1.samp),
                                direct.se =  apply(admin1.samp, 2, sd),
                                direct.lower= apply(admin1.samp, 2,  quantile, probs = c((1 - CI)/2, 1 - (1 - CI)/2))[1,],
                                direct.upper= apply(admin1.samp, 2,  quantile, probs = c((1 - CI)/2, 1 - (1 - CI)/2))[2,]
       )

       admin1_agg <- admin1_agg%>% left_join( aggregate(value1 ~ admin1.name, data = weight_dt_mean, sum), by="admin1.name")%>%
       rename( direct.est = value1)#admin1_agg: admin2toadmin1 result

       admin1_agg <- admin1_agg[, c("admin1.name", "direct.est", "direct.se", "direct.lower", "direct.upper")]


       ### ### ### ### ### ### ### ### ### ###
       ### admin1 to national for admin2 result
       ### ### ### ### ### ### ### ### ### ###

       if(weight=="population"){
           #for variance
           admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin1.name, population=admin.info$population.admin1))
           weight_dt=admin1.distinct$population[match(colnames(admin1.samp), admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
           nation.samp<- admin1.samp%*%weight_dt

           #for mean
           weight_dt_mean<-weight_dt%*%admin1_agg$direct.est

       }else{
         # admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin1.name, population=admin.info$population.admin1))
         # weight_dt=admin1.distinct$population[match(colnames(admin1.samp), admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
         weight_dt<- modt%>%group_by(admin1.name)%>%
           mutate(sumweight2=sum(weight),digits = 4)%>%
           distinct(admin1.name,sumweight2)%>%
           ungroup()%>%
           mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))


         nation.samp<- admin1.samp%*%weight_dt$prop  #for variance
         weight_dt_mean<-weight_dt$prop%*%admin1_agg$direct.est #for mean

       }


       nation_agg <- data.frame(
         # admin0.name="country",
                               direct.est=weight_dt_mean,
                                #meanfromsample =mean(nation.samp),
                               direct.se = sd(nation.samp),
                               direct.var = var(nation.samp),
                               direct.lower=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[1],
                               direct.upper=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[2])

       nation_agg$cv.mean=nation_agg$direct.se/nation_agg$direct.est
       nation_agg$cv.star=pmax(nation_agg$direct.se/ nation_agg$direct.est, nation_agg$direct.se/ (1-nation_agg$direct.est) )


       #cleaning up colnames
       # colnames(res.admin2)[colnames(res.admin2) == 'admin2.name.full'] <- 'admin2.name.full'


       res.admin2<-list(res.admin2=res.admin2,
                        agg.admin1=admin1_agg,
                        agg.natl=nation_agg,
                        admin2_post=draw.all,
                        admin1_post=admin1.samp,
                        nation_post=nation.samp,
                        admin.info=admin.info.output,
                        fixed_areas=fixed_areas,
                        admin=admin
                        )

   }

    attr(res.admin2,"class")="directEST"
    attr(res.admin2,"domain.names") <- admin.info$admin2.name.full
    return(res.admin2)

  }else if(admin==1){

    if(strata != "all"){
      message("Subnational stratum-specific direct estimates are not implemented yet. Only overall estimates are computed")
    }

    modt<- left_join(data,cluster.info$data,by="cluster")
    modt<- modt[!(is.na(modt$admin1.name)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
     if(!is.null(alt.strata)){
        modt$strata.full <- factor(modt[, alt.strata])
    }
    modt<-  modt[order(modt$admin1.name), ]

    # model
    # clusterVar = "~cluster+householdID"
    # design <- survey::svydesign(ids = stats::formula(clusterVar),
    #                             weights = ~weight , data = modt,
    #                             strata=~strata)
    # admin1_res <- survey::svyby(formula = ~value, by = ~admin1.name,
    # design = design, survey::svymean, drop.empty.groups = FALSE)

  #  aggregate results

       smoothSurvey_res<-smoothSurvey(as.data.frame(modt),
                 responseType ="binary",
                 responseVar= "value",
                 regionVar = "admin1.name",
                 clusterVar = "~cluster+householdID",
                 weightVar = "weight",
                 strataVar = "strata.full",
                 Amat =NULL,
                 CI = CI,
                 is.unit.level=FALSE,
                 smooth=FALSE, ...)

    admin1_res<-smoothSurvey_res$HT
    admin1_res$direct.se<-sqrt(admin1_res$HT.var)


    colnames(admin1_res)[colnames(admin1_res) == 'region'] <- 'admin1.name'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.est'] <- 'direct.est'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.var'] <- 'direct.var'

    colnames(admin1_res)[colnames(admin1_res) == 'HT.logit.est'] <- 'direct.logit.est'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.logit.var'] <- 'direct.logit.var'
    colnames(admin1_res)[colnames(admin1_res) == 'HT.logit.prec'] <- 'direct.logit.prec'


    admin1_res$direct.lower <- expit(admin1_res$direct.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin1_res$direct.logit.var))
    admin1_res$direct.upper <- expit(admin1_res$direct.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin1_res$direct.logit.var))
    admin1_res$cv.mean=admin1_res$direct.se/admin1_res$direct.est
    admin1_res$cv.star=pmax(admin1_res$direct.se/ admin1_res$direct.est, admin1_res$direct.se/ (1-admin1_res$direct.est) )
    res.admin1=admin1_res


    ####message for aggregation=T but missing some components and return results without aggregation
    if(aggregation==FALSE){
    }else{
      if((is.null(admin.info)||sum(is.na(admin.info$population))>0)& is.null(weight=="population") ){
        message("Need population information for aggregation")
        aggregation=FALSE
      }
    }


    if(aggregation==FALSE){


      for (i in 1:dim(admin1_res)[1]) {

        if(is.na(admin1_res[i,]$direct.logit.est)&& round(admin1_res[i,]$direct.est,digits = 8)==1 ){
          admin1_res[i,]$direct.logit.est=36
        }
        if(is.na(admin1_res[i,]$direct.logit.est)&&admin1_res[i,]$direct.est==0 ){
          admin1_res[i,]$direct.logit.est=-36
        }

        if(is.na(admin1_res[i,]$direct.logit.var)){
          admin1_res[i,]$direct.logit.var=0
        }
      }

      dd=data.frame(mean=admin1_res$direct.logit.est,sd=sqrt(admin1_res$direct.logit.var))
      draw.all= expit(apply(dd, 1, FUN = function(x) rnorm(10000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))
      colnames(draw.all)=admin1_res$admin1.name

      res.admin1=list(res.admin1=res.admin1,
                      admin1_post=draw.all,
                      admin.info=admin.info.output,
                      admin=admin)

    }else{


      ### ### ### ### ### ### ### ### ### ###
      ### admin1 to national for admin1 result
      ### ### ### ### ### ### ### ### ### ###

     # make direct.logit.est to 36 or -36 for HT=1 or 0.

      for (i in 1:dim(admin1_res)[1]) {

        if(is.na(admin1_res[i,]$direct.logit.est)&& round(admin1_res[i,]$direct.est,digits = 8)==1 ){
          admin1_res[i,]$direct.logit.est=36
        }
        if(is.na(admin1_res[i,]$direct.logit.est)&&admin1_res[i,]$direct.est==0 ){
          admin1_res[i,]$direct.logit.est=-36
        }

        if(is.na(admin1_res[i,]$direct.logit.var)){
          admin1_res[i,]$direct.logit.var=0
        }
      }

    dd=data.frame(mean=admin1_res$direct.logit.est,sd=sqrt(admin1_res$direct.logit.var))
    draw.all= expit(apply(dd, 1, FUN = function(x) rnorm(1000, mean = x[1], sd = x[2]))) # sqrt(colVars(draw.all))
    colnames(draw.all)=admin1_res$admin1.name

   if(weight=="population"){
      #for variance
      admin1.distinct=distinct(data.frame(admin1.name=admin.info$admin1.name, population=admin.info$population))
      weight_dt=admin1.distinct$population[match(admin1_res$admin1.name, admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
      nation.samp<- draw.all%*%weight_dt
      #for mean
      weight_dt_mean<-weight_dt%*%admin1_res$direct.est


    }else{
      weight_dt<- modt%>%group_by(admin1.name)%>%
        mutate(sumweight2=sum(weight),digits = 4)%>%
        distinct(admin1.name,sumweight2)%>%
        ungroup()%>%
        mutate(prop=round(sumweight2/sum(sumweight2),digits = 4))

      nation.samp<- draw.all%*%weight_dt$prop  #for variance
      weight_dt_mean<-weight_dt$prop%*%admin1_res$direct.est #for mean

    }

    nation_agg <- data.frame(
                             # admin1.name= "country",
                             direct.est=weight_dt_mean,
                             # meanFROMsample =mean(nation.samp),
                             direct.se = sd(nation.samp),
                             direct.var = var(nation.samp),

                             direct.lower=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[1],
                             direct.upper=quantile(nation.samp, probs = c((1 - CI)/2,1 - (1 - CI)/2))[2])

    nation_agg$cv.mean=nation_agg$direct.se/nation_agg$direct.est
    nation_agg$cv.star=pmax(nation_agg$direct.se/ nation_agg$direct.est, nation_agg$direct.se/ (1-nation_agg$direct.est) )

    res.admin1 = (list(res.admin1 = res.admin1,
                       agg.natl = nation_agg,
                       admin1_post=draw.all,
                       nation_post=nation.samp,
                       admin.info=admin.info.output,
                       admin=admin))
    }
    attr(res.admin1,"class")="directEST"
    attr(res.admin1,"domain.names") <- admin.info$admin1.name
    return(res.admin1)

}else if(admin==0){


    if (!is.null(alt.strata)) {
      data$admin0.name="country"
      modt<- left_join(data,cluster.info$data,by="cluster")
      # modt<- modt[!(is.na(modt$admin1.name)), ]
      modt$strata.full <- factor(modt[, alt.strata])
      message("Using alt.strata and including all clusters")

    }else{
      data$admin0.name="country"
      modt<- left_join(data,cluster.info$data,by="cluster")
      modt<- modt[!(is.na(modt$admin1.name)), ]
     # modt<- modt[!(is.na(modt$LONGNUM)), ]
      modt$strata.full <- paste(modt$admin1.name, modt$strata)
    }



    if(strata=="all"){
    }else if(strata=="urban"){
      modt<-modt%>% filter(., strata == "urban")
    }else if(strata=="rural"){
      modt<-modt%>% filter(., strata == "rural")
    }

    smoothSurvey_res<-smoothSurvey(as.data.frame(modt),
                                   responseType ="binary",
                                   responseVar= "value",
                                   regionVar = "admin0.name",
                                   clusterVar = "~cluster+householdID",
                                   weightVar = "weight",
                                   strataVar = "strata.full",
                                   Amat =NULL,
                                   CI = CI,
                                   is.unit.level=FALSE,
                                   smooth=FALSE, ...)
    admin0_res<-smoothSurvey_res$HT
    admin0_res$direct.se<-sqrt(admin0_res$HT.var)
    colnames(admin0_res)[colnames(admin0_res) == 'HT.est'] <- 'direct.est'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.var'] <- 'direct.var'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.logit.est'] <- 'direct.logit.est'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.logit.var'] <- 'direct.logit.var'
    colnames(admin0_res)[colnames(admin0_res) == 'HT.logit.prec'] <- 'direct.logit.prec'


    admin0_res$direct.lower <- expit(admin0_res$direct.logit.est + stats::qnorm((1 - CI) / 2) * sqrt(admin0_res$direct.logit.var))
    admin0_res$direct.upper <- expit(admin0_res$direct.logit.est + stats::qnorm(1 - (1 - CI) / 2) * sqrt(admin0_res$direct.logit.var))
    admin0_res$cv.mean=admin0_res$direct.se/admin0_res$direct.est
    admin0_res$cv.star=pmax(admin0_res$direct.se/ admin0_res$direct.est, admin0_res$direct.se/ (1-admin0_res$direct.est) )

   # colnames(admin0_res)[1] <- c("admin0.name")
   # return(list(res.admin0=admin0_res[,-1]))

    res.natl=list(res.natl=admin0_res[,-1],
                  admin.info=admin.info.output,
                  admin=admin
                  )
    attr(res.natl,"class")="directEST"
    # attr(res.admin0,"domain.names") <- ""
    return(res.natl)

  }

}
