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
#' @param var.fix Whether to add phantom cluster to fix direct estimate with variance close to 0.
#' @param eps the threshold of variance to implement the variance fix method.
#' @param tol the threshold of variance to implement the variance fix method.
#' @param floor_var the threshold of variance to implement the variance fix method.
#' @param all.fix Whether to add phantom cluster to all areas
#' @param ... Additional arguments passed on to the `smoothSurvey` function
#'
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,

#' @import dplyr
#' @importFrom SUMMER smoothSurvey expit
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
#' res_ad1 <- directEST_new(data = data,
#'                   cluster.info = cluster.info,
#'                   admin = 1,
#'                   aggregation = FALSE)
#' }
#'
#' @export
directEST_varfix = function (data, cluster.info, admin, strata = "all", CI = 0.95,
                           weight = c("population", "survey")[1], admin.info = NULL,
                           aggregation = FALSE, alt.strata = NULL, var.fix = TRUE, eps = 1e-12,
                           floor_var = 1e-12, tol = 1e-12, all.fix = FALSE,
                           ...)
{


  assign_in_out <- function(target_region, agg_cluster, admin.level = 2) {
    # admin.level = 1 or 2
    col_to_use <- if (admin.level == 1) "admin1.name" else "admin2.name.full"

    # genrate in_out indicator
    agg_cluster$in_out <- as.integer(agg_cluster[[col_to_use]] %in% target_region)
    return(agg_cluster)
  }

  compute_domain_summaries <- function(agg_cluster_in_out, n_h){

    v_dot_dot_dot = agg_cluster_in_out %>%
      group_by(admin1.name) %>%
      summarise(
        v_dot_dot_dot = sum(total_weight*in_out),
        .groups = "drop"
      )


    v_h_dot_dot = agg_cluster_in_out %>%
      group_by(admin1.name, strata) %>%
      summarise(
        v_h_dot_dot = sum(total_weight*in_out),
        .groups = "drop"
      )


    v_h_c_dot <- agg_cluster_in_out %>%
      group_by(admin1.name, admin2.name, admin2.name.full, strata, cluster) %>%
      summarise(
        v_h_c_dot = unique(total_weight*in_out),
        .groups = "drop"
      )


    ## calculate p_hj_hat
    p_hj_hat = agg_cluster_in_out %>%
      group_by(admin1.name) %>%
      summarise(
        p_hj_hat = sum(weight_times_value*in_out, na.rm = TRUE)/ sum(total_weight*in_out, na.rm = TRUE),
        .groups = "drop"
      )

    ## calculate p_h_hj_hat
    p_h_hj_hat_numerator = agg_cluster_in_out %>%
      group_by(admin1.name, strata) %>%
      summarise(
        p_h_hj_hat_numerator = sum(weight_times_value*in_out, na.rm = TRUE),
        .groups = "drop"
      )

    p_h_hj_hat <- p_h_hj_hat_numerator %>%
      left_join(v_h_dot_dot, by = c("admin1.name", "strata")) %>%
      mutate(
        p_h_hj_hat = p_h_hj_hat_numerator / v_h_dot_dot
      )

    ## calculate p_h_c_hj_hat
    p_h_c_hj_hat_numerator = agg_cluster_in_out %>%
      group_by(admin1.name, admin2.name, admin2.name.full, strata, cluster) %>%
      summarise(
        p_h_c_hj_hat_numerator = weight_times_value*in_out,
        .groups = "drop"
      )

    p_h_c_hj_hat <- p_h_c_hj_hat_numerator %>%
      left_join(v_h_c_dot, by = c("admin1.name", "admin2.name", "admin2.name.full","strata", "cluster")) %>%
      mutate(
        p_h_c_hj_hat = p_h_c_hj_hat_numerator / v_h_c_dot
      )


    df <- v_h_c_dot %>%
      left_join(
        v_h_dot_dot %>%
          select(admin1.name, strata, v_h_dot_dot),
        by = c("admin1.name", "strata")
      )%>%
      left_join(
        v_dot_dot_dot %>%
          select(admin1.name, v_dot_dot_dot),
        by = c("admin1.name")
      )%>%
      left_join(
        n_h %>%
          select(admin1.name, strata, n_h),
        by = c("admin1.name", "strata")
      )%>%
      left_join(
        p_h_c_hj_hat %>%
          select(
            admin1.name, admin2.name, admin2.name.full, strata, cluster, p_h_c_hj_hat),
        by = c("admin1.name", "admin2.name", "admin2.name.full", "strata", "cluster")
      ) %>%
      left_join(
        p_h_hj_hat %>%
          select(admin1.name, strata, p_h_hj_hat),
        by = c("admin1.name", "strata")
      ) %>%
      left_join(
        p_hj_hat %>%
          select(admin1.name, p_hj_hat),
        by = c("admin1.name")
      )

    return(df)
  }

  est_p_HJ <- function(df, target_region, admin.level = 2) {
    if (admin.level == 2) {
      d <- df %>% filter(.data[["admin2.name.full"]] %in% target_region)
    } else {
      d <- df %>% filter(.data[["admin1.name"]] %in% target_region)
    }

    v_ddd <- unique(d$v_dot_dot_dot)
    if (length(v_ddd) != 1L) stop("v_dot_dot_dot should be unique within the filtered data.")

    p_hj_hat_val <- unique(d$p_hj_hat)
    if (length(p_hj_hat_val) != 1L) stop("p_hj_hat should be unique within the filtered data.")


    # based on strata(h): n_h/(n_h-1) * sum_c [...]
    contrib_h <- d %>%
      group_by(admin1.name, strata) %>%
      summarise(
        n_h         = unique(n_h),
        c_out = n_h-length(cluster),
        v_h_dot_dot = unique(v_h_dot_dot),
        sum_sq = sum(
          ( v_h_c_dot * (p_h_c_hj_hat - p_hj_hat) -
              (v_h_dot_dot / n_h) * (p_h_hj_hat - p_hj_hat) )^2,
          na.rm = TRUE)
        + c_out*unique(((v_h_dot_dot / n_h)* (p_h_hj_hat - p_hj_hat))^2),
        .groups = "drop"
      ) %>%
      mutate(
        adj  = ifelse(n_h > 1, n_h / (n_h - 1), NA_real_),
        term = adj * sum_sq
      )

    var_hat <- sum(contrib_h$term, na.rm = TRUE) / (v_ddd^2)
    return(list(
      p_hj_hat = p_hj_hat_val,
      var_hat = var_hat
    ))
  }

  trigger_variance_fix <- function(modt, admin.level = 2, tol = 1e-12) {

    # 选择域列：Admin1 或 Admin2（full）
    area_col <- if (admin.level == 1) "admin1.name" else "admin2.name.full"

    area_vec <- sort(unique(modt[[area_col]]))


    out <- lapply(area_vec, function(a) {
      dat_i <- modt[modt[[area_col]] == a, , drop = FALSE]

      # overall
      p_overall   <- sum(dat_i$weight * dat_i$value, na.rm = TRUE) / sum(dat_i$weight, na.rm = TRUE)

      # by stratum (h)
      p_by_h <- dat_i %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(
          p_h = sum(weight * value, na.rm = TRUE)/sum(weight, na.rm = TRUE),
          .groups = "drop"
        )
      strata_present <- sort(unique(p_by_h$strata))

      # by stratum × cluster (h×c)
      p_by_hc <- dat_i %>%
        dplyr::group_by(strata, cluster) %>%
        dplyr::summarise(
          p_hc = sum(weight * value, na.rm = TRUE)/sum(weight, na.rm = TRUE),
          .groups = "drop"
        )

      .max_abs_diff_to_scalar <- function(v, s) {
        v <- v[is.finite(v)]
        if (!length(v) || !is.finite(s)) return(Inf)
        max(abs(v - s))
      }



      trigger <- FALSE
      phantom_to_add <- 0L

      K <- length(strata_present)
      if (K == 1L) {
        s   <- strata_present[1]
        ph  <- p_by_h$p_h[p_by_h$strata == s]
        phc <- p_by_hc$p_hc[p_by_hc$strata == s]
        trigger <- is.finite(p_overall) &&
          is.finite(ph) &&
          .max_abs_diff_to_scalar(phc, p_overall) <= tol &&
          abs(ph - p_overall) <= tol
        if (isTRUE(trigger)) phantom_to_add <- 1L

      } else if (K >= 2L) {
        max_diff_h  <- .max_abs_diff_to_scalar(p_by_h$p_h,  p_overall)
        max_diff_hc <- .max_abs_diff_to_scalar(p_by_hc$p_hc, p_overall)
        phs <- p_by_h$p_h[order(p_by_h$strata)]
        eq_between_layers <- FALSE
        if (length(phs) >= 2 && all(is.finite(phs))) {
          eq_between_layers <- max(abs(outer(phs, phs, `-`)), na.rm = TRUE) <= tol
        }
        trigger <- is.finite(p_overall) &&
          max_diff_hc <= tol &&
          max_diff_h  <= tol &&
          isTRUE(eq_between_layers)
        if (isTRUE(trigger)) phantom_to_add <- 2L
      }

      data.frame(
        area = a,
        trigger = isTRUE(trigger),
        phantom_to_add = as.integer(phantom_to_add),
        stringsAsFactors = FALSE
      )
    })

    dplyr::bind_rows(out)
  }

  ALLtrigger_variance_fix <- function(modt, admin.level = 2) {
    area_col <- if (admin.level == 1) "admin1.name" else "admin2.name.full"
    area_vec <- sort(unique(modt[[area_col]]))

    out <- lapply(area_vec, function(a) {
      dat_i <- modt[modt[[area_col]] == a, , drop = FALSE]
      K <- length(unique(dat_i$strata))

      data.frame(
        area = a,
        trigger = TRUE,
        phantom_to_add = if (K >= 2L) 2L else if (K == 1L) 1L else 0L,
        stringsAsFactors = FALSE
      )
    })

    dplyr::bind_rows(out)
  }

  compute_domain_summaries_fix <- function(
    agg_cluster_in_out,
    n_h,
    target_region,
    p_h_hj_nat,
    admin.level = 2
  ){

    if (admin.level == 2) {
      admin_strata <- agg_cluster_in_out %>%
        dplyr::filter(admin2.name.full == target_region) %>%
        dplyr::distinct(strata)

      target_admin1 <- agg_cluster_in_out %>%
        dplyr::filter(admin2.name.full == target_region) %>%
        dplyr::distinct(admin1.name) %>%
        dplyr::pull(admin1.name) %>%
        unique()
    } else {
      admin_strata <- agg_cluster_in_out %>%
        dplyr::filter(admin1.name == target_region) %>%
        dplyr::distinct(strata)

      target_admin1 <- target_region
    }

    # ===== National-level average cluster weights for phantom =====
    nat_avg_weight_h <- agg_cluster %>%
      group_by(strata) %>%
      summarise(
        nat_avg_total_weight = mean(total_weight, na.rm = TRUE),
        .groups = "drop"
      )

    # Replace local admin1 avg weight with national-level avg weight
    avg_admin1_weight_h <- nat_avg_weight_h %>%
      rename(avg_total_weight = nat_avg_total_weight)


    # avg_admin1_weight_h <- agg_cluster_in_out %>%
    #   filter(admin1.name == target_admin1) %>%
    #   group_by(strata) %>%
    #   summarise(
    #     avg_total_weight = mean(total_weight, na.rm = TRUE),
    #   ) %>%
    #   ungroup()





    df_pre_phantom <- admin_strata %>%
      left_join(avg_admin1_weight_h, by = "strata") %>%
      left_join(p_h_hj_nat, by = "strata") %>%
      mutate(avg_weight_times_value = avg_total_weight * p_h_nat)

    df_pre_phantom$phantom_to_add = 1

    df_pre_phantom$admin1.name = target_admin1


    ## v_dot_dot_dot_aug
    v_dot_dot_dot = agg_cluster_in_out %>%
      group_by(admin1.name) %>%
      summarise(
        v_dot_dot_dot = sum(total_weight*in_out),
        .groups = "drop"
      )


    v_dot_dot_dot_aug <- v_dot_dot_dot %>%
      dplyr::mutate(
        v_dot_dot_dot_aug = dplyr::if_else(
          admin1.name == target_admin1,
          v_dot_dot_dot + sum(df_pre_phantom$avg_total_weight),
          v_dot_dot_dot  # or NA, or 0, or whatever default you want
        )
      )


    ## v_h_dot_dot_aug
    v_h_dot_dot_aug <- agg_cluster_in_out %>%
      group_by(admin1.name, strata) %>%
      summarise(
        v_h_dot_dot = sum(total_weight * in_out, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(df_pre_phantom, by = c("admin1.name", "strata")) %>%
      mutate(
        v_h_dot_dot_aug = v_h_dot_dot + avg_total_weight
      )


    ## v_h_c_dot

    v_h_c_dot <- agg_cluster_in_out %>%
      group_by(admin1.name, admin2.name, admin2.name.full, strata, cluster) %>%
      summarise(
        v_h_c_dot = unique(total_weight*in_out),
        .groups = "drop"
      )



    ## calculate p_aug_hat
    p_aug_hat_numerator = agg_cluster_in_out %>%
      group_by(admin1.name) %>%
      summarise(
        p_aug_hat_numerator = sum(weight_times_value*in_out, na.rm = TRUE) + sum(df_pre_phantom$avg_weight_times_value),
        .groups = "drop"
      )

    p_aug_hat = p_aug_hat_numerator %>%
      left_join(v_dot_dot_dot_aug, by = c("admin1.name")) %>%
      mutate(
        p_aug_hat = p_aug_hat_numerator / v_dot_dot_dot_aug
      )


    ## calculate p_h_aug_hat
    p_h_aug_hat_numerator <- agg_cluster_in_out %>%
      group_by(admin1.name, strata) %>%
      summarise(
        p_h_hj_hat_numerator = sum(weight_times_value * in_out, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(df_pre_phantom, by = c("admin1.name", "strata")) %>%
      mutate(
        p_h_aug_hat_numerator = p_h_hj_hat_numerator + avg_weight_times_value
      )

    p_h_aug_hat <- p_h_aug_hat_numerator %>%
      left_join(v_h_dot_dot_aug, by = c("admin1.name", "strata")) %>%
      mutate(
        p_h_aug_hat = p_h_aug_hat_numerator / v_h_dot_dot_aug
      )

    ## calculate p_h_c_hj_hat
    p_h_c_hj_hat_numerator = agg_cluster_in_out %>%
      group_by(admin1.name, admin2.name, admin2.name.full, strata, cluster) %>%
      summarise(
        p_h_c_hj_hat_numerator = weight_times_value*in_out,
        .groups = "drop"
      )

    p_h_c_hj_hat <- p_h_c_hj_hat_numerator %>%
      left_join(v_h_c_dot, by = c("admin1.name", "admin2.name", "admin2.name.full","strata", "cluster")) %>%
      mutate(
        p_h_c_hj_hat = p_h_c_hj_hat_numerator / v_h_c_dot
      )


    df <- v_h_c_dot %>%
      left_join(
        v_h_dot_dot_aug %>%
          select(admin1.name, strata, v_h_dot_dot_aug),
        by = c("admin1.name", "strata")
      )%>%
      left_join(
        v_dot_dot_dot_aug %>%
          select(admin1.name, v_dot_dot_dot_aug),
        by = c("admin1.name")
      )%>%
      left_join(
        n_h %>%
          select(admin1.name, strata, n_h),
        by = c("admin1.name", "strata")
      )%>%
      left_join(
        df_pre_phantom %>%
          select(admin1.name, strata, phantom_to_add),
        by = c("admin1.name", "strata")
      )%>%
      left_join(
        p_h_c_hj_hat %>%
          select(
            admin1.name, admin2.name, admin2.name.full, strata, cluster, p_h_c_hj_hat),
        by = c("admin1.name", "admin2.name", "admin2.name.full", "strata", "cluster")
      ) %>%
      left_join(
        p_h_aug_hat %>%
          select(admin1.name, strata, p_h_aug_hat),
        by = c("admin1.name", "strata")
      ) %>%
      left_join(
        p_aug_hat %>%
          select(admin1.name, p_aug_hat),
        by = c("admin1.name")
      )

    return(list(
      df = df,
      df_pre_phantom = df_pre_phantom
    ))

  }

  est_p_aug <- function(df, df_pre_phantom, target_region, admin.level = 2) {
    if (admin.level == 2) {
      d <- df %>% filter(.data[["admin2.name.full"]] %in% target_region)
    } else {
      d <- df %>% filter(.data[["admin1.name"]] %in% target_region)
    }

    v_ddd_aug <- unique(d$v_dot_dot_dot_aug)
    if (length(v_ddd_aug) != 1L) stop("v_dot_dot_dot_aug should be unique within the filtered data.")

    p_aug_hat_val <- unique(d$p_aug_hat)
    if (length(p_aug_hat_val) != 1L) stop("p_aug_hat should be unique within the filtered data.")


    # forumla 9, when c belongs to real cluster
    contrib_h <- d %>%
      group_by(admin1.name, strata) %>%
      summarise(
        n_h         = unique(n_h),
        n_h_aug         = unique(n_h) + unique(phantom_to_add),
        c_out = n_h-length(cluster),
        v_h_dot_dot_aug = unique(v_h_dot_dot_aug),
        sum_sq = sum(
          ( v_h_c_dot * (p_h_c_hj_hat - p_aug_hat) -
              (v_h_dot_dot_aug / n_h_aug) * (p_h_aug_hat - p_aug_hat) )^2,
          na.rm = TRUE)
        + c_out*unique(((v_h_dot_dot_aug / n_h_aug)* (p_h_aug_hat - p_aug_hat))^2),
        .groups = "drop"
      ) %>%
      mutate(
        adj  = ifelse(n_h_aug > 1, n_h_aug / (n_h_aug - 1), NA_real_),
        term = adj * sum_sq
      )


    var_hat_real <- sum(contrib_h$term, na.rm = TRUE) / (v_ddd_aug^2)


    contrib_h_phantom <- df_pre_phantom %>%
      left_join(d, by = c("admin1.name", "strata", "phantom_to_add")) %>%
      group_by(admin1.name, strata) %>%
      summarise(
        n_h         = unique(n_h),
        n_h_aug         = unique(n_h) + unique(phantom_to_add),
        v_h_dot_dot_aug = unique(v_h_dot_dot_aug),
        sum_sq = unique((avg_weight_times_value - avg_total_weight*p_aug_hat -
                           (v_h_dot_dot_aug / n_h_aug) * (p_h_aug_hat - p_aug_hat) )^2),
        .groups = "drop"
      )%>%
      mutate(
        adj  = ifelse(n_h_aug > 1, n_h_aug / (n_h_aug - 1), NA_real_),
        term = adj * sum_sq
      )

    var_hat_phantom <- sum(contrib_h_phantom$term, na.rm = TRUE) / (v_ddd_aug^2)

    var_hat <- var_hat_real + var_hat_phantom
    return(list(
      p_aug_hat = p_aug_hat_val,
      var_hat = var_hat
    ))
  }


  cvf <- function(p, sd) {
    pmax(sd / p, sd / (1 - p))
  }

  options(survey.adjust.domain.lonely = TRUE)
  options(survey.lonely.psu = "adjust")
  if (sum(is.na(data$value)) > 0) {
    data <- data[rowSums(is.na(data)) == 0, ]
    message("Removing NAs in indicator response")
  }


  if(!is.null(admin.info)){
    admin.info=admin.info$data
    admin.info.output=admin.info
  }else{
    admin.info.output=NULL
  }

  if (admin == 2) {
    if (strata != "all") {
      message("Subnational stratum-specific direct estimates are not implemented yet. Only overall estimates are computed")
    }

    trigger_area <- character(0)
    ### Change here later for final version
    if (!is.null(cluster.info)) {
      modt <- dplyr::left_join(data, cluster.info$data, by = "cluster")
    } else {
      modt <- data
    }


    modt <- modt[!(is.na(modt$admin2.name)), ]
    modt$strata.full <- factor(paste(modt$admin1.name, modt$strata))
    if (!is.null(alt.strata)) {
      modt$strata.full <- factor(modt[, alt.strata])
    }
    modt <- modt[order(modt$admin1.name, modt$admin2.name),
    ]

    modt$weight_times_value = modt$value * modt$weight

    agg_cluster <- modt %>%
      group_by(cluster, admin1.name, strata, strata.full,
               admin2.name.full, admin2.name, weight) %>%
      summarise(
        value   = sum(value),
        n_obs       = n(),                       # number of indiviudal in each cluster
        total_weight = sum(weight),              # all indiviudal's weight
        weight_times_value = sum(weight_times_value),
        .groups = "drop"
      )


    n_h = agg_cluster %>%
      group_by(admin1.name,strata) %>%
      summarise(
        n_h = length(cluster),
        .groups = "drop"
      )


    admin2_list = unique(modt$admin2.name.full)
    admin2_res <- list()

    for (i in seq_along(admin2_list)) {
      admin2.full <- admin2_list[i]
      agg_cluster_in_out <- assign_in_out(target_region = admin2.full,
                                          agg_cluster = agg_cluster, admin.level = 2)

      df <- compute_domain_summaries(agg_cluster_in_out, n_h)

      result <- est_p_HJ(df, target_region = admin2.full, admin.level = 2)

      p_val <- result$p_hj_hat
      v_val <- result$var_hat

      logit_est <- ifelse(p_val == 0 | p_val == 1, NA, qlogis(p_val))
      logit_var <- ifelse(p_val == 0 | p_val == 1, NA,
                          v_val / (p_val * (1 - p_val))^2)
      logit_prec <- ifelse(is.na(logit_var), NA, 1 / logit_var)

      admin2_res[[i]] <- data.frame(
        admin2.name.full = admin2.full,
        direct.est       = p_val,
        direct.var       = v_val,
        direct.logit.est = logit_est,
        direct.logit.var = logit_var,
        direct.logit.prec = logit_prec,
        stringsAsFactors = FALSE
      )


    }
    admin2_res <- do.call(rbind, admin2_res)


    admin2_res$direct.se <- sqrt(admin2_res$direct.var)
    admin2_res$direct.lower <- expit(admin2_res$direct.logit.est +
                                       stats::qnorm((1 - CI)/2) * sqrt(admin2_res$direct.logit.var))
    admin2_res$direct.upper <- expit(admin2_res$direct.logit.est +
                                       stats::qnorm(1 - (1 - CI)/2) * sqrt(admin2_res$direct.logit.var))
    # admin2_res$cv <- sqrt(admin2_res$direct.var)/admin2_res$direct.est
    admin2_res$cv=cvf(admin2_res$direct.est,admin2_res$direct.se)

    a <- strsplit(admin2_res$admin2.name.full, "_")



    admin2_res$admin2.name <- matrix(unlist(a), ncol = 2,
                                     byrow = T)[, 2]
    admin2_res$admin1.name <- matrix(unlist(a), ncol = 2,
                                     byrow = T)[, 1]

    res.admin2 <- admin2_res
    if (var.fix == TRUE) {

      strata_levels <- sort(unique(tolower(trimws(modt$strata))))
      modt <- modt %>% dplyr::mutate(strata = tolower(trimws(strata)))

      # National Hajek p_h_hj_nat
      p_h_hj_nat <- agg_cluster %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(
          p_h_nat = sum(weight_times_value, na.rm = TRUE)/ sum(total_weight, na.rm = TRUE),
          .groups = "drop"
        )

      p_hj_nat <- agg_cluster %>%
        dplyr::summarise(
          p_h_nat = sum(weight_times_value, na.rm = TRUE)/ sum(total_weight, na.rm = TRUE),
          .groups = "drop"
        )

      admin_areas <- res.admin2$admin2.name.full

      ## trigger variance fix
      if (!all.fix){
        res_trigger <- trigger_variance_fix(modt, admin.level = 2, tol = tol)
      }else{
        res_trigger <- ALLtrigger_variance_fix(modt, admin.level = 2)
      }

      trigger_area = res_trigger$area[res_trigger$trigger == TRUE]
      nontrigger_area = res_trigger$area[res_trigger$trigger == FALSE]

      if (length(trigger_area) > 0) {
        admin2_res <- list()
        for (i in trigger_area) {

          admin2.full <- i

          df_trigger = res_trigger[res_trigger$area==admin2.full, ]

          agg_cluster_in_out <- assign_in_out(target_region = admin2.full,
                                              agg_cluster = agg_cluster, admin.level = 2)

          out <- compute_domain_summaries_fix(agg_cluster_in_out, n_h, target_region = admin2.full, p_h_hj_nat = p_h_hj_nat, admin.level = 2)
          df <-  out$df
          df_pre_phantom <-  out$df_pre_phantom

          result <- est_p_aug(df, df_pre_phantom, target_region = admin2.full, admin.level = 2)

          p_val <- result$p_aug_hat
          v_val <- result$var_hat
          logit_est <- ifelse(p_val == 0 | p_val == 1, NA, qlogis(p_val))
          logit_var <- ifelse(p_val == 0 | p_val == 1, NA,
                              v_val / (p_val * (1 - p_val))^2)
          logit_prec <- ifelse(is.na(logit_var), NA, 1 / logit_var)

          admin2_res[[i]] <- data.frame(
            admin2.name.full = admin2.full,
            direct.est       = p_val,
            direct.var       = v_val,
            direct.logit.est = logit_est,
            direct.logit.var = logit_var,
            direct.logit.prec = logit_prec,
            stringsAsFactors = FALSE
          )
        }

        admin2_res <- do.call(rbind, admin2_res)


        admin2_res$direct.se <- sqrt(admin2_res$direct.var)
        admin2_res$direct.lower <- expit(admin2_res$direct.logit.est +
                                           stats::qnorm((1 - CI)/2) * sqrt(admin2_res$direct.logit.var))
        admin2_res$direct.upper <- expit(admin2_res$direct.logit.est +
                                           stats::qnorm(1 - (1 - CI)/2) * sqrt(admin2_res$direct.logit.var))
        # admin2_res$cv <- sqrt(admin2_res$direct.var)/admin2_res$direct.est
        admin2_res$cv=cvf(admin2_res$direct.est,admin2_res$direct.se)

        a <- strsplit(admin2_res$admin2.name.full, "_")
        admin2_res$admin2.name <- matrix(unlist(a), ncol = 2,
                                         byrow = T)[, 2]
        admin2_res$admin1.name <- matrix(unlist(a), ncol = 2,
                                         byrow = T)[, 1]
        res.admin2.fix <- res.admin2 %>%
          rows_update(admin2_res, by = "admin2.name.full", unmatched = "ignore")
        res.admin2 = res.admin2.fix
      }


    }

    if (aggregation == FALSE) {
    }
    else {
      if ((is.null(admin.info) || sum(is.na(admin.info$population)) >
           0) || is.null(weight == "population")) {
        message("Need population information for aggregation")
        aggregation = FALSE
      }
    }
    if (aggregation == FALSE) {
      for (i in 1:dim(admin2_res)[1]) {
        if (is.na(admin2_res[i, ]$direct.logit.est) &&
            round(admin2_res[i, ]$direct.est, digits = 8) ==
            1) {
          admin2_res[i, ]$direct.logit.est = 36
        }
        if (is.na(admin2_res[i, ]$direct.logit.est) &&
            admin2_res[i, ]$direct.est == 0) {
          admin2_res[i, ]$direct.logit.est = -36
        }
        if (is.na(admin2_res[i, ]$direct.logit.var)) {
          admin2_res[i, ]$direct.logit.var = 0
        }
      }
      dd = data.frame(admin2.name.full = admin2_res$admin2.name.full,
                      value = admin2_res$direct.logit.est, sd = sqrt(admin2_res$direct.logit.var))
      draw.all = expit(apply(dd[, 2:3], 1, FUN = function(x) rnorm(10000,
                                                                   mean = x[1], sd = x[2])))
      colnames(draw.all) = admin2_res$admin2.name.full
      res.admin2 = list(res.admin2 = res.admin2,
                        admin2_post = draw.all,
                        fixed_areas=trigger_area,
                        admin=admin,
                        admin.info=admin.info.output
                 )
    }
    else {
      for (i in 1:dim(admin2_res)[1]) {
        if (is.na(admin2_res[i, ]$direct.logit.est) &&
            round(admin2_res[i, ]$direct.est, digits = 8) ==
            1) {
          admin2_res[i, ]$direct.logit.est = 36
        }
        if (is.na(admin2_res[i, ]$direct.logit.est) &&
            admin2_res[i, ]$direct.est == 0) {
          admin2_res[i, ]$direct.logit.est = -36
        }
        if (is.na(admin2_res[i, ]$direct.logit.var)) {
          admin2_res[i, ]$direct.logit.var = 0
        }
      }
      dd = data.frame(admin2.name.full = admin2_res$admin2.name.full,
                      value = admin2_res$direct.logit.est, sd = sqrt(admin2_res$direct.logit.var))
      draw.all = expit(apply(dd[, 2:3], 1, FUN = function(x) rnorm(10000,
                                                                   mean = x[1], sd = x[2])))
      colnames(draw.all) = admin2_res$admin2.name.full
      if (weight == "population") {
        weight_dt <- left_join(dd, dplyr::distinct(admin.info),
                               by = "admin2.name.full") %>% group_by(admin1.name) %>%
          mutate(prop = round(population/sum(population),
                              digits = 4))
      }
      else {
        weight_dt <- modt %>% group_by(admin2.name.full) %>%
          mutate(sumweight2 = sum(weight), digits = 4) %>%
          dplyr::distinct(admin2.name.full, sumweight2, admin1.name,
                   admin2.name) %>% group_by(admin1.name) %>%
          mutate(prop = round(sumweight2/sum(sumweight2),
                              digits = 4)) %>% left_join(dd, by = "admin2.name.full")
      }
      weight_dt <- weight_dt[match(admin2_res$admin2.name.full,
                                   weight_dt$admin2.name.full), ]
      admin1.list <- sort(unique(weight_dt$admin1.name))
      admin1.samp <- matrix(NA, 10000, length(admin1.list))
      for (i in 1:length(admin1.list)) {
        which.admin2 <- which(weight_dt$admin1.name ==
                                admin1.list[i])
        admin1.samp[, i] <- apply(draw.all[, which.admin2,
                                           drop = FALSE], 1, function(x, w) {
                                             sum(x * w)
                                           }, weight_dt$prop[which.admin2])
      }
      colnames(admin1.samp) <- admin1.list
      if (weight == "population") {
        weight_dt_mean <- left_join(admin2_res[, c("admin2.name.full",
                                                   "direct.est")], dplyr::distinct(admin.info), by = "admin2.name.full") %>%
          group_by(admin1.name) %>% mutate(prop = round(population/sum(population),
                                                        digits = 4)) %>% mutate(value1 = prop * direct.est)
      }
      else {
        weight_dt_mean <- modt %>% group_by(admin2.name.full) %>%
          mutate(sumweight2 = sum(weight), digits = 4) %>%
          dplyr::distinct(admin2.name.full, sumweight2, admin1.name,
                   admin2.name) %>% group_by(admin1.name) %>%
          mutate(prop = round(sumweight2/sum(sumweight2),
                              digits = 4)) %>% left_join(admin2_res[,
                                                                    c("admin2.name.full", "direct.est")], by = "admin2.name.full") %>%
          mutate(value1 = prop * direct.est)
      }
      admin1_agg <- data.frame(admin1.name = colnames(admin1.samp),
                               direct.se = apply(admin1.samp, 2, sd), direct.lower = apply(admin1.samp,
                                                                                           2, quantile, probs = c((1 - CI)/2, 1 - (1 -
                                                                                                                                     CI)/2))[1, ], direct.upper = apply(admin1.samp,
                                                                                                                                                                        2, quantile, probs = c((1 - CI)/2, 1 - (1 -
                                                                                                                                                                                                                  CI)/2))[2, ])
      admin1_agg <- admin1_agg %>% left_join(aggregate(value1 ~
                                                         admin1.name, data = weight_dt_mean, sum), by = "admin1.name") %>%
        rename(direct.est = value1)

        admin1_agg$cv=cvf(admin1_agg$direct.est,admin1_agg$direct.se)

      admin1_agg <- admin1_agg[, c("admin1.name", "direct.est", "direct.se", "direct.lower", "direct.upper","cv")]



      if (weight == "population") {
        admin1.distinct = dplyr::distinct(data.frame(admin1.name = admin.info$admin1.name,
                                              population = admin.info$population.admin1))
        weight_dt = admin1.distinct$population[match(colnames(admin1.samp),
                                                     admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
        nation.samp <- admin1.samp %*% weight_dt
        weight_dt_mean <- weight_dt %*% admin1_agg$direct.est

      }
      else {
        weight_dt <- modt %>% group_by(admin1.name) %>%
          mutate(sumweight2 = sum(weight), digits = 4) %>%
          dplyr::distinct(admin1.name, sumweight2) %>% ungroup() %>%
          mutate(prop = round(sumweight2/sum(sumweight2),
                              digits = 4))
        nation.samp <- admin1.samp %*% weight_dt$prop
        weight_dt_mean <- weight_dt$prop %*% admin1_agg$direct.est
      }
      nation_agg <- data.frame(direct.est = weight_dt_mean,
                               direct.se = sd(nation.samp), direct.var = var(nation.samp),
                               direct.lower = quantile(nation.samp, probs = c((1 -
                                                                                 CI)/2, 1 - (1 - CI)/2))[1], direct.upper = quantile(nation.samp,
                                                                                                                                     probs = c((1 - CI)/2, 1 - (1 - CI)/2))[2])


      nation_agg$cv=cvf(nation_agg$direct.est,nation_agg$direct.se)



      res.admin2<-list(res.admin2=res.admin2,
                       agg.admin1=admin1_agg,
                       agg.natl=nation_agg,
                       admin2_post=draw.all,
                       admin1_post=admin1.samp,
                       nation_post=nation.samp,
                       fixed_areas=trigger_area,
                       admin.info=admin.info.output,
                       admin=admin
      )
      # res.admin2 <- list(res.admin2 = res.admin2, agg.admin1 = admin1_agg,
      #                    agg.natl = nation_agg, admin2_post = draw.all,
      #                    admin1_post = admin1.samp, nation_post = nation.samp
      # )
    }
    attr(res.admin2, "class") = "directEST"
    attr(res.admin2, "domain.names") <- admin.info$admin2.name.full
    return(res.admin2)
  }
  else if (admin == 1) {
    if (strata != "all") {
      message("Subnational stratum-specific direct estimates are not implemented yet. Only overall estimates are computed")
    }
    trigger_area <- character(0)
    # change here later for final version
    if (!is.null(cluster.info)) {
      modt <- dplyr::left_join(data, cluster.info$data, by = "cluster")
    } else {
      modt <- data
    }
    modt <- modt[!(is.na(modt$admin1.name)), ]
    modt$strata.full <- paste(modt$admin1.name, modt$strata)
    if (!is.null(alt.strata)) {
      modt$strata.full <- factor(modt[, alt.strata])
    }
    modt <- modt[order(modt$admin1.name), ]

    modt$weight_times_value = modt$value * modt$weight
    agg_cluster <- modt %>%
      group_by(cluster, admin1.name, strata, strata.full,
               admin2.name.full, admin2.name, weight) %>%
      summarise(
        value   = sum(value),
        n_obs       = n(),                       # number of indiviudal in each cluster
        total_weight = sum(weight),              # all indiviudal's weight
        weight_times_value = sum(weight_times_value),
        .groups = "drop"
      )

    n_h = agg_cluster %>%
      group_by(admin1.name,strata) %>%
      summarise(
        n_h = length(cluster),
        .groups = "drop"
      )


    admin1_list = unique(modt$admin1.name)

    admin1_res <- list()


    for (i in seq_along(admin1_list)) {
      admin1.name <- admin1_list[i]
      agg_cluster_in_out <- assign_in_out(target_region = admin1.name,
                                          agg_cluster = agg_cluster, admin.level = 1)


      df = compute_domain_summaries(agg_cluster_in_out, n_h)

      result <- est_p_HJ(df, target_region = admin1.name, admin.level = 1)

      p_val <- result$p_hj_hat
      v_val <- result$var_hat

      logit_est <- ifelse(p_val == 0 | p_val == 1, NA, qlogis(p_val))
      logit_var <- ifelse(p_val == 0 | p_val == 1, NA,
                          v_val / (p_val * (1 - p_val))^2)
      logit_prec <- ifelse(is.na(logit_var), NA, 1 / logit_var)

      admin1_res[[i]] <- data.frame(
        admin1.name = admin1.name,
        direct.est       = p_val,
        direct.var       = v_val,
        direct.logit.est = logit_est,
        direct.logit.var = logit_var,
        direct.logit.prec = logit_prec,
        stringsAsFactors = FALSE
      )

    }
    admin1_res <- do.call(rbind, admin1_res)

    admin1_res$direct.se <- sqrt(admin1_res$direct.var)
    admin1_res$direct.lower <- expit(admin1_res$direct.logit.est +
                                       stats::qnorm((1 - CI)/2) * sqrt(admin1_res$direct.logit.var))
    admin1_res$direct.upper <- expit(admin1_res$direct.logit.est +
                                       stats::qnorm(1 - (1 - CI)/2) * sqrt(admin1_res$direct.logit.var))
    # admin1_res$cv <- sqrt(admin1_res$direct.var)/admin1_res$direct.est
    admin1_res$cv=cvf(admin1_res$direct.est,admin1_res$direct.se)# pmax(admin1_res$direct.se/ admin1_res$direct.est, admin1_res$direct.se/ (1-admin1_res$direct.est) )

    res.admin1 = admin1_res

    if (var.fix == TRUE) {

      strata_levels <- sort(unique(tolower(trimws(modt$strata))))
      modt <- modt %>% dplyr::mutate(strata = tolower(trimws(strata)))

      # National Hajek p_h_hj_nat
      p_h_hj_nat <- agg_cluster %>%
        dplyr::group_by(strata) %>%
        dplyr::summarise(
          p_h_nat = sum(weight_times_value, na.rm = TRUE)/ sum(total_weight, na.rm = TRUE),
          .groups = "drop"
        )

      p_hj_nat <- agg_cluster %>%
        dplyr::summarise(
          p_h_nat = sum(weight_times_value, na.rm = TRUE)/ sum(total_weight, na.rm = TRUE),
          .groups = "drop"
        )

      admin_areas <- res.admin1$admin1.name

      ## trigger variance fix
      if (!all.fix){
        res_trigger <- trigger_variance_fix(modt, admin.level = 1, tol = tol)
      }else{
        res_trigger <- ALLtrigger_variance_fix(modt, admin.level = 1)
      }

      trigger_area = res_trigger$area[res_trigger$trigger == TRUE]
      nontrigger_area = res_trigger$area[res_trigger$trigger == FALSE]

      if (length(trigger_area) > 0) {
        admin1_res <- list()
        for (i in trigger_area) {

          admin1 <- i

          df_trigger = res_trigger[res_trigger$area==admin1, ]

          agg_cluster_in_out <- assign_in_out(target_region = admin1,
                                              agg_cluster = agg_cluster, admin.level = 1)

          out <- compute_domain_summaries_fix(agg_cluster_in_out, n_h, target_region = admin1, p_h_hj_nat = p_h_hj_nat, admin.level = 1)
          df <-  out$df
          df_pre_phantom <-  out$df_pre_phantom

          result <- est_p_aug(df, df_pre_phantom, target_region = admin1, admin.level = 1)

          p_val <- result$p_aug_hat
          v_val <- result$var_hat
          logit_est <- ifelse(p_val == 0 | p_val == 1, NA, qlogis(p_val))
          logit_var <- ifelse(p_val == 0 | p_val == 1, NA,
                              v_val / (p_val * (1 - p_val))^2)
          logit_prec <- ifelse(is.na(logit_var), NA, 1 / logit_var)

          admin1_res[[i]] <- data.frame(
            admin1.name = admin1,
            direct.est       = p_val,
            direct.var       = v_val,
            direct.logit.est = logit_est,
            direct.logit.var = logit_var,
            direct.logit.prec = logit_prec,
            stringsAsFactors = FALSE
          )
        }

        admin1_res <- do.call(rbind, admin1_res)


        admin1_res$direct.se <- sqrt(admin1_res$direct.var)
        admin1_res$direct.lower <- expit(admin1_res$direct.logit.est +
                                           stats::qnorm((1 - CI)/2) * sqrt(admin1_res$direct.logit.var))
        admin1_res$direct.upper <- expit(admin1_res$direct.logit.est +
                                           stats::qnorm(1 - (1 - CI)/2) * sqrt(admin1_res$direct.logit.var))
        # admin1_res$cv <- sqrt(admin1_res$direct.var)/admin1_res$direct.est
        admin1_res$cv=cvf(admin1_res$direct.est,admin1_res$direct.se)# pmax(admin1_res$direct.se/ admin1_res$direct.est, admin1_res$direct.se/ (1-admin1_res$direct.est) )


        res.admin1.fix <- res.admin1 %>%
          rows_update(admin1_res, by = "admin1.name", unmatched = "ignore")
        res.admin1 = res.admin1.fix
      }


    }
    if (aggregation == FALSE) {
    }
    else {
      if ((is.null(admin.info) || sum(is.na(admin.info$population)) >
           0) & is.null(weight == "population")) {
        message("Need population information for aggregation")
        aggregation = FALSE
      }
    }
    if (aggregation == FALSE) {
      for (i in 1:dim(admin1_res)[1]) {
        if (is.na(admin1_res[i, ]$direct.logit.est) &&
            round(admin1_res[i, ]$direct.est, digits = 8) ==
            1) {
          admin1_res[i, ]$direct.logit.est = 36
        }
        if (is.na(admin1_res[i, ]$direct.logit.est) &&
            admin1_res[i, ]$direct.est == 0) {
          admin1_res[i, ]$direct.logit.est = -36
        }
        if (is.na(admin1_res[i, ]$direct.logit.var)) {
          admin1_res[i, ]$direct.logit.var = 0
        }
      }
      dd = data.frame(mean = admin1_res$direct.logit.est,
                      sd = sqrt(admin1_res$direct.logit.var))
      draw.all = expit(apply(dd, 1, FUN = function(x) rnorm(10000,
                                                            mean = x[1], sd = x[2])))
      res.admin1 = list(res.admin1 = res.admin1,
                        admin1_post = draw.all,
                        fixed_areas=trigger_area,
                        admin.info=admin.info.output,
                        admin=admin)
    }
    else {
      for (i in 1:dim(admin1_res)[1]) {
        if (is.na(admin1_res[i, ]$direct.logit.est) &&
            round(admin1_res[i, ]$direct.est, digits = 8) ==
            1) {
          admin1_res[i, ]$direct.logit.est = 36
        }
        if (is.na(admin1_res[i, ]$direct.logit.est) &&
            admin1_res[i, ]$direct.est == 0) {
          admin1_res[i, ]$direct.logit.est = -36
        }
        if (is.na(admin1_res[i, ]$direct.logit.var)) {
          admin1_res[i, ]$direct.logit.var = 0
        }
      }
      dd = data.frame(mean = admin1_res$direct.logit.est,
                      sd = sqrt(admin1_res$direct.logit.var))
      draw.all = expit(apply(dd, 1, FUN = function(x) rnorm(5000,
                                                            mean = x[1], sd = x[2])))
      if (weight == "population") {
        admin1.distinct = dplyr::distinct(data.frame(admin1.name = admin.info$admin1.name,
                                              population = admin.info$population))
        weight_dt = admin1.distinct$population[match(admin1_res$admin1.name,
                                                     admin1.distinct$admin1.name)]/sum(admin1.distinct$population)
        nation.samp <- draw.all %*% weight_dt
        weight_dt_mean <- weight_dt %*% admin1_res$direct.est
      }
      else {
        weight_dt <- modt %>% group_by(admin1.name) %>%
          mutate(sumweight2 = sum(weight), digits = 4) %>%
          dplyr::distinct(admin1.name, sumweight2) %>% ungroup() %>%
          mutate(prop = round(sumweight2/sum(sumweight2),
                              digits = 4))
        nation.samp <- draw.all %*% weight_dt$prop
        weight_dt_mean <- weight_dt$prop %*% admin1_res$direct.est
      }
      nation_agg <- data.frame(direct.est = weight_dt_mean,
                               direct.se = sd(nation.samp), direct.var = var(nation.samp),
                               direct.lower = quantile(nation.samp, probs = c((1 -
                                                                                 CI)/2, 1 - (1 - CI)/2))[1], direct.upper = quantile(nation.samp,
                                                                                                                                     probs = c((1 - CI)/2, 1 - (1 - CI)/2))[2])



      nation_agg$cv=cvf(nation_agg$direct.est,nation_agg$direct.se)

      res.admin1 = (list(res.admin1 = res.admin1, agg.natl = nation_agg,
                         admin1_post = draw.all, nation_post = nation.samp,
                         fixed_areas=trigger_area,
                         admin.info=admin.info.output,
                         admin=admin))
    }
    attr(res.admin1, "class") = "directEST"
    attr(res.admin1, "domain.names") <- admin.info$admin1.name
    return(res.admin1)
  }
  else if (admin == 0) {
    if (!is.null(alt.strata)) {
      data$admin0.name = "country"
      modt <- data
      modt$strata.full <- factor(modt[, alt.strata])
      message("Using alt.strata and including all clusters")
    }
    else {
      data$admin0.name = "country"
      modt <- data
      modt <- modt[!(is.na(modt$admin1.name)), ]
      modt$strata.full <- paste(modt$admin1.name, modt$strata)
    }
    if (strata == "all") {
    }
    else if (strata == "urban") {
      modt <- modt %>% filter(., strata == "urban")
    }
    else if (strata == "rural") {
      modt <- modt %>% filter(., strata == "rural")
    }
    smoothSurvey_res <- smoothSurvey(as.data.frame(modt),
                                     responseType = "binary", responseVar = "value",
                                     regionVar = "admin0.name", clusterVar = "~cluster+householdID",
                                     weightVar = "weight", strataVar = "strata.full",
                                     Amat = NULL, CI = CI, is.unit.level = FALSE, smooth = FALSE,
                                     ...)
    admin0_res <- smoothSurvey_res$HT
    admin0_res$direct.se <- sqrt(admin0_res$HT.var)
    colnames(admin0_res)[colnames(admin0_res) == "HT.est"] <- "direct.est"
    colnames(admin0_res)[colnames(admin0_res) == "HT.var"] <- "direct.var"
    colnames(admin0_res)[colnames(admin0_res) == "HT.logit.est"] <- "direct.logit.est"
    colnames(admin0_res)[colnames(admin0_res) == "HT.logit.var"] <- "direct.logit.var"
    colnames(admin0_res)[colnames(admin0_res) == "HT.logit.prec"] <- "direct.logit.prec"
    admin0_res$direct.lower <- expit(admin0_res$direct.logit.est +
                                       stats::qnorm((1 - CI)/2) * sqrt(admin0_res$direct.logit.var))
    admin0_res$direct.upper <- expit(admin0_res$direct.logit.est +
                                       stats::qnorm(1 - (1 - CI)/2) * sqrt(admin0_res$direct.logit.var))
    admin0_res$cv=cvf(admin0_res$direct.est,admin0_res$direct.se)

    res.admin0 = list(res.admin0 = admin0_res[, -1])

    res.natl=list(res.natl=admin0_res[,-1],
                  admin.info=admin.info.output,
                  admin=admin
    )
    attr(res.natl,"class")="directEST"
    # attr(res.admin0,"domain.names") <- ""
    return(res.natl)
  }
}


