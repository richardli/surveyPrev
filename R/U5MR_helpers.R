#' Helper function for the U5MR direct estimation
#' 
#' @param modt input data frame internally constructed from directEST() function
#' @param regionVar region column
#' @return
#' a fitted object in the same format as smoothSurvey() function, to be slotted in directEST() internal script.
#' 
directEST_u5mr <- function(modt, regionVar){
	modt$time <- "all"
	modt$died <- modt$value
	fit <- SUMMER::getDirect(births = modt, 
				   	   regionVar = regionVar, 
					   years = "all", 
				   	   timeVar = "time", 
				   	   clusterVar = "~cluster + householdID", 
				   	   ageVar = "age", 
				   	   weightsVar = "weight")
	# remove the added national estimates. Let the outside codes handle regionVar
	if(fit$region[1] == "All") fit <- fit[-1, ]

	# note var.est is the variance on the logit scale
	out <- list(HT = data.frame(region = fit$region,
								HT.est = fit$mean,
								HT.var = fit$var.est * fit$mean^2 * (1 - fit$mean)^2,
								HT.logit.est = fit$logit.est, 
								HT.logit.var = fit$var.est,
								HT.logit.prec = fit$logit.prec  
								))
	# to be consistent with the surveyPrev data structure, 
	#    if an area has no death, set est = 0 and logit.est = NA
	#    this differes from SUMMER convention which sets everything to NA
	no_death <- names(which(tapply(modt$value, modt[[regionVar]], sum) == 0))
	if(length(no_death) > 0){
		out$HT[match(no_death, out$HT$region), "HT.est"] <- 0
		out$HT[match(no_death, out$HT$region), "HT.var"] <- 0
	}
	return(out)
}




#' Calculate cluster model estimates using beta binomial model for U5MR
#'
#' This function calculate smoothed direct estimates at given admin level.
#'
#' @param data dataframe that contains the indicator of interests(column name is value), output of getDHSindicator function
#' @param cluster.info dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admin.info dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#' @param X dataframe that contains areal covariates, the first column should be the same admin name as in admin.info$data.
#' @param X.unit dataframe that contains unit covariates, must contain cluster
#' @param X.pixel dataframe that contains pixel covariates, must contain 1. admin1.name or admin2.name.full, 2. Population, 3. strata if stratification==T
#' @param CI Credible interval to be used. Default to 0.95.
#' @param model  smoothing model used in the random effect. Options are independent ("iid") or spatial ("bym2").
#' @param stratification whether or not to include urban/rural stratum.
#' @param aggregation whether or not report aggregation results.
#' @param nested whether or not to fit a nested model.
#' @param interact whether or not to fit a admin1 x urban/rural  model.
#' @param overdisp.mean prior mean for logit(d), where d is the intracluster correlation.
#' @param overdisp.prec prior precision for logit(d), where d is the intracluster correlation.
#' @param pc.u pc prior u for iid or bym2 precision.
#' @param pc.alpha pc prior alpha for iid or bym2 precision.
#' @param pc.u.phi pc prior u for bym2 mixing paramete.
#' @param pc.alpha.phi pc prior u for bym2 mixing paramete.
#' @param age.space.group vector indicating grouping of the ages groups in the spatial model. For example, if each age group is assigned a different spatial component, then set age.group.group to c(1:length(age.group)); if all age groups share the same spatial component, then set age.group.group to a rep(1, length(age.group)). The default for 8 age groups is c(1,2,2,2,3,3,3,3), which assigns a separate hazards to the first two groups and a common hazard for the rest of the age groups. The vector should contain values starting from 1.  
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,


clusterModel_u5mr <- function(data,cluster.info, admin.info, X=NULL,X.unit=NULL,X.pixel=NULL,admin, CI = 0.95, model = c("bym2", "iid"),
                       stratification = FALSE, aggregation = FALSE,nested=FALSE,interact=FALSE,
                       overdisp.mean=0, overdisp.prec=0.4 , pc.u = 1,  pc.alpha = 0.01, pc.u.phi=0.5,pc.alpha.phi=2/3, age.space.group = c(1, 2, 2, 2, 3, 3, 3, 3)){
	# call <- match.call()

	# Extract month count for each age group, 
	# unique() preserves order since data$age is expected to be factor
	if(!is.factor(data$age)) warning("The 'age' variable is not a factor. This is likely due to manual update to the input data. The order of the age groups may be incorrect. Please make the 'age' variable an ordered factor from younger to older age groups") 
    all_ages <- unique(data$age)
    ns <- rep(1, length(as.character(all_ages)))
    for(i in 1:length(all_ages)){
        if(all_ages[i] == "0"){
            ns[i] <- 1
            next
        }
        tmp <- as.numeric(strsplit(as.character(all_ages[i]), "-")[[1]])
        ns[i] <- tmp[2] - tmp[1] + 1
    }

    age_groups <- ns_group <- rep(NA, length(unique(age.space.group)))
    for(j in 1:length(unique(age.space.group))){
    	sub <- which(age.space.group == unique(age.space.group)[j])
    	age_groups[j] <- paste(all_ages[sub], collapse = ",")
    	ns_group[j] <- sum(ns[sub])
    }
    for(j in 1:length(age_groups)){
    	parts <- strsplit(age_groups[j], "[,-]")[[1]]
		if(parts[1] != parts[length(parts)]) age_groups[j] <- paste0(parts[1], "-", parts[length(parts)])
    }
    age_groups_map <- age_groups[age.space.group]

    # age_groups_map <- c("0", "1-11", "12-59", "12-59", "12-59", "12-59")
    # age_groups <- c("0", "1-11", "12-59")
    # ns_group <- c(1, 11, 48)


	all_models <- NULL
	# For each age group, fit a separate cluster level model
	for(a in age_groups){
		age_bins <- all_ages[age_groups_map == a]
		data_sub <- subset(data, age %in% age_bins)
		data_sub <- data_sub[, colnames(data_sub) != "age"]

		all_models[[a]] <- clusterModel(data = data_sub,
                                cluster.info = cluster.info, 
                                admin.info = admin.info, 
                                X=X,
                                X.unit=X.unit,
                                X.pixel=X.pixel,
                                admin = admin, 
                                CI = CI, 
                                model = model,
                                stratification =  stratification,
                                aggregation = aggregation,
                                nested=nested,
                                interact=interact,
                                overdisp.mean=overdisp.mean, 
                                overdisp.prec=overdisp.prec , 
                                pc.u = pc.u,  
                                pc.alpha = pc.alpha, 
                                pc.u.phi=pc.u.phi,
                                pc.alpha.phi=pc.alpha.phi)
	    # call[[1]] <- as.name("clusterModel")
		# call[[2]] <- as.name("data_sub")
		# all_models[[a]] <- eval(call)
	}


	## TODO: all models below ignores admin == 0 case!

	# helper function to combine posterior draws
	# mortality = 1 - \prod_j (1 - h_j)^n_j
	draws <- function(all_models, age_groups, ns_group, admin){
		if(admin == 1) draw_obj <- "admin1_post"
		if(admin == 2) draw_obj <- "admin2_post"

		##
		## TODO: double check the posterior columns are ordered in the same way across models!
		##       for now it seems to be the case based on the clusterModel logic
		##
		draws <- matrix(1, dim(all_models[[1]][[draw_obj]])[1], dim(all_models[[1]][[draw_obj]])[2])
		for(i in 1:length(age_groups)){
			h.draws <- all_models[[age_groups[i]]][[draw_obj]]
			draws <- draws * (1 - h.draws)^ns_group[i]
		}
		draws = 1 - draws
		return(draws)
	}

	draws_all <- draws(all_models, age_groups, ns_group, admin)
    if(admin == 1) res <- all_models[[1]]$res.admin1
    if(admin == 2) res <- all_models[[1]]$res.admin2
    res$mean <- apply(draws_all, 2, mean)
    res$median <- apply(draws_all, 2, median)
    res$sd <- apply(draws_all, 2, sd)
    res$var <- apply(draws_all, 2, var)
    res$lower <- apply(draws_all, 2, quantile, (1 - CI)/2)
    res$upper <- apply(draws_all, 2, quantile, 1 - (1 - CI)/2)
    res$cv <- res$sd / res$mean
    # TODO: define the different CVs


	# Arrange final output
	if(admin == 1){
		out <- list(res.admin1 = res, 
				inla = NULL,
				admin1_post = draws_all,  
				urban_post = NULL,  
				rural_post = NULL,
				admin.info = admin.info,
				admin = admin, 
			    hazards = all_models)   # return all hazard models
	}else if(admin == 2){
		out <- list(res.admin2 = res, 
				inla = NULL,
				admin2_post = draws_all,  
				urban_post = NULL,  
				rural_post = NULL,
				admin.info = admin.info,
				admin = admin, 
			    hazards = all_models)   # return all hazard models
	}
	
	# Not in this version yet:
	#  aggregation
	#  urban/rural posterior draws
	#  ...
	return(out)
}
  