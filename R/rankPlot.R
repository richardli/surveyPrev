#' Get ranking plot of model results
#'
#' This function return scatter plot at admin 1 or 2 level for given model results.
#'
#' @param x  a model result  using surveyPrev of class "fhModel" or "clusterModel"
#' @param direction  direction of the ranking. The default is direction = 1, which correspond to larger value having higher ranking. If direction = -1, larger value has lower ranking.
#' @return This function returns a ranking plot.  
#' @import ggplot2 
#' @import ggridges
#' @author Zehang Li, Qianyu Dong
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
#' admin.info2 <- adminInfo(poly.adm = ZambiaAdm2,
#'                         admin = 2,
#' 						   by.adm="NAME_2",
#' 						   by.adm.upper = "NAME_1")
#' cl_res_ad2_unstrat <- clusterModel(data = data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   stratification = FALSE,
#'                   model = "bym2",
#'                   admin = 2,
#'                   aggregation = TRUE,
#'                   CI = 0.95)
#' rankPlot(cl_res_ad2_unstrat)
#'
#' }
#'
#' @export
#'
rankPlot <- function(x, direction = 1){ 
	 x_att <- attributes(x)

	# USING SURVEYPREV CLASSES
	if(x_att$class %in% c("fhModel", "clusterModel")){
	  if ("admin2_post" %in% x_att$names){
	      samples = x$admin2_post
	  }else{
	      samples = x$admin1_post
	  }
	}	
	# samples is now a nsamp * nregion matrix
	for(i in 1:dim(samples)[1]){
		samples[i, ] <- rank(samples[i, ] , ties.method = 'random')
	}
	
	if(direction == -1) samples = max(samples) + 1 - samples


	samples.long <- data.frame(region.name = rep(x_att$domain.names, each = nrow(samples)), 
							   value = as.numeric(samples))
	if("res.admin2" %in% names(x)){
		upper <- x$res.admin2[, c("admin2.name.full", "admin1.name")]
		upper$admin2.name.short <- NA
	    for(i in 1:dim(upper)[1]){
	    	k <- nchar(as.character(upper$admin1.name[i]))
	    	upper$admin2.name.short[i] <- substr(upper$admin2.name.full[i], 
	    										 start = k+2, 
	    										 stop = nchar(upper$admin2.name.full[i]))
	    }
		colnames(upper) <- c("region.name", "group.name", "admin2.name.short")
		samples.long <- left_join(samples.long, upper)
	}

    meanrank <- apply(samples, 2, mean)
	samples.long$mean = meanrank[match(samples.long$region.name, x_att$domain.names)]

	g <- ggplot2::ggplot(samples.long) +
			aes(x = value, y = stats::reorder(region.name, mean)) + 
			ggridges::geom_density_ridges(stat = "binline", bins = dim(samples)[2], pad = FALSE, color = NA, fill = "skyblue", scale = 0.95) + theme(legend.position = "none") + xlab("") + ylab("") + scale_x_continuous() + theme_bw()  
	if("group.name" %in% colnames(samples.long)){
		g <- g + aes(y = stats::reorder(admin2.name.short, mean)) + facet_wrap(~group.name, scales = "free_y")
	}
	return(g)
}

