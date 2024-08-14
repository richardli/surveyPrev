#' Plot exceedance probability of model results
#'
#' This function return a map of exceedance probability for given model results.
#'
#' @param x  a model result  using surveyPrev of class "fhModel" or "clusterModel"
#' @param threshold the threshold to be used in computing the exceedance probability
#' @param direction Direction of the color scheme. It can be either 1 (smaller values are darker) or -1 (higher values are darker). Default is set to 1.
#' @param exceed  direction of the comparison The default is exceed = TRUE, which correspond to probability of prevalence larger than the threshold. If exceed = FALSE, the plot computes probability smaller than the threshold.
#' @param geo SpatialPolygonsDataFrame object for the map
#' @param by.geo variable name specifying region names in the data
#' @return This function returns a map showing probability of prevalence over/under the threshold.  
#' @param ylim range of the values to be plotted.
#' @param ... additional arguments passed to SUMMER::mapPlot().
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
#' ZambiaAdm2$admin2.name.full <- paste0(ZambiaAdm2$NAME_1,
#' 									     "_",
#' 										 ZambiaAdm2$NAME_2)
#' exceedPlot(cl_res_ad2_unstrat, threshold = 0.5, 
#' 			  exceed = TRUE, direction = -1, 
#' 			  geo = ZambiaAdm2, by.geo = "admin2.name.full")
#' exceedPlot(cl_res_ad2_unstrat, threshold = 0.5, 
#' 			  exceed = FALSE, direction = -1, 
#' 			  geo = ZambiaAdm2, by.geo = "admin2.name.full")
#'
#' }
#'
#' @export
#'
exceedPlot <- function(x, exceed = TRUE, direction = 1, threshold = NA, geo = geo, by.geo = NULL, ylim = NULL, ...){

	 if(is.na(threshold)) stop("A numerical threshold need to be specified.") 
	 x_att <- attributes(x)

	# USING SURVEYPREV CLASSES
	if(x_att$class %in% c("fhModel", "clusterModel")){
	  if ("admin2_post" %in% x_att$names){
	      samples = x$admin2_post
	  }else{
	      samples = x$admin1_post
	  }
	}	

	dat <- data.frame(region.name = x_att$domain.names,
					     value = NA)
	# samples is now a nsamp * nregion matrix
	for(i in 1:dim(samples)[2]){
		dat$value[i] <- sum(samples[, i] > threshold) /dim(samples)[1]
		if(!exceed) dat$value[i]<- sum(samples[, i] < threshold) /dim(samples)[1] 
	}
	
	g <- SUMMER::mapPlot(data = dat, geo = geo, by.data = "region.name", by.geo = by.geo, variable = "value", removetab = TRUE, legend.label = "Probability", direction = direction, ylim = ylim, ...)

	return(g)
}

