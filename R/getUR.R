#' Function to threshold population raster to obtain urban/rural fractions by Admin1 and Admin2 areas
#'
#' This function computes the urban proportion at a given survey year. It requires two population raster files and urban population fraction by admin 1 area from the census. The census year overall population raster is used to partition the grids into urban and rural pixels, based on the urban population fractions in a given area at the census year. The thresholding process is performed by first sorting the pixels from high to low population density, and find a threshold such that the fraction of population above this threshold matches the urban population fraction from the census. This step defines the urbanicity of each pixel. In the second step, for any given year's raster for a specific (sub-)population (e.g., specific age groups), we aggregate the population in the urban pixels defined in the previous step to compute urban proportion for the (sub-)population, within both admin1 and admin2 regions.
#'
#' @param tiff.census  spatial raster of population estimates at the census year when the sampling frame is based, for the whole population.
#' @param tiff.survey spatial raster of population estimates at the survey year, for the target population.
#' @param fact factor to aggregate pixels from tiff.survey to tiff.census. For example, if tiff.census is a population raster at 1km by 1km resolution and tiff.survey is a raster at 100m by 100m resolution, then fact should be set to 10. Currently we only support fact > 1. Default is 10.
#' @param poly.adm1 spatial polygons data frame for admin 1
#' @param poly.adm2 spatial polygons data frame for admin 2
#' @param varname1 column name of district name in the admin 1 spatial polygon data frame
#' @param varname2 column name of district name in the admin 2 spatial polygon data frame
#' @param prop.census a data frame with two columns: `admin1` column correspond to the admin 1 names in the `poly.adm1` file. And `frac` column specifying the proportion of population in each admin 1 area during the census year. See examples for detail.
#' @return a list of two data frames for admin 1 and admin 2 urban ratios and UR surface where 1 for urban.
#'
#' @importFrom raster extract 
#' @importFrom sp SpatialPoints SpatialPolygons getSpPPolygonsIDSlots
#' @importFrom terra rast cellFromXY values
#' @export
#' @examples
#'
#' \dontrun{
#' # ----------------------------------------------------------------------#
#' # Here we consider the example of computing urban/rural fraction for
#' #  Zambia 2018 DHS for the sub-population of children under 1 years old.
#' # This survey is based on sampling frame from the 2010 Zambia Census.
#' # ----------------------------------------------------------------------#
#' #
#' # From Table A1 of Zambia 2013-2014 DHS final report, we can obtain the fraction of
#' #   urban population by Admin 1 areas in the 2010 survey.
#' # Notice that in the appendix of the 2018 DHS final report,
#' #   only distribution of household is reported and not population size by urbanicity.
#' # When the table is not provided in the DHS report, you need to find it from
#' #   the census website directly.
#' # Please note that the admin1 column needs to match the admin 1 names in the
#' #   Admin 1 spatial polygon file exactly.
#' #   For example, here we change "Northwestern" to "North-Western"
#'
#' urban.frac <- data.frame(
#' 		admin1 = c('Central', 'Copperbelt', 'Eastern',
#' 				   'Luapula', 'Lusaka', 'Muchinga',
#' 					'North-Western', 'Northern', 'Southern','Western'),
#' 		frac = c(0.2513, 0.809, 0.1252,
#' 					0.1963, 0.8456, 0.1714,
#' 					0.2172, 0.1826, 0.2448, 0.1474))
#' # The corresponding census year population tiff can be found at:
#' # https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/
#'
#' # The code below downloads the file from the internet directly
#' # You can also download the file directly and read into R
#' link1="https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/"
#' file1="2010/ZMB/zmb_ppp_2010_1km_Aggregated_UNadj.tif"
#' tempfile1 = tempfile()
#' download.file(paste0(link1, file1), destfile = tempfile1,
#' 								method = "libcurl", mode="wb")
#' library(raster)
#' tiff1 <- raster(tempfile1)
#'
#  # The survey year population tiff by sex and age can be found at:
#' # https://hub.worldpop.org/geodata/summary?id=16429
#' # Here we compute population fractions for 0-1 year old population.
#' # The from the same link below
#' link2="https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020/"
#' # The two files are for female and male population respectively,
#' file2f="2018/ZMB/zmb_f_0_2018.tif"
#' file2m="2018/ZMB/zmb_f_0_2018.tif"
#'
#' # Since the two files are very large, we recommend downloading them
#' #   mannually and then load them into R.
#' tiff2f <- raster("zmb_f_0_2018.tif")
#' tiff2m <- raster("zmb_m_0_2018.tif")
#' tiff2 <- tiff2f + tiff2m
#'
#' frac <- getUR(tiff.census = tiff1, tiff.survey = tiff2,
#' 				 prop.census = urban.frac, fact = 10,
#' 				 poly.adm1 = ZambiaAdm1, poly.adm2 = ZambiaAdm2,
#' 				 varname1 = "NAME_1", varname2 = "NAME_2")
#'
#' library(SUMMER)
#' mapPlot(frac$admin1.ur, geo = ZambiaAdm1,
#' 		   by.data = "admin1.name", by.geo = "NAME_1", variable = "urban")
#' mapPlot(frac$admin2.ur, geo = ZambiaAdm2,
#' 		   by.data = "admin2.name", by.geo = "NAME_2", variable = "urban")
#' # Compare with the proportion of Women 14-49 years old in the built-in data
#' # These two plots should be similar but not identical
#' #   since the population is different
#' mapPlot(ZambiaPopWomen$admin2_urban, geo = ZambiaAdm2,
#' 		   by.data = "admin2.name", by.geo = "NAME_2", variable = "urban")
#'
#' }


getUR <- function(tiff.census, tiff.survey, prop.census, fact = 10, poly.adm1, poly.adm2, varname1, varname2){

	# Aggregate survey resolution to match census
	if(fact < 1) stop("Currently we only support 'fact' at least 1.")
	tiff.survey <- aggregate(tiff.survey, fact = fact, sum)

	# Extract population on the grid
	pop_grid <- as.data.frame(coordinates(tiff.survey))
	pop_grid$pop <- raster::extract(tiff.census,
                                     pop_grid[c('x','y')])
	pop_grid$pop_survey <- raster::extract(tiff.survey,
                                     pop_grid[c('x','y')])
	coords <- sp::SpatialPoints(pop_grid[, c("x", "y")])


	if(max(table(prop.census$admin1)) > 1) stop("There are duplicated areas in 'prop.census' table.")

	if("sf" %in% class(poly.adm1)) poly.adm1 <- sf::as_Spatial(poly.adm1)
	if("sf" %in% class(poly.adm2)) poly.adm2 <- sf::as_Spatial(poly.adm2)

	# Match Admin 1 name
	poly.over.adm1 <- SpatialPolygons(poly.adm1@polygons)
	admin1.key <- over(coords, poly.over.adm1)
	admin1.id <- suppressWarnings(poly.adm1@data[getSpPPolygonsIDSlots(poly.over.adm1), varname1])
	admin1.key <- admin1.id[admin1.key]


	# Match Admin 2 name
	poly.over.adm2 <- SpatialPolygons(poly.adm2@polygons)
	admin2.key <- over(coords, poly.over.adm2)
	admin2.id <- suppressWarnings(poly.adm2@data[getSpPPolygonsIDSlots(poly.over.adm2), varname2])
	admin2.key <- admin2.id[admin2.key]
	admin2.key.full <- paste(admin1.key, admin2.key, sep = "_")

	# Main data frame without areas outside of the polygon
	pop_grid$admin1.name <- admin1.key
	pop_grid$admin2.name <- admin2.key
	pop_grid$admin2.name.full <- admin2.key.full
	pop_grid <- subset(pop_grid, !is.na(admin1.key))


	if(sum(admin1.id %in% prop.census$admin1 == FALSE) > 1){
	    stop("The following areas are not in the 'prop.census' table:\n", paste(admin1.id[which(admin1.id %in% prop.census$admin1 == FALSE)], collapse = ", "))
	}


	# Loop through Admin 1 areas to compute threshold
	pop_grid$pop[is.na(pop_grid$pop)] <- 0
	pop_grid$pop_survey[is.na(pop_grid$pop_survey)] <- 0
	pop_grid$urban <- 0
	for(i in 1:length(admin1.id)){
	    sub <- which(pop_grid$admin1.name == admin1.id[i])
	    thres <- prop.census$frac[prop.census$admin1 == admin1.id[i]]
	    vals <- pop_grid$pop[sub]
	    sort.obj <- sort.int(vals, decreasing = TRUE, index.return = TRUE, method = "radix")

	    cum_frac <- cumsum(sort.obj$x) / sum(sort.obj$x)
	    which.urb <- sort.obj$ix[cum_frac < thres]
	    sub.urb <- sub[which.urb]
	    pop_grid$urban[sub.urb] <- 1
	}

	pop.long <- aggregate(pop_survey~urban + admin1.name + admin2.name + admin2.name.full, data = pop_grid, FUN = sum)
	pop <- data.frame(admin2.name.full = unique(pop_grid$admin2.name.full))
	pop <- left_join(pop, subset(pop.long, urban == 1)[, c("admin2.name.full", "pop_survey")])
	colnames(pop)[colnames(pop) == "pop_survey"] <- "urban_pop"
	pop <- left_join(pop, subset(pop.long, urban == 0)[, c("admin2.name.full", "pop_survey")])
	colnames(pop)[colnames(pop) == "pop_survey"] <- "rural_pop"
	pop$urban_pop[is.na(pop$urban_pop)] <- 0
	pop$rural_pop[is.na(pop$rural_pop)] <- 0
	pop$urban <- pop$urban_pop / (pop$urban_pop + pop$rural_pop)
	pop$admin1.name <- pop_grid$admin1.name[match(pop$admin2.name.full, pop_grid$admin2.name.full)]
	pop$admin2.name <- pop_grid$admin2.name[match(pop$admin2.name.full, pop_grid$admin2.name.full)]
	pop <- pop[, c("admin1.name", "admin2.name", "admin2.name.full", "urban_pop", "rural_pop", "urban")]
	pop <- pop[with(pop, order(admin1.name, admin2.name)), ]

	pop1.long <- aggregate(pop_survey~urban + admin1.name, data = pop_grid, FUN = sum)
	pop1 <- data.frame(admin1.name = unique(pop_grid$admin1.name))
	pop1 <- left_join(pop1, subset(pop1.long, urban == 1)[, c("admin1.name", "pop_survey")])
	colnames(pop1)[colnames(pop1) == "pop_survey"] <- "urban_pop"
	pop1 <- left_join(pop1, subset(pop1.long, urban == 0)[, c("admin1.name", "pop_survey")])
	colnames(pop1)[colnames(pop1) == "pop_survey"] <- "rural_pop"
	pop1$urban_pop[is.na(pop1$urban_pop)] <- 0
	pop1$rural_pop[is.na(pop1$rural_pop)] <- 0
	pop1$urban <- pop1$urban_pop / (pop1$urban_pop + pop1$rural_pop)


	# Create empty raster with the same extent/resolution as tiff.survey
	tiff.survey <- terra::rast(tiff.survey)
	urban_rast <-tiff.survey
	terra::values(urban_rast) <- NA  # Start with NA

	# Re-map urban values into the raster using x/y matching
	cell_idx <- terra::cellFromXY(urban_rast, pop_grid[, c("x", "y")])
	urban_rast[cell_idx] <- pop_grid$urban



if(fact>1){
  return(list(admin1.ur = pop1, admin2.ur = pop, UR_surface=urban_rast, pop_survey= tiff.survey ))
}else{
  return(list(admin1.ur = pop1, admin2.ur = pop, UR_surface=urban_rast))
}
}



