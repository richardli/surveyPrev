#' Extract Covariate Data from A list of Rasters
#'
#' This function aligns raster covariates to a population density grid, extracts values for a national grid,
#' associates administrative information, and prepares covariates for cluster-based data.
#'
#' @param tiffs A list of `SpatRaster` with element names the column names for the covariates.
#' @param tiff.population A `SpatRaster` object representing population density used as the reference grid.
#' @param UR.surface (Optional) A `SpatRaster` object indicating urban-rural classification.
#' @param cluster.info **Need STRATA here**
#' @param poly.adm A `sf` object
#' @param by.adm  the column name of column for Admin names for admin 1
#' @param by.adm.upper the column name of column for Admin names for admin 2 or other lower admin level.
#' @param fact A numeric value indicating the aggregation factor for resampling the raster data. If `fact = 1`, no aggregation is performed.
#' @param standardize whether to standardize the covariates/tiffs or not
#' @param na.rm whether to remove rows that have no Population or admin 2 membership
#' @importFrom sf st_as_sf
#' @importFrom terra  aggregate xyFromCell extract resample ncell global
#'
#' @return A list with two elements:
#'   \item{natl.grid}{A data frame containing natl grid of pixels for covariates and Admin information.}
#'   \item{cluster.cov}{A data frame with extracted covariates for cluster locations. U/R info from DHS}
#' @details
#' The function follows these steps:
#' 1. Converts `poly.adm` to an `sf` object.
#' 2. Aligns rasters in `tiffs` to match the `tiff.population` grid using bilinear resampling.
#' 3. Aggregates raster resolution if `fact` is greater than 1.
#' 4. Extracts pixel center coordinates and values for the national grid.
#' 5. Matches pixel locations to administrative boundaries.
#' 6. Extracts raster values at cluster locations.
#'
#'
#'@examples
#'
#'\dontrun{
#'
#' kenya_cov <- getCovariate(
#'     tiffs = cov_raster_stack,
#'     tiff.population = avg.pop,
#'     UR.surface = UR_surface,
#'     cluster.info = cluster.info,
#'     poly.adm = poly.adm2,
#'     by.adm = 'NAME_2',
#'     by.adm.upper = 'NAME_1',
#'     standardize=FALSE,
#'.    na.rm=FALSE,
#'     fact = 1)
#'
#'}
#'
#'
#' @export
getCovariate <- function(
    tiffs,
    tiff.population,
    UR.surface = NULL,
    cluster.info,
    poly.adm,
    by.adm,
    by.adm.upper,
    standardize=FALSE,
    na.rm=FALSE,
    fact=1){
  ####################
  ### preparation
  ####################
  poly.adm <- sf::st_as_sf(poly.adm)

  # Check if input is a list of SpatRaster objects
  if (!all(sapply(tiffs, inherits, "SpatRaster"))) {
    stop("All elements in 'tiffs' must be of class 'SpatRaster'.")
  }


  # if standardize==t, scale everthing in tiff

   # Create a data.frame to hold the mean and sd for each raster
  raster_stats <- data.frame(
    name = character(),
    mean = numeric(),
    sd = numeric(),
    stringsAsFactors = FALSE
  )

  if (standardize) {
    tiffs <- lapply(names(tiffs), function(nm) {
      r <- tiffs[[nm]]

      # Compute mean and sd
      r_mean <- global(r, fun = "mean", na.rm = TRUE)[1, 1]
      r_sd   <- global(r, fun = "sd", na.rm = TRUE)[1, 1]

      # Save stats
      raster_stats <<- rbind(raster_stats, data.frame(name = nm, mean = r_mean, sd = r_sd))

      # Return scaled raster
      # return(scale(r))
      return((r - r_mean) / r_sd)
    })

    # Reassign names to tiffs list after lapply
    names(tiffs) <- raster_stats$name
  }

  # Align each raster to the target grid (pop_den) if needed
  aligned.tiffs <- lapply(tiffs, function(r) {
    ### Resampling raster to match target dimensions
    return(terra::resample(r, tiff.population, method = "bilinear"))
  })

  # Aggregate each raster if need coarser resolution
  if(fact!=1){

    tiff.population = terra::aggregate(tiff.population, fact = fact, fun = mean, na.rm = TRUE)

    aligned.tiffs <- mapply(
      function(r, f) terra::aggregate(r, fact = f, fun = mean, na.rm = TRUE),
      aligned.tiffs, fact, SIMPLIFY = FALSE
    )

  }


  ####################
  ### natl grid
  ####################

  # Extract pixel center coordinates from the base raster (tiff.population)
  coords <- terra::xyFromCell(tiff.population, 1:terra::ncell(tiff.population))

  # Extract population values
  pop_values <- terra::extract(tiff.population, coords)

  # Extract UR values
  if(!is.null(UR.surface)){
    UR_values <- terra::extract(UR.surface, coords)
  }else{
    UR_values <- NA
  }

  # Extract values from other rasters
  extracted_tiff_values <- lapply(aligned.tiffs, function(r) terra::extract(r, coords))

  # Convert aligned raster values into a data frame
  tiff_values_df <- as.data.frame(extracted_tiff_values)
  colnames(tiff_values_df) <- names(aligned.tiffs)  # Assign column names from raster list

  # Combine everything into a single data frame (all grids)
  natl.grid <- data.frame(
    LONGNUM = coords[, 1],  # X-coordinate (Longitude)
    LATNUM = coords[, 2],   # Y-coordinate (Latitude)
    Population = unname(pop_values), # Population values from tiff.population
    strata = unname(UR_values), # UR indicator from UR surface
    tiff_values_df              # Extracted values from other rasters
  )



  ####################
  ### admin info
  ####################

  if(!is.null(by.adm.upper)){
    # define admin2.name.full as the name1_name2 version
    poly.adm <- poly.adm %>%
      mutate(admin2.name.full = paste0(.[[by.adm.upper]], "_", .[[by.adm]]))
  }

  natl_grid_sf <- sf::st_as_sf(natl.grid[c('LONGNUM','LATNUM')], coords = c("LONGNUM", "LATNUM"),
                               crs = sf::st_crs(poly.adm))

  adm_match <- sf::st_join(natl_grid_sf, poly.adm,join = st_intersects)


  if(!is.null(by.adm.upper)){

    natl.grid$admin1.name <- adm_match[[by.adm.upper]]
    natl.grid$admin2.name <- adm_match[[by.adm]]
    natl.grid$admin2.name.full <- adm_match$admin2.name.full

  }else{

    natl.grid$admin1.name <- adm_match[[by.adm]]

  }

  # set up pixel IDs for all pixels (including pixels with all NAs)
  natl.grid <- natl.grid %>% mutate(pixel_ID = row_number())



  ####################
  ### cluster cov
  ####################

  cell_ids <- terra::cellFromXY(tiff.population, cluster.info$data[,c('LONGNUM','LATNUM')])

  cluster.cov.df <- natl.grid[cell_ids,c('Population',colnames(tiff_values_df))]
  cluster.cov.df <- cbind(cluster.info$data,cluster.cov.df)
  # cluster.cov.df$cluster <- cluster.info$data$cluster
  # cluster.cov.df$strata <- cluster.info$data$strata
  rownames(cluster.cov.df) <- NULL

   #######################
   ### remove NA
   #######################

  #remove rows for no Population and admin 2
  if(na.rm==TRUE){
  natl.grid <- natl.grid %>%
    filter(!is.na(admin2.name.full)) %>%
    filter(!is.na(Population)) %>%
    arrange(LONGNUM, desc(LATNUM)) %>%
    mutate(pixel_noNA_ID = row_number())
  }



  ####################
  ### prepare return
  ####################


  if(standardize){
    #NA to 0 for all covariates
    for (col in names(tiffs)) {
      if (col %in% colnames(natl.grid)) {
        natl.grid[[col]][is.na(natl.grid[[col]])] <- 0
      }
    }

    return.obj = list(natl.grid = natl.grid,
                      cluster.cov = cluster.cov.df,
                      raster.stats=raster_stats )
  }else{
    return.obj = list(natl.grid = natl.grid,
                      cluster.cov = cluster.cov.df)
  }




  return(return.obj)

}

