#' Attach upper-level admin names to lower-level polygons
#'
#' Maps each polygon in a lower administrative layer (e.g., ADM2) to its
#' containing polygon in an upper layer (e.g., ADM1), populating an upper-level
#' name column. If containment fails for some polygons (e.g., due to slivers or
#' minor topology issues), a nearest-centroid fallback is used.
#'
#' @param poly.adm.upper Spatial polygons for the *upper* admin level
#'   (e.g., Admin 1 when the desired output is Admin 2). Can be an
#'   \code{sf} object or an \code{sp::SpatialPolygonsDataFrame}.
#' @param poly.adm Spatial polygons for the *target* admin level you want
#'   returned (e.g., Admin 2). Can be an \code{sf} object or an
#'   \code{sp::SpatialPolygonsDataFrame}.
#' @param by.adm Character scalar. Column name in \code{poly.adm} that contains
#'   the admin names for the target level (e.g., \code{"NAME_2"}).
#' @param by.adm.upper Character scalar. Column name in \code{poly.adm.upper}
#'   that contains the admin names for the upper level (e.g., \code{"NAME_1"}).
#' @param out_lower Character scalar. Name of the output column to hold the
#'   lower-level names in the returned object. Defaults to \code{"NAME_2"}.
#' @param out_upper Character scalar. Name of the output column to hold the
#'   upper-level names in the returned object. Defaults to \code{"NAME_1"}.
#' @param sort Logical. If \code{TRUE} (default), alphabetically sorts the
#'   output by \code{out_upper} then \code{out_lower}.
#'
#' @details
#' Inputs may be either \code{sf} or \code{sp} polygon data. If \code{sp} inputs
#' are provided, they are converted to \code{sf} internally. Containment is
#' determined via \code{sf::st_within()}. For polygons not matched by containment,
#' the function uses \code{sf::st_nearest_feature()} on centroids
#' (\code{sf::st_centroid()}) to assign an upper-level name.
#'
#' @return An \code{sf} object at the target (lower) admin level containing:
#' \itemize{
#'   \item \code{out_lower}: lower-level admin name (copied from \code{by.adm})
#'   \item \code{out_upper}: upper-level admin name (derived via spatial matching)
#'   \item \code{geometry}: polygon geometry
#'   \item \code{admin2.name.full}: convenience concatenation
#'         \code{paste0(out_upper, "_", out_lower)}
#' }
#'
#' @examples
#' \dontrun{
#' library(sf)
#' adm1 <- st_read("geoBoundaries/geoBoundaries-NGA-ADM1-all/geoBoundaries-NGA-ADM1.shp")
#' adm2 <- st_read("geoBoundaries/geoBoundaries-NGA-ADM2-all/geoBoundaries-NGA-ADM2.shp")
#'
#' # If both layers use "shapeName" but you want outputs named NAME_1/NAME_2:
#' res <- addUpper(
#'   poly.adm.upper = adm1,
#'   poly.adm       = adm2,
#'   by.adm         = "shapeName",   # lower-level names
#'   by.adm.upper   = "shapeName",   # upper-level names
#'   out_lower      = "NAME_2",
#'   out_upper      = "NAME_1",
#'   sort           = TRUE
#' )
#'
#' # If the columns are already NAME_1 / NAME_2:
#' res2 <- addUpper(
#'   poly.adm.upper = adm1,
#'   poly.adm       = adm2,
#'   by.adm         = "NAME_2",
#'   by.adm.upper   = "NAME_1"
#' )
#' }
#'
#'
#' @importFrom sf st_within st_nearest_feature st_centroid st_as_sf st_make_valid
#' @importFrom dplyr mutate select arrange all_of
#' @export


addUpper <- function(poly.adm.upper, poly.adm, by.adm, by.adm.upper,
                     out_lower = "NAME_2", out_upper = "NAME_1",
                     sort = TRUE) {

  # 1) Containment mapping: which upper polygon contains each lower polygon
  idx <- st_within(poly.adm, poly.adm.upper)

  # Build the NAME_1 vector using column names given by user
  upper_name_vec <- rep(NA_character_, nrow(poly.adm))
  hit <- lengths(idx) > 0
  if (any(hit)) {
    upper_name_vec[hit] <- sapply(idx[hit], function(i) poly.adm.upper[[by.adm.upper]][i[1]])
  }

  # 2) Fallback: nearest centroid for the misses
  miss <- which(!hit)
  if (length(miss) > 0) {
    nn <- st_nearest_feature(st_centroid(poly.adm[miss, ]), poly.adm.upper)
    upper_name_vec[miss] <- poly.adm.upper[[by.adm.upper]][nn]
  }

  # 3) Attach output columns with user-chosen names
  res <- poly.adm %>%
    mutate(
      !!out_lower := .data[[by.adm]],      # lower-level name column chosen by user
      !!out_upper := upper_name_vec        # upper-level name column computed
    ) %>%
    select(all_of(c(out_lower, out_upper)), geometry)

  # 4) Optional alphabetical ordering
  if (isTRUE(sort)) {
    res <- res %>% arrange(.data[[out_upper]], .data[[out_lower]])
  }

  # 5) Convenience composite name
  res$admin2.name.full <- paste0(res[[out_upper]], "_", res[[out_lower]])

  return(res)
}

