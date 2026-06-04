## Data-prep: shrink the bundled Zambia GADM example polygons.
##
## The original ZambiaAdm1/ZambiaAdm2 objects are full-resolution GADM
## SpatialPolygonsDataFrames (~230k vertices for admin-2), which are far more
## detailed than needed for the package's example choropleth maps and bloat the
## installed package size. This script simplifies the geometry (preserving
## shared district borders via rmapshaper) and drops unused GADM attribute
## columns, then re-saves the data under the original object names and class.
##
## Run from the package root:  Rscript data-raw/simplify_zambia.R
## Requires: sf, rmapshaper (not runtime dependencies of the package).

library(sf)
library(rmapshaper)

keep_frac <- 0.05  # fraction of vertices to retain

shrink <- function(obj, cols) {
  s <- sf::st_as_sf(obj)
  s <- s[, intersect(cols, names(s))]
  s <- rmapshaper::ms_simplify(s, keep = keep_frac, keep_shapes = TRUE)
  ## return in the original SpatialPolygonsDataFrame class
  methods::as(s, "Spatial")
}

## ---- Admin 1 ----
e1 <- new.env(); load("data/ZambiaAdm1.rda", envir = e1)
ZambiaAdm1 <- shrink(e1$ZambiaAdm1,
                     cols = c("GID_0", "GID_1", "COUNTRY", "NAME_1"))
save(ZambiaAdm1, file = "data/ZambiaAdm1.rda", compress = "xz")

## ---- Admin 2 ----
e2 <- new.env(); load("data/ZambiaAdm2.rda", envir = e2)
ZambiaAdm2 <- shrink(e2$ZambiaAdm2,
                     cols = c("GID_0", "GID_1", "GID_2",
                              "COUNTRY", "NAME_1", "NAME_2"))
save(ZambiaAdm2, file = "data/ZambiaAdm2.rda", compress = "xz")
