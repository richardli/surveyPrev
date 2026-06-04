# Internal polygon-hatching utilities.
#
# Ported from the 'HatchedPolygons' package (GPL-3) by Sebastien Rochette,
# which in turn derives its core crossing algorithm (polygon.onehatch /
# polygon.fullhatch) from base R's graphics polygon hatching by
# Gregory R. Warnes. 'HatchedPolygons' is not available on CRAN, so the
# relevant code is vendored here (with attribution) to keep surveyPrev free
# of non-CRAN dependencies. The 'sp'/'sf' wrapper has been reimplemented to
# operate directly on 'sf' objects.

# Compute hatch-line segments crossing a single (closed) polygon ring.
# x, y: coordinates of the ring; (x0, y0): a point on the hatch line;
# (xd, yd): direction of the hatch line.
polygon_onehatch <- function(x, y, x0, y0, xd, yd, fillOddEven = FALSE) {
  halfplane <- as.integer(xd * (y - y0) - yd * (x - x0) <= 0)
  cross <- halfplane[-1L] - halfplane[-length(halfplane)]
  does.cross <- cross != 0
  if (!any(does.cross)) return(NULL)
  x1 <- x[-length(x)][does.cross]
  y1 <- y[-length(y)][does.cross]
  x2 <- x[-1L][does.cross]
  y2 <- y[-1L][does.cross]
  t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 - x1)) /
          (xd * (y2 - y1) - yd * (x2 - x1)))
  o <- order(t)
  tsort <- t[o]
  crossings <- cumsum(cross[does.cross][o])
  if (fillOddEven) crossings <- crossings %% 2
  drawline <- crossings != 0
  lx <- x0 + xd * tsort
  ly <- y0 + yd * tsort
  lx1 <- lx[-length(lx)][drawline]
  ly1 <- ly[-length(ly)][drawline]
  lx2 <- lx[-1L][drawline]
  ly2 <- ly[-1L][drawline]
  data.frame(lx1 = lx1, ly1 = ly1, lx2 = lx2, ly2 = ly2)
}

# Generate the full set of parallel hatch segments for one polygon ring at
# the requested line density and angle. Returns a data.frame of segment
# endpoints (lx1, ly1, lx2, ly2) or NULL when the ring is too small.
polygon_fullhatch <- function(x, y = NULL, density, angle, fillOddEven = FALSE) {
  if (is.null(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }
  if (x[1] != x[length(x)] | y[1] != y[length(y)]) {
    x <- c(x, x[1L])
    y <- c(y, y[1L])
  }
  angle <- angle %% 180
  res <- NULL
  xd <- cos(angle / 180 * pi)
  yd <- sin(angle / 180 * pi)
  if (angle < 45 || angle > 135) {
    if (angle < 45) {
      first.x <- max(x)
      last.x <- min(x)
    } else {
      first.x <- min(x)
      last.x <- max(x)
    }
    y.shift <- 1 / density / abs(cos(angle / 180 * pi))
    x0 <- 0
    y0 <- floor((min(y) - first.x * yd / xd) / y.shift) * y.shift
    y.end <- max(y) - last.x * yd / xd
    while (y0 < y.end) {
      res.tmp <- polygon_onehatch(x, y, x0, y0, xd, yd, fillOddEven = fillOddEven)
      if (!is.null(res.tmp)) res <- rbind(res, res.tmp)
      y0 <- y0 + y.shift
    }
  } else {
    if (angle < 90) {
      first.y <- max(y)
      last.y <- min(y)
    } else {
      first.y <- min(y)
      last.y <- max(y)
    }
    x.shift <- 1 / density / abs(sin(angle / 180 * pi))
    x0 <- floor((min(x) - first.y * xd / yd) / x.shift) * x.shift
    y0 <- 0
    x.end <- max(x) - last.y * xd / yd
    while (x0 < x.end) {
      res.tmp <- polygon_onehatch(x, y, x0, y0, xd, yd, fillOddEven = fillOddEven)
      if (!is.null(res.tmp)) res <- rbind(res, res.tmp)
      x0 <- x0 + x.shift
    }
  }
  res
}

# Build diagonal hatching lines clipped to a set of sf polygons. Returns an
# sf object of LINESTRINGs (or NULL when nothing can be drawn) suitable for
# leaflet::addPolylines. Replacement for HatchedPolygons::hatched.SpatialPolygons.
#
#' @importFrom sf st_geometry st_crs st_cast st_linestring st_sfc
#' @importFrom sf st_intersection st_as_sf
hatched_polygons <- function(x, density = 10, angle = 45, fillOddEven = FALSE) {
  geom <- sf::st_geometry(x)
  crs <- sf::st_crs(geom)
  ## split any MULTIPOLYGON features into individual POLYGONs
  polys <- suppressWarnings(sf::st_cast(geom, "POLYGON"))
  n <- length(polys)
  if (n == 0) return(NULL)
  density <- rep(density, length.out = n)
  angle <- rep(angle, length.out = n)

  all_lines <- list()
  for (k in seq_len(n)) {
    poly <- polys[[k]]      # POLYGON sfg: list of rings, [[1]] is exterior
    ext <- poly[[1]]        # exterior ring coordinate matrix
    seg <- polygon_fullhatch(ext[, 1], ext[, 2], density = density[k],
                             angle = angle[k], fillOddEven = fillOddEven)
    if (is.null(seg) || nrow(seg) == 0) next
    lines_k <- lapply(seq_len(nrow(seg)), function(i) {
      sf::st_linestring(matrix(c(seg$lx1[i], seg$lx2[i],
                                 seg$ly1[i], seg$ly2[i]), ncol = 2))
    })
    lines_sfc <- sf::st_sfc(lines_k, crs = crs)
    ## clip lines to the actual polygon so holes are respected
    clipped <- suppressWarnings(
      sf::st_intersection(lines_sfc, sf::st_sfc(poly, crs = crs)))
    if (length(clipped) > 0) all_lines[[length(all_lines) + 1L]] <- clipped
  }
  if (length(all_lines) == 0) return(NULL)
  out <- do.call(c, all_lines)
  sf::st_as_sf(data.frame(ID = seq_along(out)), geometry = out)
}
