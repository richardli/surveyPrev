#' Get cluster information
#'
#' This function add admin 1 and admin2 information to a particular DHS survey.
#'
#' @param geo  spatial point dataframe
#' @param poly.adm1 spatial polygons dataframe for admin 1
#' @param poly.adm2 spatial polygons dataframe for admin 2 or other lower admin level.
#' @param by.adm1 the column name of column for Admin names for admin 1
#' @param by.adm2 the column name of column for Admin names for admin 2 or other lower admin level.
#' @param map whether to return a map of clusters or not
#' @return This function returns the dataset that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter mutate arrange group_by summarise
#' @importFrom dplyr left_join right_join n_distinct bind_rows
#' @importFrom sf st_as_sf st_is_valid st_join st_transform st_crs
#' @importFrom sf st_coordinates st_centroid st_as_sfc st_bbox
#' @importFrom sf st_boundary st_distance st_drop_geometry
#' @importFrom sf st_buffer st_intersection st_nearest_feature st_point_on_surface
#' @importFrom sp SpatialPolygons SpatialPoints over proj4string
#' @importFrom ggplot2 ggplot geom_sf geom_sf_text geom_point aes theme_bw theme
#' @importFrom ggplot2 scale_color_manual scale_shape_manual scale_fill_manual scale_size_manual
#' @importFrom ggplot2 guides guide_legend
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' data(ZambiaAdm1)
#' data(ZambiaAdm2)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2d)
#' }
#' @export
#'
clusterInfo <- function(geo, poly.adm1, poly.adm2=NULL, by.adm1 = "NAME_1",by.adm2 = "NAME_2",map=FALSE) {
 #  # SpatialPointsDataFrame for point shape file for each cluster
 #  points<-geo
 #  cluster.info<-points@data%>%dplyr::select(cluster=DHSCLUST, LONGNUM, LATNUM)
 #  # remove wrong points in the data if any
 #  wrong.points <- which(points@data$LATNUM < 0.0000001 & points@data$LONGNUM < 0.0000001)
 #  cluster.info <- cluster.info[!(cluster.info$cluster %in% points@data$DHSCLUST[wrong.points]),]
 #
 #  points.frame <- as.data.frame(cluster.info[,c("LONGNUM", "LATNUM")]) # retrieve GPS coordinates where data is sampled.
 #  points.frame <- sp::SpatialPoints(points.frame) # convert the GPS coordinates into "sp" object.
 #
 #  poly.over.adm1 <- sp::SpatialPolygons(poly.adm1@polygons)
 #  proj4string(points.frame) <- proj4string(poly.over.adm1)
 #  poly.over.adm1 <- sp::SpatialPolygons(poly.adm1@polygons)
 #  admin1.key <- over(points.frame, poly.over.adm1)
 #
 #
 #  # cluster.info$admin1 <- admin1.key
 #  # cluster.info$admin1.char <- paste0("admin1_", admin1.key)
 #  cluster.info$admin1.name <- as.character(eval(str2lang("poly.adm1@data$NAME_1")))[admin1.key]
 #
 #  poly.over.adm2 <- SpatialPolygons(poly.adm2@polygons)
 #  proj4string(points.frame) <- proj4string(poly.over.adm2)
 #  admin2.key <- over(points.frame, poly.over.adm2)
 #  miss.frame.adm2 <- points.frame@coords[which(is.na(admin2.key)),]
 #
 #
 #  # cluster.info$admin2 <- admin2.key
 #  # cluster.info$admin2.char <- paste0("admin2_", admin2.key)
 #  cluster.info$admin2.name <- as.character(eval(str2lang("poly.adm2@data$NAME_2")))[admin2.key]
 #  cluster.info$ admin2.name.full <- paste0(cluster.info$admin1.name,"_",cluster.info$admin2.name)
 #
 #
 # # return(cluster.info)
 #  return(list(cluster.info=cluster.info,wrong.points=wrong.points))

#
#   if(is.null(poly.adm2)){
#
#
#
#     poly.adm1<- sf::st_as_sf(poly.adm1)
#     points_sf <- sf::st_as_sf(geo)
#     if( !any(sf::st_is_valid(poly.adm1)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}
#
#     cluster.info <- points_sf %>%
#       select(cluster = DHSCLUST, LONGNUM, LATNUM) #%>%
#
#     wrong.points <- cluster.info[which(abs(cluster.info$LATNUM) <
#                                          1e-07 & abs(cluster.info$LONGNUM) <1e-07), ]$cluster
#
#     admin1.sf <- st_join(cluster.info, poly.adm1) %>%
#       sf::st_transform(st_crs(poly.adm1)) # Transform CRS if needed
#
#     cluster.info$admin1.name <- as.data.frame( admin1.sf)[,by.adm1]
#     #removing wrong.points that has no coordinates
#     cluster.info <- cluster.info[!(cluster.info$cluster %in% points_sf$DHSCLUST[wrong.points]),]
#
#
#     #removing wrong.points that has no admin 1 name
#     wrong.points <- c(wrong.points,cluster.info[ which(is.na(cluster.info$admin1.name)),]$cluster)
#     cluster.info <- cluster.info[!(cluster.info$cluster %in% wrong.points),]
#
#     # return(cluster.info)
#     cluster.info<-as.data.frame(cluster.info)
#     return(list(data=cluster.info, wrong.points = wrong.points))
#   }else{
#
#
#
#   poly.adm1<- sf::st_as_sf(poly.adm1)
#   poly.adm2<-sf::st_as_sf(poly.adm2)
#   points_sf <- sf::st_as_sf(geo)
#
#   if( !any(sf::st_is_valid(poly.adm1)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}
#   if( !any(sf::st_is_valid(poly.adm2)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}

#   # Select required columns and filter out wrong points
#   cluster.info <- points_sf %>%
#     select(cluster = DHSCLUST, LONGNUM, LATNUM) #%>%
#   #filter(!(LATNUM < 0.0000001 & LONGNUM < 0.0000001))
# #removing wrong.points that has weird LONGNUM LATNUM
#
#   # wrong.points <- cluster.info[which(cluster.info$LATNUM < 0.0000001 & cluster.info$LONGNUM < 0.0000001),]$cluster
#
#   wrong.points <- cluster.info[which(abs(cluster.info$LATNUM) <
#                                        1e-07 & abs(cluster.info$LONGNUM) <1e-07), ]$cluster
#
#
#
#   admin1.sf <- st_join(cluster.info, poly.adm1) %>%
#     sf::st_transform(st_crs(poly.adm1)) # Transform CRS if needed
#
#   cluster.info$admin1.name <- as.data.frame( admin1.sf)[,by.adm1]
#
#   # Spatial join for admin2
#   admin2.sf <- st_join(cluster.info, poly.adm2) %>%
#     sf::st_transform(st_crs(poly.adm2)) # Transform CRS if needed
#
#   if(dim(admin2.sf)[1] > dim(cluster.info)[1]){
#     admin2.sf <- admin2.sf[!duplicated(admin2.sf[,c('cluster')]),]
#     warning('overlapping admin regions exist, the first match is kept')
#     }
#
#
#   # Add admin2 name to cluster.info
#    cluster.info$admin2.name <- as.data.frame( admin2.sf)[,by.adm2]
#    cluster.info$admin2.name.full <- paste0(cluster.info$admin1.name,"_",cluster.info$admin2.name)
#
#    #removing wrong.points that has no coordinates
#    cluster.info <- cluster.info[!(cluster.info$cluster %in% points_sf$DHSCLUST[wrong.points]),]
#
#
#    #removing wrong.points that has no admin 1 name
#    wrong.points <- c(wrong.points,cluster.info[ which(is.na(cluster.info$admin1.name)),]$cluster)
#    cluster.info <- cluster.info[!(cluster.info$cluster %in% wrong.points),]
#
#   # return(cluster.info)
#    cluster.info<-as.data.frame(cluster.info)
#   # remove points outside of shapefile
#   # TODO: change those to nearest admin if within DHS jittering range

  # # 0) clean & align
  # if (!all(sf::st_is_valid(poly.adm1))) poly.adm1 <- sf::st_make_valid(poly.adm1)
  # if (!all(sf::st_is_valid(poly.adm2))) poly.adm2 <- sf::st_make_valid(poly.adm2)
  #
  # # use adm2 CRS as master
  # if (sf::st_crs(poly.adm1) != sf::st_crs(poly.adm2))
  #   poly.adm1 <- sf::st_transform(poly.adm1, sf::st_crs(poly.adm2))
  # if (is.na(sf::st_crs(points_sf))) sf::st_crs(points_sf) <- sf::st_crs(poly.adm2)
  # if (sf::st_crs(points_sf) != sf::st_crs(poly.adm2))
  #   points_sf <- sf::st_transform(points_sf, sf::st_crs(poly.adm2))
  #
  # cluster.info <- dplyr::select(points_sf, cluster = DHSCLUST, LONGNUM, LATNUM)
  #
  # # (0,0) filter
  # wrong.points <- cluster.info$cluster[
  #   abs(cluster.info$LATNUM) < 1e-7 & abs(cluster.info$LONGNUM) < 1e-7
  # ]
  # cluster.info <- cluster.info[!(cluster.info$cluster %in% wrong.points), ]
  #
  # # 1) ADM1 join (use within)
  # admin1.sf <- sf::st_join(cluster.info, poly.adm1, join = sf::st_within, left = TRUE)
  # cluster.info$admin1.name <- as.data.frame(admin1.sf)[, by.adm1]
  #
  # # drop points with no ADM1
  # na_adm1 <- is.na(cluster.info$admin1.name)
  # if (any(na_adm1)) {
  #   wrong.points <- c(wrong.points, cluster.info$cluster[na_adm1])
  #   cluster.info <- cluster.info[!na_adm1, ]
  # }
  #
  # # 2) ADM2 join *restricted to that ADM1*
  # #   build a function that, for each point, filters adm2 to its parent adm1,
  # #   then chooses the best candidate deterministically.
  # pick_adm2 <- function(pt) {
  #   adm1 <- as.character(pt$admin1.name)
  #   cand <- poly.adm2[as.character(poly.adm2[[by.adm1]]) == adm1, ]
  #   if (nrow(cand) == 0) return(NA_character_)
  #   hit <- sf::st_join(pt, cand, join = sf::st_within, left = TRUE)
  #   nm  <- as.data.frame(hit)[, by.adm2]
  #
  #   if (!is.na(nm)) return(nm)
  #
  #   # if still NA, try largest overlap via a tiny buffer
  #   eps <- 50/111320  # ~50m in degrees
  #   buf <- sf::st_buffer(sf::st_geometry(pt), eps)
  #   inter <- suppressWarnings(sf::st_intersection(sf::st_as_sf(pt), sf::st_set_geometry(cand, sf::st_geometry(cand))))
  #   if (nrow(inter)) {
  #     inter$.a <- sf::st_area(inter)
  #     nm2 <- as.data.frame(inter[which.max(inter$.a), ])[ , by.adm2]
  #     return(nm2)
  #   }
  #
  #   # last resort: nearest within same ADM1
  #   cand[[by.adm2]][ sf::st_nearest_feature(pt, cand) ]
  # }
  #
  # cluster.info$admin2.name <- vapply(seq_len(nrow(cluster.info)),
  #                                    function(i) pick_adm2(cluster.info[i, ]), character(1))
  #
  # # 3) compose full name and finalize
  # cluster.info$admin2.name.full <- paste0(cluster.info$admin1.name, "_", cluster.info$admin2.name)
  #
  # # drop any unresolved
  # bad <- is.na(cluster.info$admin2.name)
  # if (any(bad)) {
  #   wrong.points <- c(wrong.points, cluster.info$cluster[bad])
  #   cluster.info <- cluster.info[!bad, ]
  # }
  #
  #
  #   return(list(data=cluster.info, wrong.points = wrong.points))
  # }

  # helper: pick a decent projected CRS for distance (meters)
  .pick_utm <- function(x) {
    c <- st_coordinates(st_centroid(st_as_sfc(st_bbox(x))))
    lon <- c[1]; lat <- c[2]
    zone <- floor((lon + 180)/6) + 1
    epsg <- if (lat >= 0) 32600 + zone else 32700 + zone
    st_crs(epsg)
  }

  fixed.points<-c()

    if(is.null(poly.adm2)){

      poly.adm1<- sf::st_as_sf(poly.adm1)
      points_sf <- sf::st_as_sf(geo)
      if( !any(sf::st_is_valid(poly.adm1)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}

      cluster.info <- points_sf %>%
        select(cluster = DHSCLUST, LONGNUM, LATNUM) #%>%

      admin1.sf <- st_join(cluster.info, poly.adm1) %>%
        sf::st_transform(st_crs(poly.adm1)) # Transform CRS if needed

      cluster.info$admin1.name <- as.data.frame( admin1.sf)[,by.adm1]

      # wrong.points case 1: coordinates with LATNUM and LONGNUM < 1e-07
      # wrong.points case 2: points outside the country boundary due to jittering


      #Assign the wrong points (case 2) to the closest boundary and to the correct admin 1.

      wrong.points <- cluster.info[which(abs(cluster.info$LATNUM) <
                                           1e-07 & abs(cluster.info$LONGNUM) <1e-07), ]$cluster

      wrong.points <- c(wrong.points,cluster.info[ which(is.na(cluster.info$admin1.name)),]$cluster)
      result.wrong.points1=c()
      result.wrong.points2=c()
      if(!length(wrong.points) == 0){
        pts <- geo %>% filter(DHSCLUST %in% wrong.points)

        # sanity: assign CRS if missing and make polygons valid
        if (is.na(st_crs(pts)))        st_crs(pts)      <- 4326
        if (is.na(st_crs(poly.adm1)))  st_crs(poly.adm1) <- 4326

        # project to a metric CRS for accurate distances
        crs_m   <- .pick_utm(poly.adm1)
        pts_m   <- st_transform(pts, crs_m)
        adm1_m  <- st_transform(poly.adm1, crs_m)

        # distance from each point to the boundary of every admin1 polygon
        bnd_m   <- st_boundary(adm1_m)                       # MULTILINESTRING per admin1
        D       <- st_distance(pts_m, bnd_m)                 # matrix: n_points x n_admin1
        nearest_idx  <- apply(D, 1, which.min)
        min_dist_m   <- apply(D, 1, min)

        result <- pts %>%
          st_drop_geometry() %>%
          mutate(
            nearest_admin1 = adm1_m$NAME_1[nearest_idx],
            dist_km_to_boundary = as.numeric(min_dist_m) / 1000
          )

        result$match=result$nearest_admin1==result$ADM1NAME
        fixed.points<- result[result$match==TRUE |  result$dist_km_to_boundary< 10,]$DHSCLUST
        wrong.points <- wrong.points[!wrong.points %in% fixed.points]
        result.wrong.points2<- result[ result$dist_km_to_boundary < 10,]
        result.wrong.points1<- result[! result$dist_km_to_boundary < 10,]

        if(sum(fixed.points)>0){
          message("Assign to the closest Admin 1 for points outside the country boundary due to jittering.")

          cluster.info[cluster.info$cluster %in% fixed.points, "admin1.name"]<- result[result$DHSCLUST %in% fixed.points, "nearest_admin1"]

        }



        if( any(result$match==FALSE) && any(abs(result$LATNUM) > 1e-07) && any(abs(result$LONGNUM) >1e-07)){
          message("Take a closer look of wrong points")
        }

      }

      if(map==TRUE){
        geo_no0<-geo[geo$DHSCLUST %in% cluster.info[which(abs(cluster.info$LATNUM) >
                                                            1e-07 & abs(cluster.info$LONGNUM)  >1e-07), ]$cluster,]



        geo_join <- st_join(geo_no0, poly.adm1, left = TRUE) %>%
          mutate(outside = is.na(NAME_1))

        # Build one points data frame with a common 'type' column
        pts_no  <- st_drop_geometry(dplyr::filter(geo_join, !outside)) |> mutate(type = "No issue")
        pts_out <- st_drop_geometry(dplyr::filter(geo_join,  outside)) |> mutate(type = "Outside")

        pts_all <- dplyr::bind_rows(pts_no, pts_out)

        levels <- c("No issue", "Outside")

        map=ggplot() +
          geom_sf(data = poly.adm1, color = "red", fill = NA, linewidth = 0.6, alpha = 0.9) +
          geom_sf(data = poly.adm2, fill = NA, color = "#2C7FB8", linewidth = 0.4) +

          # ONE layer for points → one combined legend
          geom_point(
            data = pts_all,
            aes(x = LONGNUM, y = LATNUM, shape = type, color = type, size = type),
         alpha = 1
          ) +
          scale_shape_manual(
            name = "Cluster Type",
            breaks = levels,
            values = c("No issue" = 1, "Outside" = 17)
          ) +
          scale_color_manual(
            name = "Cluster Type",
            breaks = levels,
            values = c("No issue" = "darkblue", "Outside" = "seagreen")
          ) +
          scale_size_manual(
            name = "Cluster Type",
            breaks = levels,
            values = c("No issue" = 1, "Outside" = 3)
          ) +
          guides(size = guide_legend(override.aes = list(alpha = 1))) +
          theme_bw() +
          theme(legend.position = "right")



      }else{
        map=NULL
      }

      # return(cluster.info)
      cluster.info<-as.data.frame(cluster.info)
      return(list(data=cluster.info,
                  wrong.points = wrong.points,
                  fixed.points=fixed.points,
                  result.wrong.points1=result.wrong.points1,
                  result.wrong.points2=result.wrong.points2,
                  map=map
                ))
    }else{



      poly.adm1<- sf::st_as_sf(poly.adm1)
      poly.adm2<-sf::st_as_sf(poly.adm2)
      points_sf <- sf::st_as_sf(geo)

      if( !any(sf::st_is_valid(poly.adm1)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}
      if( !any(sf::st_is_valid(poly.adm2)) ){stop('sf object not valid, consider validating it via sf::st_make_valid')}

      # Select required columns and filter out wrong points
      cluster.info <- points_sf %>%
        select(cluster = DHSCLUST,
               LONGNUM,
               LATNUM,
               ADM1NAME ) #%>%



      admin1.sf <- st_join(cluster.info, poly.adm1) %>%
        sf::st_transform(st_crs(poly.adm1)) # Transform CRS if needed

      cluster.info$admin1.name.s1 <- as.data.frame( admin1.sf)[,by.adm1]

      # Spatial join for admin2
      admin2.sf <- st_join(cluster.info, poly.adm2) %>%
        sf::st_transform(st_crs(poly.adm2)) # Transform CRS if needed

      if(dim(admin2.sf)[1] > dim(cluster.info)[1]){
        admin2.sf <- admin2.sf[!duplicated(admin2.sf[,c('cluster')]),]
        warning('overlapping admin regions exist, the first match is kept')
      }

      cluster.info$admin2.name <- as.data.frame( admin2.sf)[,by.adm2]
      cluster.info$admin1.name <- as.data.frame( admin2.sf)[,by.adm1]


      wrong.points <- cluster.info[which(abs(cluster.info$LATNUM) <
                                           1e-07 & abs(cluster.info$LONGNUM) <1e-07), ]$cluster

      wrong.points <- unique(c(wrong.points,cluster.info[ which(is.na(cluster.info$admin1.name)),]$cluster))


      # wrong.points case 1: coordinates with LATNUM and LONGNUM < 1e-07
      # wrong.points case 2: points outside the country boundary due to jittering
      # wrong.points case 3: mismatches between Admin 1 and Admin 2 caused by inconsistent boundaries

      #step 1: Assign the wrong points (case 2) to the closest boundary
      result.wrong.points1=c()
      result.wrong.points2=c()

      if(!length(wrong.points) == 0){
        pts <- geo %>% filter(DHSCLUST %in% wrong.points)

        # sanity: assign CRS if missing and make polygons valid
        if (is.na(st_crs(pts)))        st_crs(pts)      <- 4326
        if (is.na(st_crs(poly.adm2)))  st_crs(poly.adm2) <- 4326

        # project to a metric CRS for accurate distances
        crs_m   <- .pick_utm(poly.adm2)
        pts_m   <- st_transform(pts, crs_m)
        adm2_m  <- st_transform(poly.adm2, crs_m)

        # distance from each point to the boundary of every admin1 polygon
        bnd_m   <- st_boundary(adm2_m)                       # MULTILINESTRING per admin1
        D       <- st_distance(pts_m, bnd_m)                 # matrix: n_points x n_admin1
        nearest_idx  <- apply(D, 1, which.min)
        min_dist_m   <- apply(D, 1, min)

        result <- pts %>%
          st_drop_geometry() %>%
          mutate(
            nearest_admin2 = adm2_m$NAME_2[nearest_idx],
            nearest_admin1 = adm2_m$NAME_1[nearest_idx],
            dist_km_to_boundary = as.numeric(min_dist_m) / 1000
          )

        result$match=result$nearest_admin1==result$ADM1NAME
        result.wrong.points2<- result[ result$dist_km_to_boundary < 10,]
        result.wrong.points1<- result[! result$dist_km_to_boundary < 10,]

        fixed.points<- result[isTRUE(result$match) | result$dist_km_to_boundary < 10,]$DHSCLUST
        wrong.points <- wrong.points[!wrong.points %in% fixed.points]

        if(sum(fixed.points)>0){
          message("Assign to the closest Admin 2 for points outside the country boundary due to jittering.")

          cluster.info[cluster.info$cluster %in% fixed.points, "admin1.name"]<- adm2_m[adm2_m$NAME_2 %in% result$nearest_admin2,]$NAME_1
          cluster.info[cluster.info$cluster %in% fixed.points, "admin2.name"]<- result[result$DHSCLUST %in% fixed.points, "nearest_admin2"]
          cluster.info[cluster.info$cluster %in% fixed.points, "admin1.name.s1"]<-  result[result$DHSCLUST %in% fixed.points, "nearest_admin1"]

           # cluster.info[cluster.info$cluster %in% fixed.points, "admin2.name.full"] <- paste0(cluster.info[cluster.info$cluster %in% fixed.points, "admin1.name"], "_",  cluster.info[cluster.info$cluster %in% fixed.points, "admin2.name"])

        }

        cluster.info <- cluster.info %>%
          mutate(admin2.name.full =  paste0(admin1.name, "_", admin2.name))

        if( any(result$match==FALSE) && any(abs(result$LATNUM) > 1e-07) && any(abs(result$LONGNUM) >1e-07)){
          message("Take a closer look of wrong points")
        }

      }


      cluster.info <- cluster.info[!(cluster.info$cluster %in% wrong.points),]


      #step 2: mismatched based on admin 2  case 3

      cluster.info <- cluster.info %>%
        mutate(admin2.name.full =  paste0(admin1.name, "_", admin2.name))
        # %>%   # create the column first
        # mutate(
        #   admin2.name.full = if_else(
        #     admin1.name == admin1.name.s1,
        #     paste0(admin1.name.s1, "_", admin2.name),
        #     paste0(admin1.name, "_", admin2.name)
        #   )
        # )
      mis= cluster.info[cluster.info$admin1.name!= cluster.info$admin1.name.s1,]

      cluster.info$admin1.name.s1=cluster.info$ADM1NAME=NULL



      if(!any(is.na(mis$cluster))){
        fixed.points=c(fixed.points, mis$cluster)
      }



      if(map==TRUE){
        # Choose a suitable projected CRS
        # For Africa, EPSG:3395 (World Mercator) or EPSG:3857 works fine
        # Or use an Africa Albers equal-area projection: 102022
        poly_proj <- st_transform(poly.adm1, 3395)

        # Compute points safely
        points_inside <- st_point_on_surface(poly_proj)

        if (any(!is.na(mis$cluster))) {
          # Convert back to lon/lat if you need to plot with WGS84 data
          points_inside <- st_transform(points_inside, 4326)
          mis <- st_transform(mis, st_crs(poly.adm1))

          geo_no0<-geo[geo$DHSCLUST %in% cluster.info[which(abs(cluster.info$LATNUM) >
                                                              1e-07 & abs(cluster.info$LONGNUM)  >1e-07), ]$cluster,]



          geo_join <- st_join(geo_no0, poly.adm1, left = TRUE) %>%
            mutate(outside = is.na(NAME_1))


          # Combine all cluster points
          pts_all <- bind_rows(
            st_drop_geometry(filter(geo_join, !outside)) |> mutate(type = "No issue"),
            st_drop_geometry(filter(geo_join,  outside)) |> mutate(type = "Outside"),
            st_drop_geometry(mis)                        |> mutate(type = "Mismatch")
          )

          levels <- c("No issue", "Outside", "Mismatch")

         map<- ggplot() +
            geom_sf(data = poly.adm1, color = "red", fill = NA, linewidth = 0.6, alpha = 0.9) +
            geom_sf(data = poly.adm2, fill = NA, color = "#2C7FB8", linewidth = 0.4) +

            geom_point(
              data = pts_all,
              aes(x = LONGNUM, y = LATNUM, shape = type, color = type, size = type),
              alpha = 1
            ) +
            # geom_sf_text(data = mis, aes(label = cluster), size = 2.8, nudge_y = 0.1) +

            scale_shape_manual(
              name = "Cluster Type",
              breaks = levels,
              values = c("No issue" = 1, "Outside" = 17, "Mismatch" = 18)
            ) +
            scale_color_manual(
              name = "Cluster Type",
              breaks = levels,
              values = c("No issue" = "darkblue", "Outside" = "seagreen", "Mismatch" = "seagreen")
            ) +
            scale_size_manual(
              name = "Cluster Type",
              breaks = levels,
              values = c("No issue" = 1, "Outside" = 3, "Mismatch" = 3)
            ) +
            guides(size = guide_legend(override.aes = list(alpha = 1))) +
            theme_bw() +
            theme(legend.position = "right")

        }else{

          geo_no0<-geo[geo$DHSCLUST %in% cluster.info[which(abs(cluster.info$LATNUM) >
                                                              1e-07 & abs(cluster.info$LONGNUM)  >1e-07), ]$cluster,]



          geo_join <- st_join(geo_no0, poly.adm1, left = TRUE) %>%
            mutate(outside = is.na(NAME_1))



          # Build one points data frame with a common 'type' column
          pts_no  <- st_drop_geometry(dplyr::filter(geo_join, !outside)) |> mutate(type = "No issue")
          pts_out <- st_drop_geometry(dplyr::filter(geo_join,  outside)) |> mutate(type = "Outside")

          pts_all <- dplyr::bind_rows(pts_no, pts_out)

          levels <- c("No issue", "Outside")

          map=ggplot() +
            geom_sf(data = poly.adm1, color = "red", fill = NA, linewidth = 0.6, alpha = 0.9) +
            geom_sf(data = poly.adm2, fill = NA, color = "#2C7FB8", linewidth = 0.4) +

            # ONE layer for points → one combined legend
            geom_point(
              data = pts_all,
              aes(x = LONGNUM, y = LATNUM, shape = type, color = type, size = type),
              alpha = 1
            ) +
            scale_shape_manual(
              name = "Cluster Type",
              breaks = levels,
              values = c("No issue" = 1, "Outside" = 17)
            ) +
            scale_color_manual(
              name = "Cluster Type",
              breaks = levels,
              values = c("No issue" = "darkblue", "Outside" = "seagreen")
            ) +
            scale_size_manual(
              name = "Cluster Type",
              breaks = levels,
              values = c("No issue" = 1, "Outside" = 3 )
            ) +
            guides(size = guide_legend(override.aes = list(alpha = 1))) +
            theme_bw() +
            theme(legend.position = "right")

#
#           map <- ggplot() +
#             geom_sf(data = poly.adm1, color = "red", fill = NA, linewidth = 0.6, alpha = 0.9) +
#             geom_sf(data = poly.adm2, fill = NA, color = "#2C7FB8", linewidth = 0.4) +
#
#             geom_point(
#               data = st_drop_geometry(dplyr::filter(geo_join, !outside)),
#               aes(x = LONGNUM, y = LATNUM, shape = "No issue", color = "No issue"),
#               size = 1.5, alpha = 0.7
#             ) +
#             geom_point(
#               data = st_drop_geometry(dplyr::filter(geo_join, outside)),
#               aes(x = LONGNUM, y = LATNUM, shape = "Outside", color = "Outside"),
#               size = 2.5, alpha = 0.9
#             ) +
#
#             scale_shape_manual(
#               name = "Cluster Type",
#               values = c("No issue" = 1, "Outside" = 3)
#             ) +
#             scale_color_manual(
#               values = c("No issue" = "darkblue", "Outside" = "red")
#               # ,
#               # guide = "none"   # hide color legend
#             ) +
#             theme_bw() +
#             theme(legend.position = "right")

        }
      }else{
        map=NULL
      }

      # return(cluster.info)
      cluster.info<-as.data.frame(cluster.info)


      return(list(data=cluster.info,
                  wrong.points = wrong.points,
                  fixed.points=fixed.points,
                  map=map,
                  result.wrong.points1=result.wrong.points1,
                  result.wrong.points2=result.wrong.points2,
                  result.wrong.points3=mis))


  }




}

