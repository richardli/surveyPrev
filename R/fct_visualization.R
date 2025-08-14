###############################################################
### leaflet (interactive) prevalence map for any subnational level
###############################################################
#' visualization_helpers
#'
#' @description produce interactive map for any subnational level, with the option to focus on one admin-1 level
#'
#' @param res.obj result object from surveyPrev
#'
#' @param poly.shp polygon file for plotting
#' 
#' @param admin1.focus if needed, the admin 1 level area name to create an individual focused prevalence map on
#'
#' @param color.palette which palette to use for plotting
#'
#' @param color.reverse whether to use reverse color scale
#' 
#' @param value.to.plot statistics to appear on the map
#'
#' @param value.range what range to plot, useful if want to compare plots using the same scale
#'
#' @param num_bins number of bins on the legend (what displayed might not be exact)
#'
#' @param legend.label label for the legend, such as 'Coefficient of Variation'
#'
#' @param no.hatching whether to hatch region with problematic uncertainties, recommend F (depends on rgeos package, set to T if not installed)
#'
#' @param map.title title for the map
#'
#' @param use.basemap what basemap to use 'OSM', if NULL, no basemap
#'
#' @param threshold.p cutoff for the exceedance probability map
#'
#' @return leaflet map object
#' 
#' @importFrom magrittr %>%
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4")
#' poly.adm2 <- sf::st_as_sf(ZambiaAdm2)
#' admin.info2 <- adminInfo(poly.adm = poly.adm2, 
#'                          admin = 2, 
#'                          by.adm = "NAME_2", 
#'                          by.adm.upper = "NAME_1")

#' cl_res_ad2 <- clusterModel(data=data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   model = "bym2",
#'                   admin = 2)
#' prevMap.adm2.central <- prevMap.leaflet(res.obj=res_ad2, 
#'                                               poly.shp=poly.adm2,
#'                                               admin1.focus = "Central")
#' } 
#' 

prevMap.leaflet <- function(res.obj,
                            poly.shp,
                            admin1.focus = NULL,
                            color.palette = NULL,
                            value.to.plot = 'mean',
                            value.range = NULL,
                            num_bins=NULL,
                            legend.label = 'Estimates',
                            map.title = NULL,
                            color.reverse = T,
                            no.hatching= F,
                            hatching.density=12,
                            use.basemap= NULL,
                            threshold.p=NULL,
                            legend.color.reverse= T){
  
  ########################################################
  ### check required packages
  ########################################################
  
  ### get admin levels from admin.info, by surveyPrev definition
  by.adm2 <- res.obj$admin.info$by.adm
  by.adm1 <- ifelse(is.null(res.obj$admin.info$by.adm.upper), by.adm2, 
                    res.obj$admin.info$by.adm.upper)
  adm_level <- res.obj$admin
  
  if (!requireNamespace("leaflegend", quietly = TRUE)) {
    stop("Package 'leaflegend' is required for this function. Please install it with install.packages('leaflegend').")
  }
  
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required for this function. Please install it with install.packages('viridisLite').")
  }
  
  ########################################################
  ### initialize parameters
  ########################################################
  
  poly.shp <- sf::st_as_sf(poly.shp)
  
  
  ########################################################
  ### prepare to.plot data set
  ########################################################
  ##
  
  survey.res <- res.obj[[paste0("res.admin", adm_level)]]
  
  if (adm_level == "0") {
    post.samp.mat <- NULL
  } else {
    post.samp.mat <- res.obj[[paste0("admin", adm_level, "_post")]]
  }
  
  
  res.to.plot <- harmonize_all_cols(survey.res=survey.res)
  #res.to.plot <- format_tab_num(survey.res=res.to.plot)
  #res.to.plot$value <- res.to.plot[[value.to.plot]] ### name the variable to plot as value
  
  ### prepare exceedance probabilities
  if(value.to.plot=='exceed_prob'){
    
    post.samp.mat <- res.obj[[paste0('admin',adm_level,'_post')]]
    
    if(is.null(post.samp.mat)){stop('No posterior samples provided, cannot produce exceedance probability map.')}
    if(is.null(threshold.p)){stop('No threshold provided, cannot produce exceedance probability map.')}
    
    ### process posterior samples to be in the correct format
    post.samp.mat <- as.matrix(post.samp.mat)
    
    n.samp= 1000
    if(dim(post.samp.mat)[2]==n.samp&dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
      post.samp.mat <- t(post.samp.mat)
    }
    
    # threshold.p <- 0.1
    
    res.to.plot$exceed_prob <- apply(post.samp.mat,2,function(x){sum(x>threshold.p)/length(x)})
    
    ### if no valid uncertainty measure, assign NA to exceedance probability
    if(sum(is.na(res.to.plot$var))>0){
      res.to.plot[is.na(res.to.plot$var),]$exceed_prob <- NA
    }
    
  }
  
  
  ########################################################
  ### merge results with spatial dataset
  ########################################################
  ##
  ##  if user hope to create map of specific admin 1 area, process the            shapefile accordingly
  if (!is.null(admin1.focus)) {
    poly.shp <- poly.shp[poly.shp[[by.adm1]] == admin1.focus, ]
    res.to.plot <- res.to.plot[res.to.plot$upper.adm.name == admin1.focus, ]
  }
  
  ## merge results
  if (adm_level == 0) {
    res.to.plot$region.name <- 'National'
    poly.shp$full_name <- 'National'
    
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name
  }
  if (adm_level == 1) {
    poly.shp$full_name <- poly.shp[[by.adm2]]
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name
  } 
  
  if (adm_level == 2){
    poly.shp$full_name <- paste0(poly.shp[[by.adm1]], "_", poly.shp[[by.adm2]])
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))
    
    gadm.with.res$region.name <- gadm.with.res[[by.adm2]]
    gadm.with.res$upper.adm.name <- gadm.with.res[[by.adm1]]
  }
  
  
  ### modify the format of numeric values and add warning messages
  gadm.with.res$warnings <- NA
  
  ### problematic sd, warnings and set uncertainty related measure to NA
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(!is.na(mean) &is.na(sd),
                                            "Data in this region are insufficient for <br/> reliable estimates with the current method.",
                                            NA)
    )
  
  ### no data warning
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(is.na(mean),
                                            "No data in this region",
                                            warnings))
  
  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value
  
  ### cv to %
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(cv = sprintf("%.1f%%", cv * 100))
  
  
  if(value.to.plot=='exceed_prob'){
    gadm.with.res <- gadm.with.res %>%
      dplyr::mutate(exceed_prob = sprintf("%.1f%%", exceed_prob * 100))
  }
  
  ### formatting numeric variables to 2 decimal places
  
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(across(c(mean, var, lower, upper,CI.width), ~sprintf("%.2f", .)))
  
  
  ########################################################
  ### hatching for problematic sd
  ########################################################
  
  hatching.ind <- T
  
  hatching.gadm <- gadm.with.res %>%
    subset( is.na(sd) & (!is.na(value)))
  
  ### no hatching if all regions have reasonable sd or manually set
  if(dim(hatching.gadm)[1]==0 | (no.hatching)){
    hatching.ind <- F
  }else{
    
    ### setup hatching polygons
    hatching.regions <- hatched.SpatialPolygons(hatching.gadm,
                                                density = c(hatching.density), angle = c(45))
    
    ### setup hatching legend
    warning.icon <- leaflet::awesomeIconList(
      'Sparse Data' =leaflet::makeAwesomeIcon(icon = "align-justify", library = "glyphicon",
                                              iconColor = 'gray',
                                              markerColor = 'white',
                                              squareMarker = TRUE, iconRotate = 135)
    )
  }
  
  #############################################
  ### parameters for color scale and breaks
  #############################################
  
  ### determine color palette for statistics, if not pre-specified
  ### r built-in palette
  
  if(is.null(color.palette)){
    color.palette='viridis'
    if(value.to.plot==c('mean')){
      color.palette = 'viridis'
    }
    if(value.to.plot==c('cv')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::mako(10)[2:10]
    }
    if(value.to.plot==c('CI.width')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::plasma(10)[3:10]
    }
    if(value.to.plot==c('exceed_prob')){
      #color.palette = 'cividis'
      #color.palette = viridisLite::cividis(10)
      color.palette = viridisLite::rocket(10)[3:10]
    }
  }
  
  ### determine value range if not specified, also create legend data
  if(is.null(value.range)){
    
    value.range <-  gadm.with.res$value
    
    if(value.to.plot=='exceed_prob'){
      value.range <- c(-0.001,1.001)
    }
    
    ### if no range specified, use data to determine limits for color schemes
    legend.dat <- gadm.with.res
    
    if(max(gadm.with.res$value,na.rm=T)-min(gadm.with.res$value,na.rm=T)<0.005){
      new.max <- min(1, max(gadm.with.res$value,na.rm=T)+0.005)
      new.min <- max(0, min(gadm.with.res$value,na.rm=T)-0.005)
      
      legend.dat <- data.frame(value=seq(new.min,new.max,length.out	=10),ID=c(1:10))
      
    }
    
  }else{
    ### if range specified, use range determine limits for color schemes
    legend.dat <- data.frame(value=seq(value.range[1],value.range[2],length.out	=10),
                             ID=c(1:10))
    
  }
  
  ### number of ticks on the legend
  if(is.null(num_bins)){
    if(value.to.plot=='exceed_prob'){
      num_bins <- 6
      
    }else{
      num_bins <- min(round( (max(gadm.with.res$value,na.rm=T)-min(gadm.with.res$value,na.rm=T))/0.1),6)
      num_bins <- max(4,num_bins)
    }
  }
  
  ### color palette
  if(value.to.plot=='exceed_prob'){
    pal <- leaflet::colorNumeric(palette = color.palette,
                                 domain = value.range,
                                 #na.color = '#9370DB',
                                 na.color = '#AEAEAE',
                                 reverse = color.reverse)
    
    ### whether to reverse color scheme on legend (for fixing bugs)
    if(!legend.color.reverse){legend.reverse=color.reverse}else{legend.reverse=!color.reverse}
    
    pal.legend <- leaflet::colorNumeric(palette = color.palette,
                                        domain = value.range,
                                        #na.color = '#9370DB',
                                        na.color = '#AEAEAE',
                                        reverse = legend.reverse)
    
    
  }else{
    pal <- leaflet::colorNumeric(palette = color.palette,
                                 domain = value.range,
                                 na.color = '#AEAEAE',
                                 reverse = color.reverse)
    
    ### whether to reverse color scheme on legend (for fixing bugs)
    
    if(!legend.color.reverse){legend.reverse=color.reverse}else{legend.reverse=!color.reverse}
    
    pal.legend <- leaflet::colorNumeric(palette = color.palette,
                                        domain = value.range,
                                        #na.color = '#9370DB',
                                        na.color = '#AEAEAE',
                                        reverse = legend.reverse)
    
  }
  
  numberFormat = function(x) {
    prettyNum(x, format = "f", big.mark = ",", digits =
                3, scientific = FALSE)
  }
  
  if(value.to.plot %in% c('cv','exceed_prob')){
    
    numberFormat = function(x) {
      paste0(formatC(100 * x, format = 'f', digits = 1), "%")
    }
    
  }
  
  ###############################################
  ### hovering effect, information to display
  ###############################################
  
  hover_labels <- gadm.with.res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hover_label = {
      label <- paste0('Region: ', region.name, '<br/>')
      
      if (!is.null(by.adm1) && !is.null(by.adm2) && by.adm1 != by.adm2) {
        label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
      }
      
      if(value.to.plot=='exceed_prob'){
        label <- paste0(label,  'Prob (prevalence > ',threshold.p,') = ', exceed_prob, '<br/>')
      }
      
      label <- paste0(label,
                      'Mean (95% CI): ', mean, ' (', lower, ', ', upper, ')', '<br/>',
                      'Coefficient of Variation: ', cv, '<br/>')
      if (!is.na(warnings) && warnings != "") {
        label <- paste0(label, '<span style="color: red;">Warning: ', warnings, '</span><br/>')
      }
      htmltools::HTML(label)  # Ensure that HTML rendering is applied
    }) %>%
    dplyr::ungroup() %>%
    dplyr::pull(hover_label)
  
  
  
  ###############################################
  ### assemble
  ###############################################
  
  ### base map
  adm.map <- gadm.with.res  %>% leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap = 0.1))
  
  adm.map <- add_basemap(original.map=adm.map,
                         static.ind= F,
                         basemap.type =use.basemap)
  
  #if(use.basemap=='OSM'){ adm.map <- adm.map %>%  leaflet::addTiles()}
  
  
  
  adm.map <- adm.map %>%
    leaflet::addPolygons(
      fillColor = ~pal(value),
      weight = 1,
      color = "gray",
      fillOpacity = 1,
      opacity = 1,
      label = ~ hover_labels, # display hover label
      labelOptions = leaflet::labelOptions(
        style = list("color" ="black"),  # Text color
        direction = "auto",
        textsize = "15px",
        noHide = F,  # Label disappears when not hovering
        offset = c(0,0)  # Adjust label position if necessary
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.75,
        bringToFront = TRUE,
        sendToBack=T)
    )
  
  ### add legend
  missingLabel <- ifelse(value.to.plot=='mean', 'No Data', 'N/A')
  
  adm.map <- adm.map %>%
    leaflegend::addLegendNumeric(pal = pal.legend, values = ~value, title =  htmltools::HTML(legend.label),
                                 orientation = 'vertical', fillOpacity = .7,
                                 position = 'bottomright', group = 'Symbols',
                                 width=25,height=150,naLabel = missingLabel,
                                 data=legend.dat,
                                 bins = num_bins, # Custom tick positions
                                 numberFormat=numberFormat,
                                 decreasing=T
    )
  
  if(hatching.ind){
    
    adm.map <- adm.map %>% leaflet::addPolylines(
      data = hatching.regions,
      color = c( "gray"),
      weight = 2.0,
      opacity = 0.8
    )
    adm.map <- adm.map %>% leaflegend::addLegendAwesomeIcon(iconSet = warning.icon,
                                                            title = 'Interpret with caution:',
                                                            position = 'bottomright')
    
  }
  
  ### add title
  
  if(!is.null(map.title)){
    
    tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.65);
    font-weight: bold;
    font-size: 20px;
    }
    "))
    
    title <- tags$div(
      tag.map.title, HTML(paste0(map.title))
    )
    
    adm.map <- adm.map %>%
      leaflet::addControl(title, position = "topleft", className="map-title")
  }
  
  return(adm.map)
}


###############################################################
### static prevalence map for any subnational level
###############################################################
#' visualization_helpers
#'
#' @description produce static map for any subnational level, with the option to focus on one admin-1 level
#'
#' @param res.obj result object from surveyPrev
#'
#' @param poly.shp polygon file for plotting
#' 
#' @param admin1.focus if needed, the admin 1 level area name to create an individual focused prevalence map on
#'
#' @param color.palette which palette to use for plotting
#'
#' @param color.reverse whether to use reverse color scale
#' 
#' @param value.to.plot statistics to appear on the map
#'
#' @param value.range what range to plot, useful if want to compare plots using the same scale
#'
#' @param legend.label label for the legend, such as 'Coefficient of Variation'
#'
#' @param map.title title for the map
#'
#' @param threshold.p cutoff for the exceedance probability map
#'
#' @return summer map object
#'
#' @importFrom magrittr %>%
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4")
#' admin.info2 <- adminInfo(poly.adm = sf::st_as_sf(ZambiaAdm2), 
#'                          admin = 2, 
#'                          by.adm = "NAME_2", 
#'                          by.adm.upper = "NAME_1")
#' cl_res_ad2 <- clusterModel(data=data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   model = "bym2",
#'                   admin = 2)
#'
#' prevMap.adm2.central <- prevMap.static(res.obj=res_ad2, 
#'                                        poly.shp=poly.adm2,
#'                                        admin1.focus = "Central")
#' } 
#' 

prevMap.static <- function(res.obj,
                           poly.shp ,
                           admin1.focus = NULL,
                           value.to.plot = 'mean',
                           map.title = NULL,
                           legend.label = 'Estimates',
                           threshold.p = NULL,
                           color.palette = NULL,
                           color.reverse = T,
                           value.range=NULL,
                           ...){
  
  
  ########################################################
  ### initialize parameters
  ########################################################
  
  poly.shp <- sf::st_as_sf(poly.shp)
  
  ### get admin levels from admin.info, by surveyPrev definition
  by.adm2 <- res.obj$admin.info$by.adm
  by.adm1 <- ifelse(is.null(res.obj$admin.info$by.adm.upper), by.adm2, 
                    res.obj$admin.info$by.adm.upper)
  adm_level <- res.obj$admin
  
  ### determine color scheme if not specified
  
  if(is.null(color.palette)){
    
    color.palette = viridisLite::viridis(10)
    
    if(value.to.plot==c('mean')){
      color.palette = viridisLite::viridis(10)
    }
    if(value.to.plot==c('cv')){
      color.palette = viridisLite::mako(10)[2:10]
    }
    if(value.to.plot==c('CI.width')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::plasma(10)[3:10]
    }
    if(value.to.plot==c('exceed_prob')){
      #color.palette = viridisLite::cividis(10)
      color.palette = viridisLite::rocket(10)[3:10]
      
    }
    
    ### whether to reverse color scale
    if(color.reverse){color.palette <- rev(color.palette)}
    
  }
  
  ########################################################
  ### prepare to.plot data set
  ########################################################
  
  survey.res <- res.obj[[paste0('res.admin',adm_level)]]
  
  res.to.plot <- harmonize_all_cols(survey.res=survey.res)
  
  ### prepare exceedance probabilities
  if(value.to.plot=='exceed_prob'){
    
    post.samp.mat <- res.obj[[paste0('admin',adm_level,'_post')]]
    
    if(is.null(post.samp.mat)){stop('No posterior samples provided, cannot produce exceedance probability map.')}
    if(is.null(threshold.p)){stop('No threshold provided, cannot produce exceedance probability map.')}
    
    ### process posterior samples to be in the correct format
    post.samp.mat <- as.matrix(post.samp.mat)
    
    n.samp= 1000
    if(dim(post.samp.mat)[2]==n.samp&dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
      post.samp.mat <- t(post.samp.mat)
    }
    
    # threshold.p <- 0.1
    
    res.to.plot$exceed_prob <- apply(post.samp.mat,2,function(x){sum(x>threshold.p)/length(x)})
    
  }
  
  ########################################################
  ### merge to spatial data
  ########################################################
  
  # focus on given admin 1 level area map if user specified
  if (!is.null(admin1.focus)) {
    poly.shp <- poly.shp[poly.shp[[by.adm1]] == admin1.focus, ]
    res.to.plot <- res.to.plot[res.to.plot$upper.adm.name == admin1.focus, ]
  }
  
  if(adm_level==0){
    res.to.plot$region.name <- 'National'
    poly.shp$full_name <- 'National'
    
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name
    
  }
  
  if(adm_level==1){
    poly.shp$full_name <- poly.shp[[by.adm2]]
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    
  }
  
  if(adm_level==2){
    poly.shp$full_name <- paste0(poly.shp[[by.adm1]], "_", poly.shp[[by.adm2]])
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))
  }
  
  
  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value
  gadm.with.res$method <- ''
  
  ### determine value range if not specified, also create legend data
  if(is.null(value.range)){
    
    value.range <-  c(min(gadm.with.res$value,na.rm=T),
                      max(gadm.with.res$value,na.rm=T))
    
    if(value.to.plot=='exceed_prob'){
      value.range <- c(0,1)
    }
    
  }
  
  
  
  static.map <- SUMMER::mapPlot(gadm.with.res, variables = "method", values = "value",
                                by.data = "full_name", geo = poly.shp,
                                by.geo = "full_name", is.long = TRUE,
                                removetab = T,...)+
    ggplot2::theme (legend.text=ggplot2::element_text(size=12),
                    legend.title = ggplot2::element_text(size=14),
                    strip.text.x = ggplot2::element_text(size = 12),
                    legend.key.height = ggplot2::unit(1,'cm') )
  
  
  if(value.to.plot=='exceed_prob'){
    #na.color = 'white'
    na.color = 'grey50'
  }else{
    na.color = "grey50"
  }
  
  
  if(value.to.plot %in% c('cv','exceed_prob')){
    #static.map <- static.map+viridis::scale_fill_viridis(option=color.palette,limits=value.range,
    #                                                    direction=direction,labels = scales::label_percent())
    static.map <- static.map+ggplot2::scale_fill_gradientn(colours = color.palette,limits=value.range,
                                                           labels = scales::label_percent(),
                                                           name=paste0(legend.label,'\n'),
                                                           na.value = na.color)
  }else{
    #static.map <- static.map+viridis::scale_fill_viridis(option=color.palette,limits=value.range,
    #                                                     direction=direction)
    static.map <- static.map+ggplot2::scale_fill_gradientn(colours=color.palette,limits=value.range,
                                                           labels = scales::label_number(accuracy = 0.01),
                                                           name=paste0(legend.label,'\n'),
                                                           na.value = na.color)
  }
  
  
  return(static.map)
  
  
}


###############################################################
### ridge plot
###############################################################
#'
#' @description static ridge plot for posterior densities
#'
#' @param res.obj result object from surveyPrev
#'
#' @param admin1.focus whether to plot densities for regions within a single upper admin
#'
#' @param plot.extreme.num number of regions to plot for the top n and bottom n regions
#'
#' @param legend.label label on the legend
#'
#' @param color.reverse whether to reverse color scheme
#'
#' @param plot.format c('Long','Wide') for extreme regions, side-by-side or long plot
#'
#' @param top.bottom.label c('Top','Bottom') how to name the extremes, top 10 bottom 10? need to change when close to 0 is bad for the indicator
#'
#' @return ggplot2 object
#'
#' @importFrom magrittr %>%
#'
#' @export
#' 
#' @example
#' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4")
#' poly.adm1 <- sf::st_as_sf(ZambiaAdm1)
#' admin.info1 <- adminInfo(poly.adm = poly.adm1, 
#'                          admin = 1, 
#'                          by.adm = "NAME_1")
#' cl_res_ad1 <- clusterModel(data=data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info1,
#'                   stratification = FALSE,
#'                   model = "bym2",
#'                   admin = 1,
#'                   aggregation = TRUE,
#'                   CI = 0.95)
#'  
#' posterior_ridge_plot(cl_res_ad1, plot.extreme.num=5)
#' 
#' }
#' 

posterior_ridge_plot <- function(res.obj,
                                 admin1.focus=NA ,
                                 plot.extreme.num=8,
                                 legend.label = 'Value',
                                 color.reverse= T,
                                 plot.format = c('Long','Wide')[1],
                                 top.bottom.label=c('Top','Bottom')
){
  
  ########################################################
  ### initialize parameters
  ########################################################
  ### get admin levels from admin.info, by surveyPrev definition
  adm_level <- res.obj$admin
  
  
  ### color scheme
  if(color.reverse){direction=-1}else{direction=1}
  
  ########################################################
  ### prepare posterior sample data set
  ########################################################
  
  
  n.samp=1000
  ### prepare plot data
  survey.res <- res.obj[[paste0('res.admin',adm_level)]]
  res.summary <- harmonize_all_cols(survey.res=survey.res)
  post.samp.mat <- res.obj[[paste0('admin',adm_level,'_post')]]
  
  
  ### process posterior samples to be in the correct format and
  post.samp.mat <- as.matrix(post.samp.mat)
  if(dim(post.samp.mat)[2]==n.samp&dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
    post.samp.mat <- t(post.samp.mat)
  }
  
  
  #message(post.samp.mat[1:10])
  
  if(adm_level==1){
    by.res = 'region.name'
    res.to.plot <- data.frame(region.name = rep(res.summary[, by.res],each= n.samp),
                              value = as.numeric(post.samp.mat))
    
    res.summary.to.match <- as.data.frame(res.summary)
    res.summary.to.match$order.name <- res.summary.to.match[,by.res]
    
    ## order.name is the same as region.name in res.to.plot,
    ## naming this way to not confuse with the original region.name in res.summary
    
  }
  
  if(adm_level==2){
    
    by.res = 'region.name.full'
    if(!is.na(admin1.focus)){
      res.summary[,by.res]<- paste0(res.summary$region.name)
    }else{
      res.summary[,by.res]<- paste0(res.summary$region.name,' (',res.summary$upper.adm.name,')')
    }
    
    
    res.to.plot <- data.frame(region.name = rep(res.summary[, by.res],each= n.samp),
                              upper.adm.name = rep(res.summary[, 'upper.adm.name'],each= n.samp),
                              value = as.numeric(post.samp.mat))
    
    
    if(!is.na(admin1.focus)){
      
      res.to.plot <- res.to.plot[res.to.plot$upper.adm.name ==admin1.focus,]
      
      
      if(dim(res.to.plot)[1]==0){
        message(paste0('wrong upper admin name - cannot plot ',admin1.focus))
        return()
      }
      
      res.summary.to.match <- as.data.frame(res.summary)
      res.summary.to.match <- res.summary.to.match[res.summary.to.match$upper.adm.name ==admin1.focus,]
      res.summary.to.match$order.name <- res.summary.to.match[,by.res]
      
    }else{
      
      res.summary.to.match <- as.data.frame(res.summary)
      res.summary.to.match$order.name <- res.summary.to.match[,by.res]
      
    }
    
    
  }
  
  
  ########################################################
  ### prepare order and plot data set
  ########################################################
  
  ### if not many regions, plot all even if specified plot only top and bottom k
  
  n.regions <- dim(res.summary.to.match)[1]
  
  if(!is.na(plot.extreme.num)){
    if(n.regions < 2*plot.extreme.num+1){
      plot.extreme.num=NA
    }
  }
  
  ### order regions
  
  order.num <- (order(res.summary.to.match[['median']]))
  #ordered.res.summary <- res.summary.to.match[order(as.numeric(median))]
  #ordered.res.summary <- res.summary.to.match[order(res.summary.to.match[['median']]),]
  region.name.vec <- res.summary.to.match[['order.name']]
  res.to.plot.order <- region.name.vec[order.num]
  
  # show only extreme regions
  if(!is.na(plot.extreme.num)){
    
    n.levels <- plot.extreme.num*2
    ridge_top_k_order <- res.to.plot.order[1:plot.extreme.num]
    ridge_bottom_k_order <- res.to.plot.order[(n.regions-plot.extreme.num+1):n.regions]
    
    ### top 10 regions
    top_k_plot_dt <- res.to.plot[res.to.plot$region.name %in% ridge_top_k_order, ]
    top_k_plot_dt$region.name = factor(top_k_plot_dt$region.name, levels = rev(ridge_top_k_order))
    #top_k_plot_dt$rank <- paste0(top.bottom.label[1],' ',plot.extreme.num, ' regions')
    top_k_plot_dt$rank <-  paste0(plot.extreme.num, top.bottom.label[1])
    
    
    ### top 10 regions
    bottom_k_plot_dt <- res.to.plot[res.to.plot$region.name %in% ridge_bottom_k_order, ]
    bottom_k_plot_dt$region.name = factor(bottom_k_plot_dt$region.name, levels = rev(ridge_bottom_k_order))
    #bottom_k_plot_dt$rank <- paste0(top.bottom.label[2],' ',plot.extreme.num, ' regions')
    bottom_k_plot_dt$rank <- paste0(plot.extreme.num, top.bottom.label[2])
    
    
    ### combine together
    res.to.plot <- rbind(top_k_plot_dt,bottom_k_plot_dt)
    #res.to.plot$rank <- factor(res.to.plot$rank, levels = c(paste0(top.bottom.label[1],' ',plot.extreme.num, ' regions'),
    #                                                       paste0(top.bottom.label[2],' ',plot.extreme.num, ' regions')))
    res.to.plot$rank <- factor(res.to.plot$rank, levels = c(paste0(plot.extreme.num, top.bottom.label[1]),
                                                            paste0(plot.extreme.num, top.bottom.label[2])))
    
  }
  
  
  # show all
  if(is.na(plot.extreme.num)){
    
    n.levels <- n.regions
    res.to.plot$region.name = factor(res.to.plot$region.name, levels = rev(res.to.plot.order))
    
  }
  
  
  ########################################################
  ### plot posterior density
  ########################################################
  
  
  ### set up ranges of values
  ridge.max <- max(res.to.plot$value)*1.03
  if(ridge.max>0.95){ridge.max=1}
  ridge.min <- min(res.to.plot$value)*0.97
  if(ridge.min<0.05){ridge.min=0}
  
  #density.height.scale <- max(15/length(res.to.plot.order),3)
  #density.height.scale <- min(density.height.scale,5)
  
  ### plot all regions
  if(is.na(plot.extreme.num)){
    ridge.plot.adm <- ggplot2::ggplot(res.to.plot, ggplot2::aes(x = value, y = region.name)) +
      ggridges::geom_density_ridges_gradient(ggplot2::aes(fill = ggplot2::after_stat(x)),
                                             scale= max(15/n.levels,3)) +
      ggplot2::scale_fill_viridis_c(legend.label, lim = c(ridge.min,ridge.max), direction = direction)
    
  }
  
  ### plot extreme regions
  if(!is.na(plot.extreme.num)){
    
    if(is.null(plot.format)){
      plot.format='Long'
    }
    
    if(plot.format=='Long'){
      ### long
      ridge.plot.adm <- ggplot2::ggplot(res.to.plot, ggplot2::aes(x = value, y = region.name)) +
        ggridges::geom_density_ridges_gradient(ggplot2::aes(fill = ggplot2::after_stat(x)),
                                               scale= max(15/n.levels,3)) +
        ggplot2::scale_fill_viridis_c(legend.label, lim = c(ridge.min,ridge.max), direction = direction)+
        ggplot2::facet_grid(rank ~ . ,scales = "free") +
        ggplot2::theme(panel.spacing = ggplot2::unit(1.5, "lines"))
      
    }
    
    
    if(plot.format=='Wide'){
      ### long
      ridge.plot.adm <- ggplot2::ggplot(res.to.plot, ggplot2::aes(x = value, y = region.name)) +
        ggridges::geom_density_ridges_gradient(ggplot2::aes(fill = ggplot2::after_stat(x)),
                                               scale= max(15/n.levels,3)) +
        ggplot2::scale_fill_viridis_c(legend.label, lim = c(ridge.min,ridge.max), direction = direction)+
        ggplot2::facet_wrap(rank ~ . ,scales = "free") +
        ggplot2::theme(panel.spacing = ggplot2::unit(1.5, "lines"))
      
    }
    
  }
  
  
  ### styles for the plot
  
  if(!is.na(plot.extreme.num) & plot.format=='Wide'& adm_level==2){
    y.text.size =13
  }else{y.text.size=16}
  
  ylabel <- ifelse(!is.null(admin1.focus), admin1.focus, "Region name")
  
  ridge.plot.adm <- ridge.plot.adm +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                   text = ggplot2::element_text(size=16),
                   strip.text = ggplot2::element_text(size=18),
                   axis.text.y = ggplot2::element_text(size = y.text.size,
                                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0))
    )+
    ggplot2::xlab("")+
    ggplot2::ylab(ylabel)+
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top",
                                                    title.hjust = .5,
                                                    title=legend.label,
                                                    label.position = "bottom"))+
    ggplot2::theme (legend.position = 'bottom',
                    legend.key.height= ggplot2::unit(0.5,'cm'),
                    legend.text= ggplot2::element_text(size=16),
                    legend.key.width = ggplot2::unit(2,'cm'),
                    legend.title = ggplot2::element_text(size=16),
                    legend.box.margin= ggplot2::margin(-15,0,0,0),
                    legend.margin=ggplot2::margin(0,0,0,0))
  
  
  return(ridge.plot.adm)
  
}

###############################################################
### scatter plot
###############################################################
#'
#' @description interactive scatter plot comparing estimates from two methods for the same admin level
#'
#' @param res.obj.x result object from surveyPrev
#'
#' @param res.obj.y result object from surveyPrev
#'
#' @param value.to.plot which statistics to plot 'mean'
#'
#' @param label.x label on x-axis
#'
#' @param label.y label on y-axis
#'
#' @param plot.title title for the plot
#'
#' @param interactive whether to render interactive or static plot
#'
#' @return plotly or ggplot2 object
#'
#' @importFrom magrittr %>%
#'
#' @export
#' 
#' @example
#' 
#' #' \dontrun{
#' geo <- getDHSgeo(country = "Zambia", year = 2018)
#' cluster.info <- clusterInfo(geo = geo,
#'                             poly.adm1 = ZambiaAdm1,
#'                             poly.adm2 = ZambiaAdm2)
#' dhsData <- getDHSdata(country = "Zambia",
#'                                  indicator = "ancvisit4+",
#'                                  year = 2018)
#' data <- getDHSindicator(dhsData, indicator = "ancvisit4")
#' poly.adm2 <- sf::st_as_sf(ZambiaAdm2)
#' admin.info2 <- adminInfo(poly.adm = poly.adm2, 
#'                          admin = 2, 
#'                          by.adm = "NAME_2", 
#'                          by.adm.upper = "NAME_1")

#' res_adm2_cl <- clusterModel(data=data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   model = "bym2",
#'                   admin = 2)
#'res_adm2_fh <- fhModel(data=data,
#'                   cluster.info = cluster.info,
#'                   admin.info = admin.info2,
#'                   model = "bym2",
#'                   admin = 2)
#' 
#' scatter.plot(res_adm2_cl, res_adm2_fh)
#' } 
#' 

scatter.plot <- function(res.obj.x,
                         res.obj.y ,
                         value.to.plot = 'mean',
                         label.x = 'Method 1 Estimates',
                         label.y = 'Method 2 Estimates',
                         plot.title=NULL,
                         interactive=T){
  
  ### get admin levels from admin.info, by surveyPrev definition
  adm_level <- res.obj.x$admin
  if(adm_level == 2){
    by.res = 'region.name.full'
  }else{
    by.res = 'region.name'
  }
  
  ########################################################
  ### prepare to.plot data set
  ########################################################
  
  ### prepare plot data
  survey.res.x <- res.obj.x[[paste0('res.admin',adm_level)]]
  if(is.null(survey.res.x)){return(NULL)} # do not plot if model not fitted
  res.to.plot.x <- harmonize_all_cols(survey.res=survey.res.x)
  
  survey.res.y <- res.obj.y[[paste0('res.admin',adm_level)]]
  if(is.null(survey.res.y)){return(NULL)} # do not plot if model not fitted
  res.to.plot.y <- harmonize_all_cols(survey.res=survey.res.y)
  
  
  ### label x and y
  res.to.plot.x$value_x <- res.to.plot.x[[value.to.plot]]
  res.to.plot.x <- res.to.plot.x[!is.na(res.to.plot.x$value_x),]
  res.to.plot.x$hover_x <- res.to.plot.x[[value.to.plot]] # in the case of missing data, to distinguish hover effect
  res.to.plot.x$label.x <- label.x
  
  res.to.plot.y$value_y <- res.to.plot.y[[value.to.plot]]
  res.to.plot.y <- res.to.plot.y[!is.na(res.to.plot.y$value_y),]
  res.to.plot.y$hover_y <- res.to.plot.y[[value.to.plot]] # in the case of missing data, to distinguish hover effect
  res.to.plot.y$label.y <- label.y
  
  
  ### color missing data
  if(length(res.to.plot.x$value_x)<length(res.to.plot.y$value_y)){
    missing <- subset(res.to.plot.y, !res.to.plot.y[[by.res]]%in% res.to.plot.x[[by.res]])
    
    if(value.to.plot %in% c('mean')){
      ### mean, missing on the left
      missing$value_x=rep(min(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }else{
      ### cv, CI.width, missing on the right
      missing$value_x=rep(max(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }
    
    missing$hover_x=rep(NA,dim(missing)[1])
    missing$hover_y=missing$value_y
    
  }else if( length( res.to.plot.x$value_x)>length(res.to.plot.y$value_y)){
    missing<-subset(res.to.plot.x, !res.to.plot.x[[by.res]] %in% res.to.plot.y[[by.res]])
    
    if(value.to.plot %in% c('mean')){
      missing$value_y=rep(min(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }else{
      missing$value_y=rep(max(c(res.to.plot.x$value_x, res.to.plot.y$value_y),na.rm=T),dim(missing)[1])
    }
    
    missing$hover_x=missing$value_x
    missing$hover_y=rep(NA,dim(missing)[1])
    
  }else if(length(res.to.plot.x$value_x)==length(res.to.plot.y$value_y)){
    
  }
  
  ### merge to one dataset
  ###
  if(adm_level == 2){
    res.to.plot.y <- res.to.plot.y[,!colnames(res.to.plot.y)%in% c('region.name','upper.adm.name')] # avoid same names
  }
  
  res.to.plot.all <- merge(res.to.plot.x, res.to.plot.y, by=by.res)
  
  
  ### make the plot
  if(interactive==F){dot.size = 3}else{dot.size=1.5}
  
  
  if(length(res.to.plot.x$value_x)==length(res.to.plot.y$value_y)){
    
    # missingness
    
    lim <- range(c(res.to.plot.all$value_x, res.to.plot.all$value_y), na.rm = TRUE)
    static.plot <- ggplot2::ggplot(res.to.plot.all, ggplot2::aes(x=value_x,y=value_y)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
      ggplot2::geom_point(alpha = 0.5, color = "royalblue",size=dot.size) +
      ggplot2::labs(title = plot.title)+
      ggplot2::xlab(label.x)+
      ggplot2::ylab(label.y)+
      ggplot2::xlim(lim) +
      ggplot2::ylim(lim) +
      ggplot2::theme_bw()
    
  }else{
    # no missing
    
    lim <- range(c(res.to.plot.all$value_x, res.to.plot.all$value_y), na.rm = TRUE)
    static.plot <- ggplot2::ggplot(res.to.plot.all,  ggplot2::aes(x=value_x,y=value_y)) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed")+
      ggplot2::geom_point(alpha = 0.5, color = "royalblue",size=dot.size) +
      ggplot2::geom_point(data = missing,  ggplot2::aes(x=value_x,y=value_y), color = "red", shape=17,size=dot.size)+
      ggplot2::labs(title = plot.title)+
      ggplot2::xlab(label.x)+
      ggplot2::ylab(label.y)+
      ggplot2::xlim(lim) +
      ggplot2::ylim(lim) +
      ggplot2::theme_bw()
  }
  
  
  ### percentage if coefficient of variation
  if(value.to.plot=='cv'){
    static.plot <- static.plot+
      ggplot2::scale_y_continuous(labels =  scales::percent,limits = lim)+
      ggplot2::scale_x_continuous(labels =  scales::percent,limits = lim)
    
  }
  
  
  if(interactive==F){
    
    static.plot <- static.plot+ggplot2::coord_fixed(ratio = 1)+
      ggplot2::theme(axis.text= ggplot2::element_text(size=14),
                     axis.title= ggplot2::element_text(size=15))
    
    
    
    return(static.plot)
  }
  
  ### hover effect
  if(adm_level == 2){
    
    if(value.to.plot=='cv'){
      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            'Upper admin name: ',upper.adm.name,
                                                            "</br>",
                                                            label.x,": ", round(100*hover_x,digits = 1),'%',
                                                            "</br>",
                                                            label.y,": ", round(100*hover_y,digits = 1),'%') )
      
    }else{
      
      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            'Upper admin name: ',upper.adm.name,
                                                            "</br>",
                                                            label.x,": ", round(hover_x,digits = 3),
                                                            "</br>",
                                                            label.y,": ", round(hover_y,digits = 3)) )
    }
    
    
  }else{
    if(value.to.plot=='cv'){
      
      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            label.x,": ", round(100*hover_x,digits = 1),'%',
                                                            "</br>",
                                                            label.y,": ",  round(100*hover_y,digits = 1),'%') )
      
    }else{
      static.plot <- static.plot +ggplot2::aes(text =paste0('Region name: ',region.name,
                                                            "</br></br>",
                                                            label.x,": ", round(hover_x,digits = 3),
                                                            "</br>",
                                                            label.y,": ", round(hover_y,digits = 3)) )
      
    }
    
  }
  
  ### make the plot interactive
  
  interactive.plot <- plotly::ggplotly(static.plot,
                                       tooltip = "text",height = 400, width = 500)%>%
    plotly::layout(margin= list(
      l = 80,
      r = 80,
      b = 20,
      t = 20,
      pad = 4
    ))
  
  
  return(interactive.plot)
  
}





#' visualization helper functions
#'
#' @description make the naming for one column the same for different methods
#'
#' @param survey.res result summary data.frame
#'
#' @param from_col possible column names for a feature, such as c("direct.est", "mean")
#'
#' @param to_col harmonized name for the column
#'
#' @return data.frame with modified column names
#'
#' @noRd
#'
#'

harmonize_one_col <- function(survey.res,
                              from_col,
                              to_col){
  
  # Find the first matching column in the list
  existingCol <- from_col[from_col %in% names(survey.res)][1]
  
  # Create harmonized column if found match, else assign NA
  survey.res[to_col] <- if (!is.null(existingCol)) survey.res[[existingCol]] else NA
  
  
  return(survey.res)
}


###############################################################
### harmonize column names
###############################################################
#' visualization_helpers
#'
#' @description make the naming for all columns the same for different methods and admin levels
#'
#' @param survey.res result summary data.frame
#'
#' @return data.frame with modified column names
#'
#' @noRd
#'
#'

harmonize_all_cols <- function(survey.res){
  
  ###### harmonize summary statistics
  
  stat_var <- c('mean','median','sd','var','lower','upper','CI.width','cv')
  ### mean
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.est','mean'),
                                 to_col = 'mean')
  
  ### sd
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.se','sd'),
                                 to_col = 'sd')
  
  ### var
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.var','var'),
                                 to_col = 'var')
  
  ### coefficient of variation
  survey.res$cv <- survey.res$sd/survey.res$mean
  
  
  ### lower CI
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.lower','lower'),
                                 to_col = 'lower')
  
  ### upper CI
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.upper','upper'),
                                 to_col = 'upper')
  
  
  ### CI width
  survey.res$CI.width <- survey.res$upper-survey.res$lower
  
  
  ### set problematic uncertainties to NA
  survey.res <- survey.res %>%
    dplyr::mutate(var = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,var),
                  lower = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,lower),
                  upper = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,upper),
                  cv = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,cv),
                  CI.width = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,CI.width)
    )
  survey.res <- survey.res %>%
    dplyr::mutate(sd = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,sd))
  
  
  ### for direct estimates, mean is median
  if(!'median' %in% names(survey.res)){
    survey.res$median <- survey.res$mean
  }
  
  ###### harmonize region variables
  
  ### national estimates
  if(!'admin1.name' %in% names(survey.res)& !'admin2.name.full'%in% names(survey.res)){
    
    survey.res <- survey.res[, stat_var[stat_var %in% names(survey.res)], drop = FALSE]
    
    return(survey.res)
  }
  
  
  ### estimates not finer than stratification level
  
  if(!'admin2.name.full' %in% names(survey.res)){
    
    survey.res$region.name <- survey.res$admin1.name
    
    res.var <- c('region.name',stat_var)
    
    survey.res <- survey.res[, res.var[res.var %in% names(survey.res)], drop = FALSE]
    
    return(survey.res)
  }
  
  ### estimates finer than stratification level
  
  if('admin2.name.full' %in% names(survey.res)){
    
    if('admin1.name' %in% names(survey.res)){
      survey.res$region.name <-  survey.res$admin2.name
      survey.res$upper.adm.name <- survey.res[['admin1.name']]
    }else{
      survey.res <- survey.res %>%
        tidyr::separate(admin2.name.full, into = c("upper.adm.name", "region.name"), sep = "_", remove = FALSE)
    }
    
    survey.res$region.name.full <- survey.res[['admin2.name.full']]
    
    res.var <- c('region.name',stat_var,'upper.adm.name','region.name.full')
    
    survey.res <- survey.res[, res.var[res.var %in% names(survey.res)], drop = FALSE]
    
    return(survey.res)
  }
  
}

###############################################################
### function to add basemap
###############################################################
#' @description produce interactive map for country boundaries
#'
#' @param original.map the object to add basemap on
#'
#' @param static.ind indicator of static (ggplot2) or interactive map (leaflet)
#'
#' @param basemap.type what basemap to use 'OSM' or 'WHO'
#'
#' @return leaflet/ggplot2 object
#'
#' @noRd
#'

add_basemap <- function(original.map,
                        static.ind= F,
                        basemap.type =NULL){
  
  
  if(is.null(basemap.type)){
    return(original.map)
  }
  
  if(basemap.type=='OSM'&static.ind==F){
    
    return.map <- tryCatch({
      original.map %>%  leaflet::addTiles()
    },error = function(e) {
      message(e$message)
      message('basemap did not load successfully')
      return.map <<- original.map
    })
    
  }else{
    
    return.map <- original.map
    
  }
  
  return(return.map)
  
}