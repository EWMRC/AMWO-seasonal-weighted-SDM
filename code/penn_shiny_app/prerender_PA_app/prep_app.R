library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(parallel)
library(pbapply)
library(leaflet)
library(colorspace)
library(RColorBrewer)
library(leaflet.providers)
library(leaflet.esri)

#Load the weighted rasters

m10r0 <- raster("m10r0.tif")
m9r1 <- raster("m9r1.tif")
m8r2 <- raster("m8r2.tif")
m7r3 <- raster("m7r3.tif")
m6r4 <- raster("m6r4.tif")
m5r5 <- raster("m5r5.tif")
m4r6 <- raster("m4r6.tif")
m3r7 <- raster("m3r7.tif")
m2r8 <- raster("m2r8.tif")
m1r9 <- raster("m1r9.tif")
m0r10 <- raster("m0r10.tif")

gamelands <- readRDS("gamelands.rds")

#Rendering statewide leaflet layers
walk2(.x = c(m10r0, m9r1, m8r2, m7r3, m6r4, m5r5, m4r6, m3r7, m2r8, m1r9, m0r10), .y = c("m10r0", "m9r1", "m8r2", "m7r3", "m6r4", "m5r5", "m4r6", "m3r7", "m2r8", "m1r9", "m0r10"), .f = function(iterated_layer, iterated_name){
  
  #iterated_layer <- m10r0 #
  #iterated_name <- "m10r0" #
  
  quantiles <- raster::quantile(iterated_layer, 1:100/100)
  
  intervals <- findInterval(getValues(iterated_layer), quantiles)
  iterated_layer <- setValues(iterated_layer, intervals)
  #iterated_layer <- iterated_layer*5 # scaling from 0 to 100
  
  #pal <- oompaBase::jetColors(100)
  pal_fun <- colorNumeric(palette = "RdYlBu", domain = c(min(values(iterated_layer), na.rm = TRUE), max(values(iterated_layer), na.rm = TRUE)), na.color = NA, reverse = TRUE)
  pal_fun_rev <- colorNumeric(palette = "RdYlBu", domain = c(min(values(iterated_layer), na.rm = TRUE), max(values(iterated_layer), na.rm = TRUE)), na.color = NA, reverse = FALSE)
  
  
  #pal_fun <- colorQuantile(palette = pal, domain = c(min(values(iterated_layer), na.rm = TRUE), max(values(iterated_layer), na.rm = TRUE)), na.color = NA, n = 10)
  
  if(iterated_name == "m10r0"){
    polygon_weight <- 1
  } else if (iterated_name == "m9r1"){
    polygon_weight <- 0.85
  } else if (iterated_name == "m8r2"){
    polygon_weight <- 0.7
  } else if(iterated_name == "m7r2"){
    polygon_weight <- 0.6
  } else {
    polygon_weight <- 0.5
  }
  
  iterated_gamelands <- gamelands %>%
    st_transform(4326) %>%
    mutate(relevant_si = pull(gamelands, paste0(iterated_name, "_", "si")),
           relevant_mean = pull(gamelands, paste0(iterated_name, "_", "mean")),
           relevant_high = pull(gamelands, paste0(iterated_name, "_", "high")),
           relevant_med = pull(gamelands, paste0(iterated_name, "_", "med")))
  
  
  leaflet() %>%
    #addTiles(group = "Street map") %>%
    addEsriBasemapLayer(esriBasemapLayers$Topographic, autoLabels = TRUE, group = "Topographic map") %>%
    addEsriBasemapLayer(esriBasemapLayers$Imagery, autoLabels = TRUE, group = "Satellite map") %>%
    addRasterImage(x = iterated_layer, opacity = 0.8, colors = pal_fun, group = "Predictive layer", project = FALSE) %>% #
    #addLegend(pal = pal_fun, values = values(iterated_layer), title = "Percentile") %>%
    addLegend(pal = pal_fun_rev, values = values(iterated_layer), title = "Percentile", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
    addPolygons(data = iterated_gamelands, color = "black", weight = polygon_weight, smoothFactor = 0.5, opacity = 1, fillOpacity = 0, group = "fine_zoom", popup = ~paste0("<b>Gameland:</b> ", name, "<br/> <b>Landscape suitability index:</b> ", signif(relevant_si, 4), "<br/> <b>Average pixel value:</b> ", signif(relevant_mean, 4), "<br/> <b>High suitability:</b> ", signif(relevant_high, 4), "%<br/> <b>Moderate suitability:</b> ", signif(relevant_med, 4), "%")) %>%
    groupOptions("fine_zoom", zoomLevels = 9:25) %>%
    addPolygons(data = iterated_gamelands, color = "black", weight = polygon_weight, smoothFactor = 0.5, opacity = 0.5, fillOpacity = 0, group = "coarse_zoom", popup = ~paste0("<b>Gameland:</b> ", name, "<br/> <b>Landscape suitability index:</b> ", signif(relevant_si, 4), "<br/> <b>Average pixel value:</b> ", signif(relevant_mean, 4), "<br/> <b>High suitabilty:</b> ", signif(relevant_high, 4), "%<br/> <b>Moderate suitability:</b> ", signif(relevant_med, 4), "%")) %>%
    groupOptions("coarse_zoom", zoomLevels = 8) %>%
    addLayersControl(
      baseGroups = c("Topographic map", "Satellite map"),
      overlayGroups = c("Predictive layer"),
      options = layersControlOptions(collapsed = TRUE)
    )->
    iterated_leaflet
  
  
  
  saveRDS(iterated_leaflet, paste0("leafmap_",iterated_name,".rds"))
  
})

#Rendering gameland leaflet files

walk(.x = names(gamelands)[4:25], .f = function(iterated_name){
  
  #iterated_name <- names(gamelands)[3] #
  
  # Determine the label used in the popup
  if(str_detect(iterated_name, "si")){
    popup_label <- "Landscape suitability index"
  } else {
    popup_label <- "Average pixel value"
  }
  
  #creating the column of relevant high and med results
  dash_loc <- str_locate(iterated_name, "_") #prep code so that I can get the first value of this
  
  gamelands %>%
    st_transform(4326) %>%
    mutate(relevant_weight = as.numeric(pull(gamelands, iterated_name)),
           relevant_high = pull(gamelands, paste0(str_sub(iterated_name, start = 1, end = dash_loc[1]), "high")),
           relevant_med = pull(gamelands, paste0(str_sub(iterated_name, start = 1, end = dash_loc[1]), "med"))) ->
    iterated_gamelands
  
  iterated_gamelands %>%
    pull(relevant_weight) %>%
    stats::quantile(1:100/100, na.rm = TRUE) ->
    gameland_quantiles
  
  iterated_gamelands %>%
    mutate(relevant_interval = findInterval(relevant_weight, gameland_quantiles)) ->
    iterated_gamelands
  
  # pal <- iterated_gamelands %>%
  #   pull(relevant_interval) %>%
  #   unique() %>%
  #   length() %>%
  #   oompaBase::jetColors()
  #sequential_hcl(h = c(270, 90), c = c(40, 25), cmax = 90, l = c(15, 99), power = c(2, 1.5), rev = TRUE)#sea blue
  #sequential_hcl(n = ., h = c(15,50), c = c(100, 30), l = c(35, 90), power = 1.1, rev = TRUE)#redder the better
  #diverge_hcl()
  
  pal_fun <- colorNumeric(palette = "RdYlBu", domain = c(min(unlist(pull(iterated_gamelands, relevant_interval)), na.rm = TRUE), max(unlist(pull(iterated_gamelands, relevant_interval)), na.rm = TRUE)), na.color = NA, reverse = TRUE) 
  pal_fun_rev <- colorNumeric(palette = "RdYlBu", domain = c(min(unlist(pull(iterated_gamelands, relevant_interval)), na.rm = TRUE), max(unlist(pull(iterated_gamelands, relevant_interval)), na.rm = TRUE)), na.color = NA, reverse = FALSE) 
  
  iterated_gamelands %>%
    st_transform(4326) %>%
    leaflet() %>%
    #addTiles(group = "Street map") %>%
    addEsriBasemapLayer(esriBasemapLayers$Topographic, autoLabels = TRUE, group = "Topographic map") %>%
    addEsriBasemapLayer(esriBasemapLayers$Imagery, autoLabels = TRUE, group = "Satellite map") %>%
    addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                opacity = 1.0, fillOpacity = 0.5, fillColor = ~pal_fun(relevant_interval),
                highlightOptions = highlightOptions(color = "black", weight = 2,
                                                    bringToFront = TRUE),
                popup = ~paste0("<b>Gameland:</b> ", name, "<br/> <b>", popup_label,":</b> ", signif(relevant_weight, digits = 4), "<br/> <b>Percentile:</b> ", relevant_interval, "<br/> <b>High suitability:</b> ", round(relevant_high, 0), "%<br/> <b>Moderate suitability:</b> ", round(relevant_med, 0), "%"), 
                group = "State gamelands") %>%
    addLegend(pal = pal_fun_rev, values = unlist(pull(iterated_gamelands, relevant_interval)), title = "Percentile", labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
    addLayersControl(
      baseGroups = c("Topographic map", "Satellite map"),
      overlayGroups = c("State gamelands"),
      options = layersControlOptions(collapsed = TRUE)) -> 
    iterated_leaflet
  
  
  saveRDS(iterated_leaflet, paste0("gamemap_",iterated_name,".rds"))
  
})
