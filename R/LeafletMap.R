source('./R/helper_functions.R')
source('./R/TabItemDash.R')
source('./R/MapLayer.R')
library(R6)
library(raster)
library(rgdal)
library(rgeos)


LeafletMap <- R6Class(
  "LeafletMap",
  public = list(
    
    # Fields
    # Basemap
    basemapFile = NULL, # name for basemap file
    countryBaseMap = NULL,
    
    # Map Layers
    numLayers = NULL, # number of map layers
    layerChoices = NULL, # name of data layers
    mapLayerList = NULL,
    
    # Map Plot
    palette = NULL, # color palette used
    groups = NULL,
    plotLabels = NULL,
    digits = NULL, # rounding of data
    dense = NULL,
    legendLabels = NULL,
    prefix=NULL, # prefix on data
    
    # Constructor
    initialize = function(
      basemapFile,
      countryBaseMap = NULL,
      numLayers,
      layerChoices,
      palette,
      groups,
      digits,
      dense,
      legendLabels,
      plotLabels,
      prefix
    ){
      self$basemapFile = basemapFile
      self$countryBaseMap = self$typeCheck(countryBaseMap, "CountryBaseMap")
      self$numLayers = numLayers
      self$layerChoices = layerChoices
      self$palette = palette
      self$groups = groups
      self$digits = digits
      self$dense = dense
      self$legendLabels = legendLabels
      self$plotLabels = plotLabels
      self$prefix = prefix
      #self$mapLayerList = self$typeCheck(mapLayerList, "MapLayer", isList = TRUE)
    },
    
    # EFFECTS: checks that fields are correct type (R is not typed)
    typeCheck = function(
      object, 
      desiredClass,
      isList = FALSE
    ){
      if(isList){
        classCheck = class(object[[1]])[1]
      } else {
        classCheck = class(object)[1]
      }
      if(classCheck!=desiredClass){
        sout("Must be class", desiredClass)
        stop("Object is not desired class")
        
      } else{
        return(object)
      }
    },
    
    createMapLayers = function(){
      mapLayerList <- list()
      for(i in 1:self$numLayers){
        mapLayer <- MapLayer$new(
          self$digits[i],
          self$prefix[i], 
          self$groups[i],
          self$plotLabels[i], 
          self$palette[i],
          self$layerChoices[i],
          self$countryBaseMap)
        mapLayerList[[i]] <- mapLayer
      }
      self$mapLayerList <- mapLayerList
    },
    
    drawMap = function(
      rawData, ...
    ){
      data <- rawData$allData
      data <- rawData$subsetData(data, ...)
      m <- leaflet(options=leafletOptions(zoomControl=FALSE),
                   width="50%") %>% setView(lng = -100, lat = 60, zoom = 3)%>%
        addTiles(group="basemap")
      for(i in 1:self$numLayers){
        # mapLayer <- mapLayerList[[i]]
        # data[[mapLayer$valueName]] = as.numeric(as.character(data[[mapLayer$valueName]]))
        # data$value = data[[mapLayer$valueName]]

      
      m <- m %>% addPolygons(data=self$countryBaseMap$regions, opacity=0.5, fillOpacity=0.8, group=1,
                             color="white", weight=0.8, fillColor="blue",layerId = 'test',
                             highlightOptions = highlightOptions(
                               color = "white", opacity = 1, weight = 2, fillOpacity = 1,
                               bringToFront = TRUE, sendToBack = TRUE)) 
      }
      return(m)
      }
    

    
    
    
    
  ))

