source('./R/helper_functions.R')
source('./R/TabItemDash.R')
source('./R/MapLayer.R')
library(R6)
library(raster)
#library(rgdal)
library(rgeos)


LeafletMap <- R6Class(
  "LeafletMap",
  public = list(

    # Fields
    mapName = NULL,
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
    rawData = NULL,
    layerIds = NULL, # ids for regions to be used in map_shape_click
    numRegions = NULL, # number of regions e.g. number of provinces
    regionNames = NULL, # names of the regions e.g. Alberta
    maxOverYears = NULL,
    minOverYears = NULL,

    # Constructor
    initialize = function(
      mapName,
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
      prefix,
      rawData
    ){
      self$mapName = mapName
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
      self$rawData = rawData
      self$numRegions = length(countryBaseMap$divisions)
      self$regionNames = countryBaseMap$divisions
      self$maxOverYears = rawData$maxOverYears
      self$minOverYears = rawData$minOverYears
      self$createMapLayers()
      self$makeLayerIds()

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

    # REQUIRES: year is an integer in the valid range
    # EFFECTS: renders the map for that year
    drawMap = function(
      year
    ){
      m <- leaflet(options=leafletOptions(zoomControl=FALSE),
                   width="50%") %>% setView(lng = -100, lat = 40, zoom = 4)%>%
        addTiles(group="basemap")
      i = 1
      for(layerName in self$layerChoices){
        if(i==1){
          initLayer = TRUE
        } else {
          initLayer = FALSE
        }
        mapLayer <- self$mapLayerList[[i]]
        mapName = self$mapName
        valueName = "total"
        regions = self$countryBaseMap$regions
        regions$value = self$rawData$annualSums[[mapName]][[layerName]][[year]][[valueName]]$value
        minYear = self$minOverYears[[mapName]][[layerName]]$total
        maxYear = self$maxOverYears[[mapName]][[layerName]]$total
        pal <- leaflet::colorNumeric(
          mapLayer$pal,
          domain = range(minYear, maxYear),
          na.color="grey")

      m <- m %>% addPolygons(data=regions, opacity=0.5, fillOpacity=0.8, group=mapLayer$group,
                             color="white", weight=0.8, fillColor=~pal(value), layerId = self$layerIds[[i]],
                             highlightOptions = highlightOptions(
                               color = "white", opacity = 1, weight = 2, fillOpacity = 1,
                               bringToFront = TRUE, sendToBack = TRUE, dashArray=NULL)) %>%
        addLegend("bottomleft", pal = pal, values=c(minYear, maxYear),
                title = self$groups[i], group=self$groups[i],
                opacity = 1, na.label="No Data", labFormat = myLabFormat(prefix=self$prefix[i],
                                                                         digits=self$digits[i]),
                layerId=self$legendLabels[i], initLayer = initLayer)
      i = i+1
      }
      m <- m %>% addLayersControl(baseGroups = c(self$groups),
                                  options = layersControlOptions(collapsed=FALSE)) %>%
                 htmlwidgets::onRender(
                    "function(el, x) {
                    L.control.zoom({ position: 'topright' }).addTo(this)}") # move zoom control to top right
        m = m %>% htmlwidgets::onRender("
            function(el, x) {
            var myMap = this;
            myMap.on('baselayerchange',
                function (e) {
                    console.log(x);
                    console.log(el);
                    console.log(myMap);
                    myMap.fire('click', {latlng:[36,-102]});
                })
            }")
      return(m)
      },

    # EFFECTS:  gets the data for the given layer, year, and value type
    #           to present in the valueBox
    #' @param layer integer: value of the map layer
    #' @param valueName string: name of the column queried, e.g. directCost
    #' @param year integer: within valid years
    getLayerValueData = function(
      layer, valueName, year
    ){
      layerValueData = self$rawData$annualSums[[self$mapName]][[layer]][[year]][[valueName]]$value
      million = 1000000
      mill = max(layerValueData)/million
      if(mill > 1){
        layerValueData = costToMill(layerValueData)
      } else {
        layerValueData = round(layerValueData, self$digits[layer])
      }
      return(layerValueData)
    },

    makeLayerIds = function(

    ){
      layerIds = list()
      for(layer in 1:self$numLayers){
        layerId = c()
        for(region in 1:self$numRegions){
          id = paste0("layer_", layer, "_region_", region)
          layerId = c(layerId, id)
        }
        layerIds[[layer]] = layerId
      }
      self$layerIds = layerIds

    }
  )
  )

