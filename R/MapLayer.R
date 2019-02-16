source('./R/helper_functions.R')
source('./R/TabItemDash.R')
library(R6)
library(raster)
#library(rgdal)
library(rgeos)


MapLayer <- R6Class(
  "MapLayer",
  public = list(
    
    # Fields
    
    countryBaseMap = NULL,
    paletteScaleSize = 5,
    group = NULL,
    prefix = NULL, # prefix for legend
    plotLabel = NULL,
    digits = NULL, # number of digits to round
    valueName = NULL, # name of column to be plotted
    pal = NULL, # palette for map
    
    
    # Constructor
    initialize = function(
      digits,
      prefix, 
      group,
      plotLabel, 
      palette,
      valueName,
      countryBaseMap
    ){
      self$countryBaseMap = self$typeCheck(countryBaseMap, "CountryBaseMap")
      self$group <- group
      self$digits <- digits
      self$plotLabel <- plotLabel
      self$prefix <- prefix
      self$valueName <- valueName
      self$setPalette(palette)
    },
    
    # EFFECTS: checks that fields are correct type (R is not typed)
    typeCheck = function(
      object, desiredClass
    ){
      if(class(object)[1]!=desiredClass){
        sout("Must be class", desiredClass)
        stop("Object is not desired class")
        
      } else{
        return(object)
      }
    },
    
    # EFFECTS: sets a color palette for the new map
    setPalette = function(
      palette = "viridis"
    ){
      sout('~~~ LeafletMap: Setting color palette ~~~')
      if(names(palette)=="viridis"){
        pal = viridis_pal(option = palette)(self$paletteScaleSize)
        pal = rev(pal)
      } else if (names(palette)=="brewer"){
        coul = RColorBrewer::brewer.pal(4, palette)
        pal = colorRampPalette(coul)(25)
      } else if(names(palette)=="custom"){
        coul = c("#e0b8d8", 
                 #"#00A7E1",
                 "#240f20")
        #coul = c("#062F4F", "#813772", "#B82601")
        pal = colorRampPalette(coul)(100)
      }
      self$pal = pal
      sout('LeafletMap: color palette set')
    }
    
    
    
    
  ))

