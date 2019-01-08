source('./R/helper_functions.R')
source('./R/TabItemDash.R')
#source('../R/initialize.R')
library(R6)
library(raster)
library(rgdal)
library(rgeos)


CountryBaseMap <- R6Class(
  "CountryBaseMap",
  public = list(
    
    # Fields
    
    country = NULL,
    filename = NULL,
    init = FALSE, # do we need to generate the basemap?
    divisions = NULL, # provinces/territories/states
    regions = NULL,
    
    # Constructor
    initialize = function(
      country,
      filename,
      init = FALSE
    ){
      self$filename = filename
      if(!country %in% c("CAN", "US")){
        stop("Country must be in country codes")
      }
      self$country = country
      self$init = init
      if(init){
        self$createBaseMap()
      }
    },
    
    # EFFECTS: Creates the basemap with provinces/states/territories
    createBaseMap = function() {
      countryMap <- getData('GADM', country=self$country, level=1) 
      
      divisions <- unique(countryMap$NAME_1)
      self$divisions <- divisions
      #prov_red <- can1$NAME_1
      newProj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      countryMapPr <- spTransform(countryMap, newProj)
      #prov_red <- provinceConvert(prov_red, "toShort")
      countryMap2 <- countryMapPr[countryMapPr$NAME_1 %in% divisions,]
      countryMapSimplified <- gSimplify(countryMap2, tol=0.1)
      regions <- gBuffer(countryMapSimplified, byid=TRUE, width=0)
      regions <- SpatialPolygonsDataFrame(spTransform(regions,
                                                      CRS("+proj=longlat +ellps=sphere +no_defs")),
                                          data.frame(Region=names(regions),
                                                     row.names=names(regions),
                                                     stringsAsFactors=FALSE))
      self$regions <- regions
      #self$prov_red <- prov_red
      save(self, file=self$filename)
    }
    
  ))

