source('./R/helper_functions.R')
#source('../R/initialize.R')
library(RColorBrewer)
library(R6)
mapData <- R6Class(
  "mapData",
  public = list(

  # Fields
    col_range = 50,
    scale_size = 5,
    layers = 1,
    costYear = "data.frame",
    costAll = "data.frame",
    costYearNoType = "data.frame",
    types = "logical",
    typesList = "character",
    min_pop = "numeric",
    max_pop = "numeric",
    regions = "SpatialPolygonsDataFrame",
    prov_red = "character",
    costDensity = "numeric",
    provinces = "character",
    pal="character",
    palette="character",
    group="character",
    plotLabel = "character",
    digits = "numeric",
    prefix = "character",
    legendLabels = "character",
    
    initialize = function(
      canMap,
      digits, 
      prefix, 
      group, 
      plotLabel,
      palette
    ){
      self$regions <- canMap@regions
      self$prov_red <- canMap@prov_red
      self$provinces <- canMap@provinces
      self$group <- group
      self$digits <- digits
      self$plotLabel <- plotLabel
      self$prefix <- prefix
      self$setPalette(palette)
    },
    
    dataReorder = function(
      newData,
      to = "toShort"
    ){
      provinces = self$prov_red
      newProvinces = newData@provinces
      newProvinces = provinceConvert(newProvinces, to=to)
      newOrder = rep(0, length(provinces))
      for (i in 1:length(provinces)){
        newOrder[i] <- which(newProvinces==provinces[i])
      }
      
      return(newOrder)
    },
    
    getCostDensity = function(
      dense
    ){
      file <- "./census_data/T10120180918023605.CSV"
      census <- new("censusData")
      census <- readFile(census, file)
      census <- setProvinces(census)
      census <- getPopulation(census)
      newOrder <- dataReorder(object, census, to="toShort")
      population <- census@population[newOrder]
      costAll <- self$costAll
      popAll <- getCost(object@costAll, object@prov_red)
      cd <- c()
      
      if(dense==TRUE){
        
        for(i in 1:nrow(popAll)){
          prov <- popAll$provinces[i]
          prov <- provinceConvert(prov, to="long", quebec=2)
          cost <- popAll$pop[i]/census@population[prov]
          if(is.na(cost)){cost <- 0}
          cd <- c(cd, cost)
        }}
      
      else{
        for(i in 1:nrow(popAll)){
          prov <- popAll$provinces[i]
          prov <- provinceConvert(prov, to="long", quebec=2)
          cost <- popAll$pop[i]
          if(is.na(cost)){cost <- 0}
          cd <- c(cd, cost)
        }
        
      }
      popAll$cd <- cd
      sub <- which(popAll$cd==0)
      sub <- popAll[-sub,]
      min_id <- which(sub$cd==min(sub$cd))
      max_id <- which(popAll$cd==max(popAll$cd))
      min_pop <- sub$cd[min_id]
      min_prov <- provinceConvert(sub$provinces[min_id], to="long")
      max_pop <- popAll$cd[max_id]
      max_prov <- provinceConvert(popAll$provinces[max_id], to="long")
      self$min_pop <- min_pop
      self$max_pop <- max_pop
      popYear <- getCost(object@costYear, object@prov_red)
      popYear <- popYear$pop
      if(dense==TRUE){
        self$costDensity <- popYear/population
      } else {
        self$costDensity <- popYear
      }
      return(object)
    }
    

    
  )

)

