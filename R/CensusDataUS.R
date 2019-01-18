source('./R/helper_functions.R')
source('./R/CensusData.R')
library(R6)
library(jsonlite)

CensusDataUS <- R6Class(
  "CensusDataUS",
  inherit = CensusData,
  public = list(
    
    # Fields
    
    
    
    # Constructor
    initialize = function(
      country, 
      year
    ){
      super$initialize(country, year)
      self$getCensusData()
    },
    
    # @Override
    # EFFECTS: using the API from the US Government, get most recent census data
    #          for each state
    getCensusData = function(){
      api <- paste0("https://api.census.gov/data/", 
                    as.character(self$year),
                    "/pep/population?get=POP,GEONAME&for=state:*")
      api <- paste0("https://api.census.gov/data/2017/pep/population?get=POP,GEONAME&for=state:*")
      census <- fromJSON(api)
      census <- census[-1,]
      data <- list()
      data$population = as.numeric(census[,1])
      data$region = census[,2]
      data$stateCode = census[,3]
      self$data = data
      
      
    }
    
    
    
  ))

