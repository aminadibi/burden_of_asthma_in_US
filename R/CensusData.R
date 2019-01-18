source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)



CensusData <- R6Class(
  "CensusData",
  public = list(
    
    # Fields
    country = NULL,
    year = NULL,
    data = NULL,
    
    # Constructor
    initialize = function(
      country,
      year
    ){
      self$country = country
      self$year = year
    },
    
    # EFFECTS: get census data from country government website, abstract
    getCensusData = function(){

    }

    
    
  ))

