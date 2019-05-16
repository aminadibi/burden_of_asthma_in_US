source('./R/helper_functions.R')
source('./R/CensusData.R')
library(R6)
library(jsonlite)

CensusDataUS <- R6Class(
  "CensusDataUS",
  inherit = CensusData,
  public = list(

    # Fields
    populationGrowthFile = NULL,
    projectedPopulation = NULL,

    # Constructor
    initialize = function(
      country,
      year,
      populationGrowthFile = NULL,
      minYear = NULL
    ){
        super$initialize(country, year)
        self$populationGrowthFile = populationGrowthFile
        self$getCensusData()
        if(!is.null(populationGrowthFile)){
             self$calculatePopulationGrowth(minYear)
         }
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

    },

    # REQUIRES: csv file has 2 columns, Year and PercentChange
    #           the initial entry in Year = the census year (self$year)
    #           the final entry in Year = maxYear
    #           minYear >= census year
    #           PercentChange is the percent the population changed in a year
    # EFFECTS:
    calculatePopulationGrowth = function(minYear){
        growthData = read.csv(self$populationGrowthFile)
        years = growthData$Year
        rateChange = growthData$PercentChange/100+1
        censusPopulation = self$data$population
        projectedPopulation = list()
        population = censusPopulation
        for(i in 1:length(years)) {
            population = population * rateChange[i]
            year = years[i] - 2000
            if(year >= minYear - 2000){
                projectedPopulation[[year]] = population
            }
        }
        self$projectedPopulation = projectedPopulation

    }



  ))

