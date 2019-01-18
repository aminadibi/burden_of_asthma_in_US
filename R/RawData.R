source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)



RawData <- R6Class(
  "RawData",
  public = list(

    # Fields
    dataSubClasses = NULL,
    fileName = NULL,
    allData = NULL,
    reName = NULL,
    reNameIndices = NULL,
    annualSums = NULL,
    annualSumsPerCapita = NULL,
    maxOverYears = NULL,
    minOverYears = NULL,
    maxOverYearsPerCapita = NULL,
    minOverYearsPerCapita = NULL,
    censusData = NULL,

    # Constructor
    initialize = function(
      fileName, dataSubClasses,
      reNameIndices = NULL,
      reName = NULL
    ){
      self$fileName = fileName
      self$dataSubClasses = dataSubClasses
      self$reName = reName
      self$reNameIndices = reNameIndices
      self$readCsv()
    },

    # EFFECTS: reads data file
    readCsv = function(){
      csv = read.csv(self$fileName)
      self$allData = csv
      for(dataSubClass in self$dataSubClasses){
        className = dataSubClass$name
        data = csv[[className]]
        dataSubClass$data = data
        sout("Getting data from", className)
        options = levels(data)
        dataSubClass$cleanData(options)
      }
      if(!is.null(self$reName)){
        dataNames = names(self$allData)
        dataNames[self$reNameIndices] = self$reName
        names(self$allData) = dataNames
      }


    },
    
    # REQUIRES: censusData is a type or subtype of CensusData
    # EFFECTS: add census data
    addCensusData = function(
      censusData
    ){
      self$censusData = censusData
    },
    
    # REQUIRES: regionType is a string for the column containing regions, i.e. State, Province, County
    #           valueNames is a list of column names containing the values to be used e.g. indirectCost
    # MODIFIES: this
    # EFFECTS: generate the annual total values for each data sub class by region
    #          and generate the total values per capita
    generateAnnualSums = function(regionType, valueNames, subsetName){
      years = as.numeric(self$dataSubClasses$year$options)
      regions = self$dataSubClasses$state$options
      annualSums = list()
      annualSumsPerCapita = list()

      for(year in years){
        annualValueSums = list()
        annualValueSumsPerCapita = list()
        init = TRUE
        oneYear = self$subsetData(self$allData, list("Year", year))
        i = 1
        for(region in regions){
        regionYear = self$subsetData(oneYear, list(regionType, region))
        total = 0
        totalPerCapita = 0
        for(valueName in valueNames){
          valueSum = sum(as.numeric(regionYear[[valueName]]))
          if(init){
            annualValueSum = data.frame(region=regions, value=rep(0, length(regions)))
            annualValueSums[[valueName]] = annualValueSum
            annualValueSumPerCapita = data.frame(region=regions, value=rep(0, length(regions)))
            annualValueSumsPerCapita[[valueName]] = annualValueSum
          }
          annualValueSums[[valueName]]$value[i] = valueSum
          censusIndex = which(self$censusData$data$region==region)
          if(length(censusIndex)==0){
            stop("One of your data region names does not match the official names. Please correct your data.")
            sout(region)
          }
          censusValue = as.numeric(self$censusData$data$population[censusIndex])
          annualValueSumsPerCapita[[valueName]]$value[i] = valueSum/censusValue
          totalPerCapita = sum(totalPerCapita, valueSum/censusValue)
          total = sum(total, valueSum)
          
        }
        annualValueSumsPerCapita$total$value[i] = totalPerCapita
        annualValueSums$total$value[i] = total
        init = FALSE
        i = i+1
        }
        
        annualSums[[year]] = annualValueSums
        annualSumsPerCapita[[year]] = annualValueSumsPerCapita
        
      }
      self$annualSums[[subsetName]] = annualSums
      self$annualSumsPerCapita[[subsetName]] = annualSumsPerCapita
      self$generateAnnualMaxMin("total", subsetName)
    },
    # REQUIRES: valueNames is a list of column names containing the values to be used e.g. indirectCost
    # MODIFIES: this
    # EFFECTS: generate the max and min over all years for each column e.g. directCost all years
    #          use to make map colors
    generateAnnualMaxMin = function(
      valueNames, subsetName
    ){
      maxOverYears = list()
      minOverYears = list()
      maxOverYearsPerCapita = list()
      minOverYearsPerCapita = list()
      years = as.numeric(self$dataSubClasses$year$options)
      for(valueName in valueNames){
        maxOverYears[[valueName]] = 0
        maxOverYearsPerCapita[[valueName]] = 0
        minOverYears[[valueName]] = min(self$annualSums[[subsetName]][[years[1]]][[valueName]]$value)
        minOverYearsPerCapita[[valueName]] = min(
          self$annualSumsPerCapita[[subsetName]][[years[1]]][[valueName]]$value)
        for(year in years){
          annualSum = self$annualSums[[subsetName]][[year]][[valueName]]$value
          annualSumPerCapita = self$annualSumsPerCapita[[subsetName]][[year]][[valueName]]$value
          annualSumMax = max(annualSum)
          annualSumMin = min(annualSum)
          annualSumMaxPerCapita = max(annualSumPerCapita)
          annualSumMinPerCapita = min(annualSumPerCapita)
          maxOverYears[[valueName]] = max(maxOverYears[[valueName]], annualSumMax)
          minOverYears[[valueName]] = min(minOverYears[[valueName]], annualSumMin)
          maxOverYearsPerCapita[[valueName]] = max(maxOverYearsPerCapita[[valueName]], annualSumMaxPerCapita)
          minOverYearsPerCapita[[valueName]] = min(minOverYearsPerCapita[[valueName]], annualSumMinPerCapita)
        }
      }
      self$maxOverYears[[subsetName]] = maxOverYears
      self$minOverYears[[subsetName]] = minOverYears
      self$maxOverYearsPerCapita[[subsetName]] = maxOverYearsPerCapita
      self$minOverYearsPerCapita[[subsetName]] = minOverYearsPerCapita
    },
    

    # REQUIRES: parameters in the format list("parameter", value)
    # MODIFIES: data
    # EFFECTS: Given a data frame, and a list of parameters to subset,
    #          return a subset of the data frame
    subsetData = function(data,...){
      args = list(...)
      indices = c()
      first = TRUE
      for(arg in args){
        arg = as.list(arg)
        if(length(arg)!=2){
          stop("Arguments must be list of length 2")
        }
        key = arg[[1]]
        value = arg[[2]]
        if(value!="all"){
          indicesArg = which(data[[key]]==value)
          if(!first){
            indices = c(intersect(indices, indicesArg), intersect(indicesArg, indices))
          } else{
            indices = indicesArg
            first = FALSE
          }
        } else {
          
        }
      }
      indices = unique(indices)
      subsettedData = data[indices,]
      return(subsettedData)
    }


  ))

