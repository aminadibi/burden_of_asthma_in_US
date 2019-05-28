source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)



RawData <- R6Class(
  "RawData",
  public = list(

    # Fields
    dataSubClasses = NULL, # DataSubClass
    fileName = NULL, # string : name of file containing data
    allData = NULL,
    cleanedData = NULL,
    cleanedFinalData = NULL, # data.frame : used in annual sums
    transformedData = NULL,
    reName = NULL, # [string] : list of new column names
    reNameIndices = NULL, # [integer] : indices of column names to change
    annualSums = NULL,
    annualSumsPerCapita = NULL,
    maxOverYears = NULL,
    minOverYears = NULL,
    maxOverYearsPerCapita = NULL,
    minOverYearsPerCapita = NULL,
    censusData = NULL,
    keywordsToRemove = NULL,
    totalNames = NULL,

    # Constructor
    initialize = function(
      fileName, dataSubClasses,
      reNameIndices = NULL,
      reName = NULL,
      keywordsToRemove = NULL,
      totalNames = NULL
    ){
      self$fileName = fileName
      self$dataSubClasses = dataSubClasses
      self$reName = reName
      self$reNameIndices = reNameIndices
      self$keywordsToRemove = keywordsToRemove
      self$totalNames = totalNames
      self$readCsv()
    },

    # EFFECTS: reads data file,
    #          allData: fixes any spelling errors, fixes options for each column
    #          cleanedFinalData: removes specified rows (keywordsToRemove)
    readCsv = function(){
      csv = read.csv(self$fileName)
      self$allData = csv
      for(dataSubClass in self$dataSubClasses){
        className = dataSubClass$name
        data = csv[[className]]
        dataSubClass$data = data
        sout("Getting data from", className)
        options = levels(data)
        fixedOptions = dataSubClass$fixSpelling(options)
        levels(data) = fixedOptions
        dataSubClass$cleanData(fixedOptions, self$totalNames)
        self$allData[[className]] = data

      }
      if(!is.null(self$reName)){
        dataNames = names(self$allData)
        dataNames[self$reNameIndices] = self$reName
        names(self$allData) = dataNames
      }
      self$removeRows(self$keywordsToRemove, names(self$dataSubClasses))
    },

    # MODIFIES: this
    # EFFECTS: given a cell value to find and cell value to insert,
    #          edit any cells matching in the column provided
    #' @param valueToFind cell value to find
    #' @param valueToInsert cell value to insert
    #' @param columnName string: column to search
    fixCellValues = function(valueToFind, valueToInsert, columnName){
      data = self$allData
      dataColumn = data[[columnName]]
      indices = which(dataColumn==valueToFind)
      data[[columnName]][indices] = valueToInsert
      self$cleanedData = data

    },

    # EFFECTS: add census data
    #' @param censusData type or subtype of CensusData
    addCensusData = function(
      censusData
    ){
      self$censusData = censusData
    },

    # EFFECTS: removes row with cell of a given keyword
    #' @param keywordsToRemove vector of options to remove, e.g. c("Total", "AllSex)
    #' @param columnNames c(string): vector of column names to search
    removeRows = function(keywordsToRemove, columnNames){
        data = self$allData
        for(keyword in keywordsToRemove){
            for(columnName in columnNames){
                dataColumn = data[[columnName]]
                indices = which(dataColumn==keyword)
                if(length(indices)!=0){
                    data = data[-indices,]
                }
            }
        }
        self$cleanedFinalData = data
    },

    # MODIFIES: this
    # EFFECTS: generate the annual total values for each data sub class by region
    #          and generate the total values per capita
    #' @param regionType string: name of column containing regions, i.e. State, Province, County
    #' @param valueNames c(string): vector of column names containing the values to be used e.g. indirectCost
    #' @param layerNames c(string): vector of layers on the map, e.g. overall and perCapita
    generateAnnualSums = function(regionType, valueNames, subsetName, layerNames){
        layerFunctions = c("identityFunction", "perCapitaFunction")
        years = as.numeric(self$dataSubClasses$Year$options)
        regions = self$dataSubClasses$State$options
        annualSums = list()
        annualSumsPerCapita = list()
        layer = 1
        for(layerName in layerNames){
            layerFunction = layerFunctions[layer]
            for(year in years){
                sumOneYearAllValueTypes = list()
                totalFrame = data.frame(region=regions, value=rep(0, length(regions)))
                sumOneYearAllValueTypes$total = totalFrame
                init = TRUE
                oneYear = self$subsetData(self$cleanedFinalData, list("Year", year))
                i = 1
                for(region in regions){
                    regionYear = self$subsetData(oneYear, list(regionType, region))
                    total = 0

                    for(valueName in valueNames){
                        valueSum = sum(as.numeric(regionYear[[valueName]]))
                        if(init){
                            sumOneYearOneValueType = data.frame(region=regions, value=rep(0, length(regions)))
                            sumOneYearAllValueTypes[[valueName]] = sumOneYearOneValueType
                            sumOneYearTotal = sumOneYearOneValueType
                        }

                        valueSumTransform = self[[layerFunction]](region, valueSum, year)
                        sumOneYearAllValueTypes[[valueName]]$value[i] = valueSumTransform
                        total = sum(total, valueSumTransform)

                    }
                    sumOneYearAllValueTypes$total$value[i] = total
                    init = FALSE
                    i = i+1
                }
                annualSums[[year]] = sumOneYearAllValueTypes
            }
            self$annualSums[[subsetName]][[layerName]] = annualSums
            layer = layer + 1
        }
        self$generateAnnualMaxMin(c("total"), subsetName, layerNames)
    },

    # EFFECTS:  returns valueSum unchanged
    #' @param region string: name of a Province/State/County
    #' @param valueSum any: value for that region
    #' @param year integer: year in form -2000 e.g. 2019 = 19, etc
    identityFunction = function(
      region, valueSum, year
    ){
      return(valueSum)
    },

    # MODIFIES: valueSum
    # EFFECTS:  finds the population for that region and
    #           divides valueSum by it
    #' @param region string: name of a Province/State/County
    #' @param valueSum any: value for that region
    #' @param year integer: year in form -2000 e.g. 2019 = 19, etc
    perCapitaFunction = function(
      region, valueSum, year
    ){
      censusIndex = which(self$censusData$data$region==region)
      if(length(censusIndex)==0){
        stop("One of your data region names does not match the official names. Please correct your data.")
        sout(region)
      }
      censusValue = as.numeric(self$censusData$projectedPopulation[[year]][censusIndex])
      return(valueSum/censusValue)
    },

    # REQUIRES: valueNames is a list of column names containing the values to be used e.g. indirectCost
    #           subsetName is the name of the map, e.g. mapOne
    #           layerNames is the layers on the map, e.g. overall and perCapita
    # MODIFIES: this
    # EFFECTS: generate the max and min over all years for each column e.g. directCost all years
    #          use to make map colors
    generateAnnualMaxMin = function(
      valueNames, subsetName, layerNames
    ){
      for(layerName in layerNames){
      maxOverYears = list()
      minOverYears = list()
      years = as.numeric(self$dataSubClasses$Year$options)
      for(valueName in valueNames){
        maxOverYears[[valueName]] = 0
        minOverYears[[valueName]] = min(
          self$annualSums[[subsetName]][[layerName]][[years[1]]][[valueName]]$value)
        for(year in years){
          annualSum = self$annualSums[[subsetName]][[layerName]][[year]][[valueName]]$value
          annualSumMax = max(annualSum)
          annualSumMin = min(annualSum)
          maxOverYears[[valueName]] = max(maxOverYears[[valueName]], annualSumMax)
          minOverYears[[valueName]] = min(minOverYears[[valueName]], annualSumMin)
        }
      }
      self$maxOverYears[[subsetName]][[layerName]] = maxOverYears
      self$minOverYears[[subsetName]][[layerName]] = minOverYears
      }
    },

    # REQUIRES: parameters in the format list("parameter", value)
    # EFFECTS: Given a data frame, and a list of parameters to subset,
    #          return a subset of the data frame
    subsetData = function(data, ...){
      args = list(...)
      print(args)
      indices = c()
      first = TRUE

      for(arg in args){
        arg = as.list(arg)
        if(length(arg)!=2){
          stop("Arguments must be list of length 2")
        }
        key = arg[[1]]
        value = arg[[2]]
        print(value)
        if(length(value)==1 && value=="total") {
          value = self$dataSubClasses[[key]]$totalName
          print(value)
        }
        indicesArg = which(data[[key]] %in% value)
        if(!first){
          indices = c(intersect(indices, indicesArg), intersect(indicesArg, indices))
        } else{
          indices = indicesArg
          first = FALSE
        }
      }
      indices = unique(indices)
      subsettedData = data[indices,]
      return(subsettedData)
    }


  ))

