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
    maxOverYears = NULL,
    minOverYears = NULL,

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
    
    # REQUIRES: regionName is a string for the column containing regions, i.e. State, Province, County
    #           valueNames is a list of column names containing the values to be used e.g. indirectCost
    # MODIFIES: this
    # EFFECTS: generate the annual total values for each data sub class by region
    generateAnnualSums = function(regionName, valueNames){
      years = as.numeric(self$dataSubClasses$year$options)
      regions = self$dataSubClasses$state$options
      annualSums = list()

      for(year in years){
        annualValueSums = list()
        init = TRUE
        oneYear = self$subsetData(self$allData, list("Year", year))
        i = 1
        for(region in regions){
        regionYear = self$subsetData(oneYear, list(regionName, region))
        for(valueName in valueNames){
          valueSum = sum(as.numeric(regionYear[[valueName]]))
          if(init){
            annualValueSum = data.frame(region=regions, value=rep(0, length(regions)))
            annualValueSums[[valueName]] = annualValueSum
          }
          annualValueSums[[valueName]]$value[i] = valueSum
          
        }
        init = FALSE
        i = i+1
        }
        
        annualSums[[year]] = annualValueSums
        
      }
      self$annualSums = annualSums
      self$generateAnnualMaxMin(valueNames)
    },
    # REQUIRES: valueNames is a list of column names containing the values to be used e.g. indirectCost
    # MODIFIES: this
    # EFFECTS: generate the max and min over all years for each column e.g. directCost all years
    #          use to make map colors
    generateAnnualMaxMin = function(
      valueNames
    ){
      maxOverYears = list()
      minOverYears = list()
      years = as.numeric(self$dataSubClasses$year$options)
      for(valueName in valueNames){
        maxOverYears[[valueName]] = 0
        minOverYears[[valueName]] = min(self$annualSums[[years[1]]][[valueName]]$value)
        for(year in years){
          annualSum = self$annualSums[[year]][[valueName]]$value
          annualSumMax = max(annualSum)
          annualSumMin = min(annualSum)
          maxOverYears[[valueName]] = max(maxOverYears[[valueName]], annualSumMax)
          minOverYears[[valueName]] = min(minOverYears[[valueName]], annualSumMin)
        }
      }
      self$maxOverYears = maxOverYears
      self$minOverYears = minOverYears
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

