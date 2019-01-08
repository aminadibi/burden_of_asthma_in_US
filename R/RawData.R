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
        }
      }
      indices = unique(indices)
      subsettedData = data[indices,]
      return(subsettedData)
    }


  ))

