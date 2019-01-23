source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)
library(hunspell)


DataSubClass <- R6Class(
  "DataSubClass",
  public = list(

    # Fields
    name = NULL,
    options = NULL,
    data = NULL,
    totalName = NULL,

    # Constructor
    initialize = function(
      name, 
      totalName,
      options = NULL
    ){
      self$name = name
      self$options = options
      self$totalName = totalName
    },

    addOptions = function(options){
      self$options = options
    },

    # REQUIRES: keywordsToRemove is a vector of options to remove, e.g. c("Total", "AllSex")
    # EFFECTS: amends the data, "abstract"
    cleanData = function(options, keywordsToRemove = NULL){
      cleanedOptions = c()
      i=1
      for(option in options){
        if(option=="" || option %in% keywordsToRemove){
        } else {
          cleanedOptions[i] = option
          i = i + 1
        }
        
      }
      self$options = cleanedOptions
    }


  ))

