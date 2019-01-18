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

    # Constructor
    initialize = function(
      name, options = NULL
    ){
      self$name = name
      self$options = options
    },

    addOptions = function(options){
      self$options = options
    },

    # EFFECTS: amends the data, "abstract"
    cleanData = function(options){
      cleanedOptions = c()
      i=1
      for(option in options){
        if(option==""){
        } else {
          cleanedOptions[i] = option
          i = i + 1
        }
        
      }
      self$options = cleanedOptions
    }


  ))

