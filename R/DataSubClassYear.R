source('./R/helper_functions.R')
source('R/DataSubClass.R')
library(R6)



DataSubClassYear <- R6Class(
  "DataSubYear",
  inherit = DataSubClass,
  public = list(

    # Fields
    name = NULL,
    options = NULL,

    # Constructor
    initialize = function(
      name, options = NULL
    ){
      super$initialize(name, options)
    },

    # EFFECTS: amends the data
    # @Override
    cleanData = function(options){
      i = 1
      super$cleanData(options)
      superOptions = self$options
      cleanedOptions = c()
      sout("~~~ Cleaning Data ~~~")
      for(option in superOptions){
        n = as.numeric(option)
        if(is.na(n)){
          
        }else{
          cleanedOptions[i] = option
          i = i + 1
        }
      }
      self$options = cleanedOptions


    }


  ))

