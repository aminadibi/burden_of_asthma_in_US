source('./R/helper_functions.R')
source('R/DataSubClass.R')
library(R6)



DataSubClassState <- R6Class(
  "DataSubClassState",
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
    
    # REQUIRES: keywordsToRemove is a vector of options to remove, e.g. c("Total", "AllSex")
    # EFFECTS: amends the data
    # @Override
    cleanData = function(options, keywordsToRemove = NULL){
      i = 1
      super$cleanData(options, keywordsToRemove)
      superOptions = self$options
      cleanedOptions = c()
      sout("~~~ Cleaning Data ~~~")
      for(option in superOptions){
        
        if(option=="US"){
          
        }
        else {
          if(option=="Colombia"){
            option = "District of Columbia"
          }
          option = spellCheck(option)
          cleanedOptions[i] = option
          i = i + 1
        }
      }
      self$options = cleanedOptions
      
      
    }
    
    
  ))

