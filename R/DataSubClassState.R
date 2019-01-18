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
    
    # EFFECTS: amends the data
    # @Override
    cleanData = function(options){
      i = 1
      super$cleanData(options)
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

