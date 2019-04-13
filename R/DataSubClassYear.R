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
    prettyOptions = NULL,
    prettyOptionsSettings = NULL,

    # Constructor
    initialize = function(
        name, options = NULL, hasPrettyOptions = TRUE, totalName = NULL,
        prettyOptionsSettings = list(action = "add", value = 2000)
    ){
        super$initialize(name = name, totalName = totalName,
                         options = options, hasPrettyOptions = hasPrettyOptions)
        self$prettyOptionsSettings = prettyOptionsSettings

    },

    makePrettyOptions = function() {
        if(self$hasPrettyOptions){
            if(self$prettyOptionsSettings$action == "add") {
                a = self$options + self$prettyOptionsSettings$value
                self$prettyOptions = a
            }
        }
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
        n = as.numeric(option)
        if(is.na(n)){

        }else{
          cleanedOptions[i] = option
          i = i + 1
        }
      }
      self$options = as.numeric(cleanedOptions)
      self$makePrettyOptions()


    },

    # EFFECTS: given the options (levels), correct the spelling
    #          returns options with fixed spelling
    fixSpelling = function(options) {
      return(options)
    }


  ))

