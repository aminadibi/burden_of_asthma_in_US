source('./R/helper_functions.R')
source('./R/TabItemDash.R')
#source('../R/initialize.R')
library(R6)



TabItemDashText <- R6Class(
  "TabItemDashText",
  inherit = TabItemDash,
  public = list(

    # Fields
    markdownFileName = NULL,
    imageId = NULL,
    imFile = NULL,
    # Server Output
    outputTypes = NULL,
    outputIds = c(),

    # Constructor
    initialize = function(
      title,
      inputId,
      tabNumber,
      markdownFileName,
      imageId = NULL,
      imFile = NULL
    ){
      super$initialize(title, inputId, tabNumber)
      self$markdownFileName = markdownFileName
      self$imageId = imageId
      if(!is.null(self$imageId)){
        self$outputTypes = c("imageOutput")
        self$imFile = imFile
      }

    },

    tabItem = function(){
      shinydashboard2::tabItem(
        tabName=self$inputId,
        self$makeTextTab()
       )},
    makeTextTab = function(){
      a = includeMarkdown(paste0("./static_data/", self$markdownFileName))
      if(!is.null(self$imageId)){
        b = imageOutput(self$imageId)
        return(list(a,b))
      } else{
        return(list(a))
      }

    }


  ))

