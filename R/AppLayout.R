source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)
library(RColorBrewer)

AppLayout <- R6Class(
  "AppLayout", 
  public = list(
    
    numberOfTabs = NULL,
    appTitle = NULL,
    dashboard = TRUE,
    dashboardColour = "cg-blue",
    
    initialize = function(numberOfTabs, appTitle, 
                          dashboard=T, dashboardColour="cg-blue"){
      
      
      self$numberOfTabs <- numberOfTabs
      self$appTitle <- appTitle
      self$dashboard <- dashboard
      self$dashboardColour <- dashboardColour
    }
    
))

