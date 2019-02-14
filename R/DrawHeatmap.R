source('./R/helper_functions.R')
source('./R/TabItemDash.R')
source('./R/utils.R')
source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)
library(ggplot2)
library(plotly)
buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
DashHeatmap <- R6Class(
  "DashHeatmap",
  #inherit = TabItemDash,
  public = list(
    
    # Fields -----
    dataSubClassNames = NULL,
    
    initialize = function(dataSubClassNames){
      self$dataSubClassNames = dataSubClassNames
    },
    
    # REQUIRES:
    # MODIFIES:
    # EFFECTS:
    
    drawGraph = function(rawData, valueName, xName, yName, zName, ...){
      
      data <- rawData$cleanedData
      data <- rawData$subsetData(data, ...)
      first = TRUE
      
      data[[valueName]] = as.numeric(as.character(data[[valueName]]))
      data$value = data[[valueName]]
      indices = which(data$Age=="Allage")
      data = data[-indices,]
      
      canvas
      graph <- ggplot(data, aes_string(x = xName, y=yName)) +
        geom_raster(aes_string(fill = zName), na.rm = TRUE) 
        
      pgraph <- ggplotly (graph) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F,
                                            modeBarButtonsToRemove=buttonremove) %>%
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
      return(pgraph)
      
    }
    
    
  ))






