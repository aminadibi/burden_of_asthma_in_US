source('./R/helper_functions.R')
source('./R/TabItemDash.R')
source('./R/utils.R')
source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)
library(ggplot2)
library(plotly)
buttonremove <- list("sendDataToCloud", "lasso2d", "pan2d" , "zoom2d", "hoverClosestCartesian")
DashGraph <- R6Class(
  "DashGraph",
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

    drawGraph = function(rawData, valueName, ...){

      data <- rawData$allData
      data <- rawData$subsetData(data, ...)
      first = TRUE
      for(dataSubClassName in self$dataSubClassNames){
        if(first){
          Legend <- interaction(data[[dataSubClassName]])
          first = FALSE
        } else {
          Legend <- interaction(Legend, data[[dataSubClassName]], sep=" ")
        }
      }

      data$Legend <- Legend
      data[[valueName]] = as.numeric(as.character(data[[valueName]]))
      data$value = data[[valueName]]
      #sout(data$Legend)
      graph <- ggplot(data, aes(x = Year, y=value/1000000, fill = Legend)) +
    geom_bar(stat = "identity", position = "dodge")  +
    labs(x="Year", y="")
      #+ scale_y_continuous(label=scales::dollar_format(suffix = "M")) + theme_bw()
       #+ scale_y_continuous(label=scales::dollar_format(suffix = "M")) + theme_bw()
      print(class(graph))
      sout("Testing")
      pgraph <- ggplotly (graph) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F,
                              modeBarButtonsToRemove=buttonremove) %>%
        layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
      return(pgraph)

    }


  ))






