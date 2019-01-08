source('./R/helper_functions.R')
source('./R/TabItemDash.R')
#source('../R/initialize.R')
library(R6)



TabItemDashMap <- R6Class(
  "TabItemDashMap",
  inherit = TabItemDash,
  public = list(

    # Fields

    mainBoxColor = "info",
    valueBoxNumber = 4,
    valueBoxWidths = c(4,4,4,12),
    valueBoxOutputIds = NULL,
    mapOutputId = NULL,
    sliderId = NULL,
    # Server Output
    outputTypes = c("valueBoxOutput", "leafletOutput"),
    outputIds = c(),
    leafletGroups = NULL,
    # Map Settings
    mapSettings = NULL,
    numLayers = NULL,
    layerChoices = NULL,

    # Constructor
    initialize = function(
      title,
      inputId,
      mainBoxColor,
      valueBoxNumber,
      valueBoxWidths,
      tabNumber,
      mapSettings,
      numLayers,
      layerChoices
    ){
      super$initialize(title, inputId, tabNumber)
      self$mainBoxColor = mainBoxColor
      self$valueBoxNumber = valueBoxNumber
      self$valueBoxWidths = valueBoxWidths
      self$makeId("slider", "sliderId")
      self$makeId("valueBoxOutput", "valueBoxOutputIds", self$valueBoxNumber)
      self$makeId("mapOutput", "mapOutputId")
      self$leafletGroups = paste0(self$mapOutputId, "_groups")
      self$numLayers = numLayers
      self$mapSettings = mapSettings
      self$layerChoices = layerChoices
    },

    tabItem = function(){
      shinydashboard2::tabItem(
        tabName = self$inputId,
        box(solidHeader=FALSE,
          status=self$mainBoxColor,
        # TabBox --> ValueBoxOutput
          lapply(1:self$valueBoxNumber, function(i){
            boxId = self$boxIds[i]
            valueBoxOutput(boxId, self$valueBoxWidths[i])
            self$outputIds = c(self$outputIds, boxId)
          }),

        # AppBrick --> MapBrick
        leafletOutput(outputId=self$mapOutputId, width="100%"),
        # AppBrick --> SliderInput
        sliderInput(inputId=self$sliderId,
                    label="Year",
                    width="100%",
                    min=2015,
                    max=2030,
                    value=2015,
                    step=5,
                    round=FALSE,
                    ticks=TRUE,
                    sep="",
                    animate = animationOptions(interval = 300,
                                               loop = FALSE)),
        width=12, height=6))}



  ))

