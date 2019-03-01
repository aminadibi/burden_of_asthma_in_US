#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(googleChart)
library(shiny)
library(shinythemes)
library(shinydashboard2)
library(devtools)
library(ggplot2)
library(plotly)
library(scales)
library(readr)
library(rmarkdown) #for markdown file
library(knitr) #for markdown file
library(htmltools)
library(maps) # interactive map
library(mapproj)
library(leaflet)
source("./R/Census.R")
source("./R/MapData.R")
source("./R/MetaData.R")
source("./R/helper_functions.R")
source("./R/AppLayout.R")
source("./R/DashGraph.R")
source("./R/LeafletMap.R")
source("./R/CountryBaseMap.R")
source("./R/colorSchemes.R")
source("./R/extendShinyjs2.R")

load(file="./data/metaData.RData")
load(file="./data/cleanedRawData.RData")
load(file="./data/leafletMapList.RData")
print(metaData)

# Left Sidebar

tab_titles = metaData@tab_titles
i=1
appLayout <- AppLayout$new(6, "burdenOfAsthma")
initialize = TRUE
cat("~~~ Starting UI ~~~", fill=T)

includeScript("./ts/startFunctions.js")
ui <- dashboardPage(skin=appLayout$dashboardColour,

  # header
  dashboardHeader(title=metaData@app_title, titleWidth=320),
  # sidebar
  dashboardSidebar(
    sidebarMenu(id="selectedTab",
      menuItem(tab_titles[1], tabName = "costTab", icon = icon("dollar sign", lib="font-awesome"),
               menuSubItem("Map", tabName=metaData@tab_ids[1],icon=icon("globe americas", lib="font-awesome")),
               menuSubItem("Graph", tabName=metaData@tab_ids[2], icon=icon("bar-chart", lib="font-awesome"))),

      menuItem(tab_titles[2], tabName = "qalyTab", icon = icon("sort numeric up", lib="font-awesome"),
               menuSubItem("Map", tabName="tab3",icon=icon("globe americas", lib="font-awesome")),
               menuSubItem("Graph", tabName="tab4",icon=icon("bar-chart", lib="font-awesome"))),


      menuItem(tab_titles[3], tabName = "tab5", icon = icon("address-book", lib="font-awesome")),
      menuItem(tab_titles[4], tabName = "tab6", icon = icon("balance-scale", lib="font-awesome"))
    )
  ),
  # body
  dashboardBody(asList = T,
    shinyjs::useShinyjs(),
    tabItems(asList = T,

            lapply(1:metaData@tabs, function(i){
              metaData@tabItemsList[[i]]$tabItem()
            }))
    ))




server <- function(input, output, session) {

  shinyjs::showLog()
  tabItemsList = metaData@tabItemsList
  cat("~~~ Starting server ~~~", fill=T)
  colorScheme = colorSchemes[[metaData@colorScheme]]
  filename = "./static_data/USMap.RData"
  init=TRUE
  if(init){
    countryBaseMap <- CountryBaseMap$new("US", filename, init=TRUE)
  } else{
    load(filename)
  }

  observe({
    selectedTabItem <- tabItemsList[[input$selectedTab]]
    leafletGroups <- selectedTabItem$leafletGroups
    if(!is.null(leafletGroups) && !is.null(input[[leafletGroups]])){
      mapSettings = selectedTabItem$mapSettings
    }

     if(tabItemsList[[input$selectedTab]]$title=="Graph"){

       selectedTabItem <- tabItemsList[[input$selectedTab]]
      for(i in 1:selectedTabItem$sidebarChoicesNumber){
        sidebarShownInput <- input[[selectedTabItem$sidebarShownIds[i]]]
        sout(selectedTabItem$sidebarHiddenIds[i])
        if(!is.null(sidebarShownInput) && sidebarShownInput=="Select"){
          shinyjs::show (id = selectedTabItem$sidebarHiddenIds[i], anim = TRUE)
        }
        else {
          shinyjs::hide (id = selectedTabItem$sidebarHiddenIds[i], anim = TRUE)
        }

    }
    }})

  lapply(1:metaData@tabs, function(i){
    settings = metaData@tab_settings[[i]]
    outputTypes = tabItemsList[[i]]$outputTypes
    tabItemDash = tabItemsList[[i]]
    sout("Setting up tab components for tab:", i)
    if(is.null(outputTypes)){

    } else{
    lapply(1:length(outputTypes), function(k){

      outputType = outputTypes[k]
      # OutputType 0: Plotly Graph
      if(outputType=="plotlyOutput"){
        sout("~~~ Plotly Graph ~~~")
        outputId = tabItemDash$graphOutputId
        googleChartId = tabItemDash$googleChartOutputId
        output[[googleChartId]] = renderGoogleChart({
          googleChart(googleChartId,
                      data = rawData$subsetData(rawData$cleanedData,
                                                list("State", c("Iowa", "Arkansas")),
                                                list("Sex", c("Female", "Male")),
                                                list("Age",
                                                     c("15 to 19 years",
                                                       "20 to 24 years",
                                                       "25 to 29 years",
                                                       "80 to 84 years")),
                                                       list("Year", 19)),
                      column0 = "State",
                      column1="qalyLost",
                      column2 = "indirectCost",
                      column3 = "Age",
                      headerTitles = c("State", "QALY Lost", "Indirect Cost", "Age", "NoSize"))
        })
        output[[outputId]]<- renderPlotly({
          sout("~~~ Making Graph ~~~")
          dataSubClassNames = c("Year", "State", "Sex", "Age")
          dashGraph <- DashGraph$new(dataSubClassNames)
          p <- reactive({
            selectedTabItem <- tabItemsList[[input$selectedTab]]
            if(selectedTabItem$dropdown){
              dropdownSelected = input[[selectedTabItem$dropdownId]]
            } else {
              dropdownSelected = selectedTabItem$dropdownChoices
            }
            arguments = list()
            count = 3
            print(count)
            for(i in 1:length(dataSubClassNames)){
              shown = selectedTabItem$sidebarShownIds[i]
              hidden = selectedTabItem$sidebarHiddenBoxIds[i]
              if(input[[shown]]=="All" || is.null(input[[hidden]])){
                arguments[[count]] <- list(dataSubClassNames[i], "total")
              } else if(input[[shown]]=="Select"){
                arguments[[count]] <- list(dataSubClassNames[i], input[[hidden]])
              }
              count = count+1

            }
            arguments[[1]] <- rawData
            arguments[[2]] <- dropdownSelected
            print(dropdownSelected)
            print(arguments)
            do.call(dashGraph$drawGraph, args = arguments)
            # dashGraph$drawGraph(rawData, dropdownSelected, list("Year", 19), list("Sex", "Female"),
            #                     list("State", c("Iowa", "Washington")))
          })
          p()
        })
      }
      # OutputType 1: Download
      else if(outputType=="downloadOutput"){
        sout("~~~ Download ~~~")
        outputId = tabItemDash$downloadOutputId
        output[[outputId]] <- downloadHandler(
          filename = function(){
            paste(tabItemDash$pngDownloadName, Sys.Date(), ".png", sep="")},
          content = function(file) {
            ggsave(file, device="png", width=11, height=8.5)})
      }
      # OutputType 2: Image Output
      else if(outputType=="imageOutput"){
        output[[tabItemDash$imageId]] <- renderImage({
          width  <- session$clientData$output_logos_width
          height <- session$clientData$output_logos_height
          # Return a list containing the filename
          list(src = paste0("./static_data/",tabItemDash$imFile),
               contentType = 'image/png',
               width = width,
               alt = "Logos")
        }, deleteFile = FALSE)
      }
      # OutputType 3: Leaflet Output
      else if(outputType=="leafletOutput"){
        cat("~~~ Leaflet Map ~~~", fill=T)

        mapOutputId = tabItemDash$mapOutputId
        p <- reactive({
          selectedTab <- input$selectedTab
          leafletMap <- leafletMapList[[selectedTab]]
          return(leafletMap)
        })
        year <- 19

        output[[mapOutputId]] <- renderLeaflet({

          leafletMap <- p()
          leafletMap$drawMap(year)
      })
        cat("~~~ Setting up Info Boxes ~~~", fill=T)

        mapShapeClick <- paste0(mapOutputId, "_shape_click")
        changeLayer <- paste0(mapOutputId, "_groups_baselayerchange")
        value <- reactiveValues(noClickYet = FALSE, layer=1)
        # observe({
        #   selectedTabItem <- tabItemsList[[input$selectedTab]]
        #
        #   if(!is.null(input[[selectedTabItem$leafletGroups]])){
        #     mapSettings = selectedTabItem$mapSettings
        #     group2 = input[[selectedTabItem$leafletGroups]]
        #     sout(group2)
        #     group2 = group2[-which(group2=="basemap")]
        #     g2 = which(mapSettings$groups==group2)
        #     sout(g2)
        #     value$layer <- g2
        #     sout("Layer: ",value$layer)
        #   }
        #
        #
        # })
        valueBoxOutputIds <- tabItemDash$valueBoxOutputIds
        lapply(1:tabItemDash$valueBoxNumber, function(box){
          boxId <- valueBoxOutputIds[box]
          observeEvent(input[[mapShapeClick]],{
            print(input[[mapShapeClick]])
            value$default <- input[[mapShapeClick]]$id
          })

         output[[boxId]] <- renderValueBox({
           valueName = tabItemDash$valueBoxChoices[box] # column of data to view in box
           region <- eventReactive(input[[mapShapeClick]],
                                   ignoreNULL = FALSE, { # update the location selectInput on map clicks
             input[[mapShapeClick]]$id
           })
           layerRegionId <- region()
           if(is.null(layerRegionId)){
             layerRegionId <- "layer_1_region_1"
           }
        #   } else {
        #       if(length(value$layer)!=1){
        #         layer <- 1
        #       } else{
        #         layer <- value$layer
        #       }

            layerRegionId = strsplit(layerRegionId, "_")
            layerId = as.numeric(layerRegionId[[1]][2])
            regionId = as.numeric(layerRegionId[[1]][4])
          leafletMap <- p()
          value <- leafletMap$getLayerValueData(valueName=valueName, year = 19, layer = layerId)
          regionName = leafletMap$regionNames[regionId]
          subtitles = names(tabItemDash$valueBoxChoices)
          if(box==tabItemDash$valueBoxNumber){
            subtitle = subtitles[box]
            subtitle = paste0(subtitle, regionName)
           } else{
             subtitle = subtitles[box]
           }
          # if(typeList$labels[prov]==0 || typeList$labels[prov]=="No data"){
          #   value = "No data"
          # } else {
          #   value = paste0(settings$boxPrefix, typeList$labels[prov])
          # }

           valueBox(
             value=value[regionId],
             subtitle = subtitle,
             color = colorScheme[box],
             icon = metaData@valueBoxIcons[[layerId]]
           )
        })
        })
      }
      }
        )}
      })



  }

# Run the application
shinyApp(ui = ui, server = server)

