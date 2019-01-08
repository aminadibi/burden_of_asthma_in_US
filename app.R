#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

source("./R/Cost.R")
source("./R/Census.R")
source("./R/MapData.R")
source("./R/CreateMap.R")
source("./R/MetaData.R")
source("./R/helper_functions.R")
source("./R/AppLayout.R")
source("./R/DashGraph.R")
source("./R/LeafletMap.R")
source("./R/CountryBaseMap.R")

load(file="./data/metaData.RData")
load(file="./data/cleanedRawData.RData")
load(file="./data/leafletMapList.RData")
print(metaData)

# Left Sidebar

rids <- c("radioGender", "radioAgeGroup", "radioProvinces")
choices_cost <- list("Total" = "sum",
                     "Inpatient" = "hosp",
                     "Outpatient" = "MSP",
                     "Pharma" = "pharm")
s_tabs = c(2,3)
colors <- c("olive", "purple", "maroon", "aqua")
colors <- c("ink", "posy", "embers", "black")
colors <- c("ink", "steel-blue", "cobalt-blue", "posy")
iconBox <- list(icon("user", lib="font-awesome"), icon("usd", lib="font-awesome"))
tab_titles = metaData@tab_titles
i=1
appLayout <- AppLayout$new(6, "burdenOfAsthma")
initialize = TRUE
cat("~~~ Starting UI ~~~", fill=T)


ui <- dashboardPage(skin=appLayout$dashboardColour,

  # header
  dashboardHeader(title=metaData@app_title, titleWidth=320),
  # sidebar
  dashboardSidebar(
    sidebarMenu(id="selectedTab",
      menuItem(metaData@tab_titles[1], tabName = "costTab", icon = icon("dollar sign", lib="font-awesome"),
               menuSubItem("Map", tabName="tab1",icon=icon("globe americas", lib="font-awesome")),
               menuSubItem("Graph", tabName="tab2", icon=icon("bar-chart", lib="font-awesome"))),

      menuItem(metaData@tab_titles[2], tabName = "qalyTab", icon = icon("sort numeric up", lib="font-awesome"),
               menuSubItem("Map", tabName="tab3",icon=icon("globe americas", lib="font-awesome")),
               menuSubItem("Graph", tabName="tab4",icon=icon("bar-chart", lib="font-awesome"))),


      menuItem(metaData@tab_titles[3], tabName = "tab5", icon = icon("address-book", lib="font-awesome")),
      menuItem(metaData@tab_titles[4], tabName = "tab6", icon = icon("balance-scale", lib="font-awesome"))
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

  tabItemsList = metaData@tabItemsList
  cat("~~~ Starting server ~~~", fill=T)
  cost_data <- new("costData")
  cost_data <- readRDS(cost_data, "./data/cost.rds")
  cost <- cost_data@data
  copdNumber <- read_rds("./data/copdNumber.rds")


  dataList <- list("CostDensity"=cost,"Cost"=cost, "copdNumber"=copdNumber)
  filename = "./static_data/USMap.RData"
  init=TRUE
  if(init){
    countryBaseMap <- CountryBaseMap$new("US", filename, init=TRUE)
  } else{
    load(filename)
  }
  group_prev <<- "new"
  group_prev2 <<- "new"


  observe({
    #sout("Selected Tab: ", input$selectedTab)
    selectedTabItem <- tabItemsList[[input$selectedTab]]
    leafletGroups <- selectedTabItem$leafletGroups
    if(!is.null(leafletGroups) && !is.null(input[[leafletGroups]])){
      mapSettings = selectedTabItem$mapSettings
      sout("test")
      group = input$map_groups
      sout("test", group)
      sout(input$map_groups_baselayerchange)
      group = group[-which(group=="basemap")]
      print(length(group))
      if(length(group)==1){
        group_prev <<- group
        print(group_prev)
      }else{
        group = c(setdiff(group_prev, group), setdiff(group, group_prev))
        group_prev <<- group
      }
      g = which(mapSettings$groups!=group)

      proxy = leafletProxy("map")
      proxy %>% hideGroup(mapSettings$groups[g])
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
      
      if(outputType=="plotlyOutput"){
        sout("~~~ Plotly Graph ~~~")
        outputId = tabItemDash$graphOutputId
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
              hidden = selectedTabItem$sidebarHiddenIds[i]
              sout(input[[shown]])
              print(input[[hidden]])
              if(input[[shown]]=="All"){
                
              } else if(input[[shown]]=="Select" && !is.null(input[[hidden]])){
                sout(hidden)
                arguments[[count]] <- list(dataSubClassNames[i], input[[hidden]])
              }
              count = count+1
              
            }
            arguments[[1]] <- rawData
            arguments[[2]] <- dropdownSelected
            print(arguments)
            do.call(dashGraph$drawGraph, args = arguments)
            # dashGraph$drawGraph(rawData, dropdownSelected, list("Year", 19), list("Sex", "Female"),
            #                     list("State", c("Iowa", "Washington")))
          })
          p()
        })
      } else if(outputType=="downloadOutput"){
        sout("~~~ Download ~~~")
        output[[settings$label[k]]] <- downloadHandler(
          filename = function(){
            paste(settings$png_name, Sys.Date(), ".png", sep="")},
          content = function(file) {
            ggsave(file, device="png", width=11, height=8.5)})
      } else if(outputType=="imageOutput"){
        sout(tabItemDash$imageId)
        output[[tabItemDash$imageId]] <- renderImage({
          width  <- session$clientData$output_logos_width
          height <- session$clientData$output_logos_height
          # Return a list containing the filename
          list(src = paste0("./static_data/",settings$imFile),
               contentType = 'image/png',
               width = width,
               alt = "Logos")
        }, deleteFile = FALSE)
      } 
      
      else if(outputType=="leafletOutput"){
        cat("~~~ Leaflet Map ~~~", fill=T)

        mapOutputId = tabItemDash$mapOutputId
        sout(mapOutputId)
        p <- reactive({
          selectedTab <- input$selectedTab
          leafletMap <- leafletMapList[[selectedTab]]
          return(leafletMap)
        })

        output[[mapOutputId]] <- renderLeaflet({

          leafletMap <- p()
          leafletMap$drawMap(rawData)

        #   map$setupMap()
        #   mapRender <- map$drawMap()
        #   mapRender <- mapRender %>% htmlwidgets::onRender("function(el, x) {
        #                                        L.control.zoom({ position: 'topright' }).addTo(this)
        # }")
        #   mapRender

      })
        cat("~~~ Setting up Info Boxes ~~~", fill=T)
        #cat(paste0("Number of Boxes = ", settings$numberOfBoxes), fill=T)
        mapShapeClick <- paste0(mapOutputId, "_shape_click")
        changeLayer <- paste0(mapOutputId, "_groups_baselayerchange")
        value <- reactiveValues(default = "Alberta", layer=1)
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
        sout(valueBoxOutputIds)
        lapply(1:tabItemDash$valueBoxNumber, function(box){
          boxId <- valueBoxOutputIds[box]

          observeEvent(input[[mapShapeClick]],{
            value$default <- input[[mapShapeClick]]$id
          })


        output[[boxId]] <- renderValueBox({
          valueBox(
            1,"test", "blue"
          )
        })
        # output[[boxId]] <- renderValueBox({
        #   if(value$default=="Alberta"){
        #     layer <- 1
        #     groupid <- "group11"
        #   } else {
        #       if(length(value$layer)!=1){
        #         layer <- 1
        #       } else{
        #         layer <- value$layer
        #       }
        #   province <- eventReactive(input[[mapShapeClick]], { # update the location selectInput on map clicks
        #     input[[mapShapeClick]]$id
        #   })
        #     groupid <- province()
        #
        #
        #       #layer <- as.numeric(substr(groupid, 6,6))
        #
        #
        #   }
          # mapDataList <- p()
          # map <- CreateMap$new(layers=mapSettings$layers,
          #            groups = mapSettings$groups, legendLabels=mapSettings$legendLabels,
          #            mapDataList=mapDataList)
          # map$setupMap()
          # cat("Creating map", fill=T)

          # sout(groupid)
          #
          # prov <- as.numeric(substr(groupid,7,9))
          # print(prov)
          # mapLayer <- map$mapDataList[[layer]]
          # print(mapLayer@costYear)
          #
          # print(settings$treatmentType[box])
          # print(settings$treatmentType)
          # typeList <- map$costType(layer, settings$treatmentType[box], TRUE, mapSettings$dense[layer])
          # print(typeList)
          # print(settings$treatmentType[box])
          # if(box==4){
          #   subtitle = paste0(settings$treatmentTypeTitles[box], typeList$provinces[prov],
          #                     settings$boxSuffix[layer])
          # } else{
          #   subtitle = settings$treatmentTypeTitles[box]
          # }
          # if(typeList$labels[prov]==0 || typeList$labels[prov]=="No data"){
          #   value = "No data"
          # } else {
          #   value = paste0(settings$boxPrefix, typeList$labels[prov])
          # }
          #
          # valueBox(
          #   value = value,
          #   subtitle = subtitle,
          #   color = colors[box],
          #   icon = iconBox[[layer]]
          #
          # )
        #})
        })
      } else if(outputType=="infoBox"){

      }
      }
        )}
      })



  n_copd_plot <- function(){
    if (input$radioGender2 == "All") {
      genderCheck <- "all genders"
    } else {
      genderCheck <- input$Gender2
    }

    if (input$radioAgeGroup2 == "All") {
      ageGroupCheck <- "all ages"
    } else {
      ageGroupCheck <- input$AgeGroup2
    }

    if (input$radioProvinces2 == "All") {
      provinceCheck <- "Canada"
    } else {
      provinceCheck <- input$Provinces2
    }
    copdNumber$Legend <- interaction(copdNumber$province, copdNumber$gender, copdNumber$age, sep=" ")
    p <- ggplot(subset (copdNumber, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (province %in% provinceCheck))), aes(x = Year, y=value, color = Legend)) +
      geom_point() + geom_line() + theme_bw() + labs(x="Year", y="") +
      scale_y_continuous("\n", labels = comma)

    ggplotly (p) %>% config(displaylogo=F, doubleClick=F,  displayModeBar=F, modeBarButtonsToRemove=buttonremove) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))

  }

  getMapData <- function(mapSettings){

    genderCheck <- "all genders"
    ageGroupCheck <- "all ages"
    yearCheck <- input$sliderYear
    noType <- TRUE

    mapDataList <- mapSettings$mapDataList
    for(i in 1:mapSettings$layers){
      data <- dataList[[i]]
      mapLayer <- MapLayer$new(
        digits = mapSettings$digits[i],
        group=mapSettings$groups[i],
        prefix = mapSettings$prefix[i],
        plotLabel=mapSettings$plotLabels[i], 
        palette=mapSettings$palette[i],
        countryBaseMap = countryBaseMap
      )
      costAll  <- subset(data, ((gender %in% genderCheck) & (age %in% ageGroupCheck) &(province!="Canada")))
      costYear  <- subset(data, ((gender %in% genderCheck) & (age %in% ageGroupCheck) & (Year %in% yearCheck)))

      if("type" %in% colnames(data)){
        mapData@costAll <- subset(costAll, ((type %in% "sum")))
        mapData@costYear <- subset(costYear, ((type %in% "sum")))
        mapData@costYearNoType <- costYear
        mapData@types <- TRUE
        mapData@typesList <- c("sum", "hosp", "MSP", "pharm")
      } else {
        mapData@costAll <- costAll
        mapData@costYear <- costYear
      }

      mapData <- getCostDensity(mapData, mapSettings$dense[i])

      mapDataList[[i]] <- mapData
    }
    return(mapLayerList)
  }



  }

# Run the application
shinyApp(ui = ui, server = server)

