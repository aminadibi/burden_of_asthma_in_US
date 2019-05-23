source("R/RawData.R")
source("R/DataSubClass.R")
source("R/DataSubClassYear.R")
source("R/DataSubClassState.R")
source("R/TabItemDashMap.R")
source("R/TabItemDashGraph.R")
source("R/TabItemDashText.R")
source("R/LeafletMap.R")
source("R/CensusDataUS.R")
source("R/CountryBaseMap.R")
source("R/TransformedData.R")

appData = fromJSON("static_data/app.json")
dataSubClassNames = appData$data$classNames
totalNames = c("20Year", "US", "Allsex", "Allage")
keywordsToRemove = c(totalNames, "total", "Total", "")
dataSubClasses = list(
  "Year"=DataSubClassYear$new(name = dataSubClassNames[1], totalName = totalNames[1], hasPrettyOptions = TRUE),
  "State"=DataSubClassState$new(dataSubClassNames[2], totalNames[2]),
  "Sex"=DataSubClass$new(dataSubClassNames[3], totalNames[3]),
  "Age"=DataSubClass$new(dataSubClassNames[4], totalNames[4])
)

fileName = paste0("static_data/WEBAPP_US_20190514.csv")
populationGrowthFile = paste0("static_data/populationGrowthUS.csv")
reNameIndices = c(5,6,7)
reName = c("directCost", "indirectCost", "qalysLost")

rawData = RawData$new(fileName, dataSubClasses, reNameIndices, reName, keywordsToRemove, totalNames)
censusData = CensusDataUS$new("US", 2017, populationGrowthFile, dataSubClasses$Year$prettyOptions[1])
rawData$addCensusData(censusData)
rawData$fixCellValues("Total", "Allage", "Age")

transformedData = TransformedData$new(rawData$cleanedFinalData)
transformedData$groupDataColumn(groupByColumn = "Year",
                                groupType = "interval",
                                newColumnName = "yearCombined",
                                intervalSize = 5,
                                intervalNames = c("2019 - 2023", "2024 - 2028", "2029 - 2033",
                                                  "2034 - 2038"))
rawData$transformedData = transformedData$data
tabItemsList = list()
for(i in 1:appData$appLayout$subTabs$number){
  tabType = appData$tabs$tabType[[i]]
  if(tabType == "map"){
    layerChoices = appData$tabs$layerChoices[[i]]$valueName
    names(layerChoices) = appData$tabs$layerChoices[[i]]$label
    tabItemsList[[appData$tabs$inputId[i]]] = TabItemDashMap$new(
      title = appData$tabs$title[[i]],
      inputId = appData$tabs$inputId[[i]],
      mainBoxColor = appData$tabs$mainBoxColor[[i]],
      valueBoxNumber = appData$tabs$valueBoxNumber[[i]],
      valueBoxWidths = appData$tabs$valueBoxWidths[[i]],
      tabNumber = i,
      layerChoices = layerChoices,
      numLayers = appData$tabs$leafletMap$numLayers[i])
  } else if(tabType == "graph") {
    tabItemsList[[appData$tabs$inputId[i]]] = TabItemDashGraph$new(
      title = appData$tabs$title[i],
      inputId = appData$tabs$inputId[[i]],
      mainBoxColor = appData$tabs$mainBoxColor[[i]],
      tabNumber = i,
      dropdownChoices = appData$tabs$dropdownChoices[[i]]$valueName,
      dropdownSelected = appData$tabs$dropdownSelected[[i]],
      dropdown = appData$tabs$dropdown[[i]],
      pngDownloadName = appData$tabs$pngDownloadName[[i]],
      sidebarChoicesNumber = appData$tabs$sidebarChoicesNumber[[i]],
      sidebarShownLabels = appData$tabs$sidebarShownLabels[[i]],
      dataSubClasses = rawData$dataSubClasses,
      columnOptions = sapply(appData$tabs$columnOptions, "[[", i),
      columnTypes = appData$tabs$columnTypes[[i]]
    )
  } else if(tabType == "text") {
    tabItemsList[[appData$tabs$inputId[i]]] = TabItemDashText$new(
      title = appData$tabs$title[i],
      inputId = appData$tabs$inputId[i],
      tabNumber = i,
      markdownFileName = appData$tabs$markdownFileName[i],
      imageId = appData$tabs$imageId[i],
      imFile = appData$tabs$imFile[i]
    )
  }
}
country = "US"
filename = paste0("./static_data/", country, "Map.RData")
init=TRUE
if(init){
  countryBaseMap <- CountryBaseMap$new("US", filename, init=TRUE)
} else{
  load(filename)
}
rawData$generateAnnualSums("State", c("directCost", "indirectCost"), "mapOne", c("overall", "perCapita"))
rawData$generateAnnualSums("State", c("qalysLost"), "mapThree", c("overall", "perCapita"))
leafletMapList = list()
for(i in c(1,3)){
  leafletMap = appData$tabs$leafletMap
  palette = leafletMap$palette[[i]]$value
  names(palette) = leafletMap$palette[[i]]$label
  leafletMapList[[appData$tabs$inputId[i]]] = LeafletMap$new(
    mapName = leafletMap$mapName[[i]],
    basemapFile = leafletMap$basemapFile[[i]],
    countryBaseMap = countryBaseMap,
    palette = palette,
    numLayers = leafletMap$numLayers[[i]],
    layerChoices = leafletMap$layerChoices[[i]]$value,
    groups = leafletMap$groups[[i]],
    plotLabels = leafletMap$plotLabels[[i]],
    digits = leafletMap$digits[[i]],
    dense = leafletMap$dense[[i]],
    legendLabels = leafletMap$legendLabels[[i]],
    prefix = leafletMap$prefix[[i]],
    rawData = rawData)
}

save(rawData, file="data/cleanedRawData.RData")
save(leafletMapList, file="data/leafletMapList.RData")
save(appData, file="data/appData.RData")
save(tabItemsList, file="data/tabItemsList.RData")

