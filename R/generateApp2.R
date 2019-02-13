source("R/MetaData.R")
source("R/RawData.R")
source("R/DataSubClass.R")
source("R/DataSubClassYear.R")
source("R/DataSubClassState.R")
source("R/TabItemDashMap.R")
source("R/TabItemDashGraph.R")
source("R/TabItemDashText.R")
source("R/settings.R")
source("R/LeafletMap.R")
source("R/CensusDataUS.R")
source("R/CountryBaseMap.R")
dataSubClassNames = c("Year", "State", "Sex", "Age")
totalNames = c("20Year", "US", "Allsex", "Allage")
dataSubClasses = list(
  "year"=DataSubClassYear$new(dataSubClassNames[1], totalNames[1]),
  "state"=DataSubClassState$new(dataSubClassNames[2], totalNames[2]),
  "sex"=DataSubClass$new(dataSubClassNames[3], totalNames[3]),
  "age"=DataSubClass$new(dataSubClassNames[4], totalNames[4])
)

fileName = paste0("static_data/WEBAPP_US.csv")
reNameIndices = c(5,6,7)
reName = c("indirectCost", "directCost", "qalyLost")

rawData = RawData$new(fileName, dataSubClasses, reNameIndices, reName, totalNames)
censusData = CensusDataUS$new("US", 2017)
rawData$addCensusData(censusData)
rawData$fixCellValues("Total", "Allage", "Age")
tabItemsList = list(
  "tab1" = TabItemDashMap$new(title = "Map",
                              inputId = "tab1",
                              mainBoxColor = "info",
                              valueBoxNumber = 3,
                              valueBoxWidths = c(6,6,12),
                              tabNumber = 1,
                              layerChoices = c("Direct Cost"="directCost", "Indirect Cost"="indirectCost"),
                              mapSettings = mapSettings1,
                              numLayers = 2),
  "tab2" = TabItemDashGraph$new(title = "Graph",
                              inputId = "tab2",
                              mainBoxColor = "info",
                              tabNumber = 2,
                              dropdownChoices = c("Direct Cost"="directCost", "Indirect Cost"="indirectCost"),
                              dropdownSelected = c("directCost"),
                              sidebarChoicesNumber = length(dataSubClasses),
                              sidebarShownLabels = dataSubClassNames,
                              dataSubClasses = rawData$dataSubClasses),
  "tab3" = TabItemDashMap$new(title = "Map",
                              inputId = "tab3",
                              mainBoxColor = "info",
                              valueBoxNumber = 2,
                              valueBoxWidths = c(6,6),
                              tabNumber = 3,
                              layerChoices = "qalyLost",
                              mapSettings = mapSettings2,
                              numLayers = 1),
  "tab4" = TabItemDashGraph$new(title = "Graph",
                                inputId = "tab4",
                                mainBoxColor = "info",
                                tabNumber = 4,
                                dropdownChoices = "qalyLost",
                                dropdownSelected = NULL,
                                dropdown = FALSE,
                                sidebarChoicesNumber = length(dataSubClasses),
                                sidebarShownLabels = dataSubClassNames,
                                dataSubClasses = rawData$dataSubClasses),
  "tab5" = TabItemDashText$new(title = "About",
                               inputId = "tab5",
                               tabNumber = 5,
                               markdownFileName = "about.Rmd",
                               imageId = "image1"
),
  "tab6" = TabItemDashText$new(title = "Terms",
                               inputId = "tab6",
                               tabNumber = 6,
                               markdownFileName = "disclaimer.rmd"
)

)
filename = "./static_data/USMap.RData"
init=TRUE
if(init){
  countryBaseMap <- CountryBaseMap$new("US", filename, init=TRUE)
} else{
  load(filename)
}
rawData$generateAnnualSums("State", c("directCost", "indirectCost"), "mapOne", c("overall", "perCapita"))
rawData$generateAnnualSums("State", c("qalyLost"), "mapThree", c("overall"))
leafletMapList <- list(
  "tab1" = LeafletMap$new(mapName = "mapOne",
                          basemapFile = "./static_data/canadaMap.RData",
                          countryBaseMap = countryBaseMap,
                          palette = c("custom"="custom", "custom"="custom"),
                          numLayers = 2,
                          layerChoices = c("Overall Cost"="overall","Cost Per Capita"="perCapita"),
                          groups = c("Overall Cost","Cost per Capita"),
                          plotLabels = c("Cost: $","Cost/Person: $"),
                          digits = c(-5, -1),
                          dense = c(FALSE, TRUE),
                          legendLabels = c("legend1", "legend2"),
                          prefix=c("$", "$"),
                          rawData = rawData),
  "tab3" = LeafletMap$new(mapName = "mapThree",
                          basemapFile = "./static_data/canadaMap.RData",
                          countryBaseMap = countryBaseMap,
                          #palette = c("brewer"="Greens"),
                          palette = c("custom"="custom"),
                          numLayers = 1,
                          layerChoices = c("QALY Lost"="overall"),
                          groups = c("QALY Lost"),
                          plotLabels = c("QALY Lost: "),
                          digits = c(0),
                          dense = c(TRUE),
                          legendLabels = c("legend3"),
                          prefix=c(""),
                          rawData = rawData))



choices_gender <- list("female" = "Female",
                       "male" = "Male",
                       "all" = "all genders")
choices_age <- list("35-54" = "35",
                    "55-64" = "55",
                    "65-74" = "65",
                    "75 and older" = "75",
                    "all" = "all ages")
choices_prov <- list(
  "Alberta" = "AB",
  "British Columbia" = "BC",
  "Manitoba" = "MB",
  "New Brunswick" = "NB",
  "Newfoundland and Labrador" = "NL",
  "Nova Scotia" = "NS",
  "Ontario" = "ON",
  "Prince Edward Island" = "PE",
  "Quebec" = "QC",
  "Saskatchewan" = "SK",
  "all" = "Canada")
choices_cost <- list("Total" = "sum",
                     "Inpatient" = "hosp",
                     "Outpatient" = "MSP",
                     "Pharma" = "pharm")

tab3 <- c("plotlyOutput", "download")
tab2 <- c("selectInput","infoBox","infoBox","infoBox","infoBox", "leafletOutput", "sliderInput")
tab1 <- c("selectInput", "plotlyOutput", "download")
tab4 <- c("infoBox","infoBox","infoBox","infoBox","leafletOutput", "sliderInput")
tab6 <- c("markdown")
tab5 <- c("markdown", "image")
tab2input <- c("costTypeMap", "sliderYear")
tab4input <- c()
tab3input <- c("plot_n_COPD")
tab1input <- c()
tab5input <- c()
tab6input <- c()
tab2id <- list("label" = c("costTypeMap","box1","box2", "box3","box4", "map", "sliderYear"),
               "title" = c("Cost Map", "","","","","", "Year"),
               "treatmentTypeTitles" = c("Inpatient", "Outpatient", "Medication",
                                         "Total Cost for "),
               "treatmentType" = c("hosp", "MSP", "pharm", "sum"),
               "choices" = list(choices_cost),
               "selected" = c("sum"),
               "numberOfBoxes" = 4,
               "boxLabel"="box",
               "boxPrefix"="$",
               "tabNumber"=2,
               "boxSuffix"=c(" per Capita", ""),
               "sliderSettings" = list("min"=2015,
                                       "max"=2030,
                                       "value"=2015,
                                       "step"=NULL,
                                       "round"=FALSE,
                                       "ticks"=TRUE,
                                       "sep"="",
                                       "animate" = animationOptions(interval = 300,
                                                                    loop = FALSE)),
               "functions"=c("getMapData"))
tab3id <- list("label" = c("plot_n_COPD", "download_plot_n"),
               "title" = c("", "Download Plot"),
               "tabNumber"=3,
               "png_name"="COPD_Projected_Prevalence_",
               "functions"=c("n_copd_plot"))
tab1id <- list("label" = c("costType", "plot_cost", "download_plot_cost"),
               "title" = c("Cost Type", "", "Download Plot"),
               "choices" = list(choices_cost),
               "tabNumber"=1,
               "selected" = c("sum"),
               "png_name"="COPD_Projected_cost_",
               "functions"=c("cost_plot"))
tab4id <- list("label" = c("box01","box02", "box03","box04","map2", "sliderYear2"),
               "title" = c("","","","", "Case Map", "", "Year"),
               "choices" = list(choices_cost),
               "numberOfBoxes"=4,
               "tabNumber"=4,
               "treatmentType" = c("hosp", "MSP", "pharm", "sum"),
               "treatmentTypeTitles" = c("Inpatient", "Outpatient", "Medication",
                                         "Total Cases in "),
               "selected" = c("sum"),
               "boxLabel" = "box0",
               "boxPrefix"="",
               "boxSuffix" = c(" per Capita"),
               "sliderSettings" = list("min"=2015,
                                       "max"=2030,
                                       "value"=2015,
                                       "step"=NULL,
                                       "round"=FALSE,
                                       "ticks"=TRUE,
                                       "sep"="",
                                       "animate" = animationOptions(interval = 300,
                                                                    loop = FALSE)),
               "functions"=c("getMapData"))
tab6id <- list("markdownFile"="disclaimer.rmd")
tab5id <- list("markdownFile"="about.Rmd","imFile"="logos2.png", "label"=c("","logos"))

metaData = new("MetaData")
metaData@app_title = "Burden of Asthma in US"
metaData@tabs = 6
metaData@colorScheme = "startup"
metaData@tab_titles <- c("Cost", "QALY", "About", "Terms")
metaData@tab_ids <- c("tab1", "tab2", "tab3", "tab4", "tab5", "tab6")
metaData@sidebar = 3
metaData@sidebar_titles = c("Gender", "Age Group","Province")
metaData@sidebar_labels = c("Gender", "AgeGroup","Provinces")
metaData@sidebar_choices_long = list(choices_gender, choices_age, choices_prov)
metaData@sidebar_choices_short = list(list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"),
                                      list("all" = "All","select" = "Select"))
metaData@sidebar_skip = c(1)
metaData@tab_inout = list(tab1, tab2, tab3, tab4, tab5, tab6)
metaData@tab_settings = list(tab1id, tab2id, tab3id, tab4id, tab5id, tab6id)
metaData@tab_input = list(tab1input, tab2input, tab3input, tab4input, tab5input, tab6input)
metaData@tabItemsList = tabItemsList
metaData@valueBoxIcons = list(icon("user", lib="font-awesome"), icon("usd", lib="font-awesome"))
save(metaData, file="data/metaData.RData")
save(rawData, file="data/cleanedRawData.RData")
save(leafletMapList, file="data/leafletMapList.RData")



