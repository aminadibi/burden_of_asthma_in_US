mapSettings0 <- list(filename = "./static_data/canadaMap.RData",
  palette = c("brewer"="YlGnBu", "brewer"="YlGnBu","brewer"="Greens"),
  layers = 3,
  mapDataList = list("CostDensity"=0,"Cost"=0, "copdNumber"=0),
  groups = c("Cost per Capita","Overall Cost", "Number of COPD"),
  plotLabels = c("Cost/Person: $", "Cost: $","COPD Cases per Year: "),
  digits = c(-1, -5, 2),
  dense = c(TRUE, FALSE, TRUE),
  legendLabels = c("legend1", "legend2", "legend3"),
  prefix=c("$", "$", "")
)
mapSettings1 <- list(filename = "./static_data/canadaMap.RData",
                     palette = c("custom"="custom", "custom"="custom"),
                     layers = 2,
                     mapDataList = list("CostDensity"=0,"Cost"=0),
                     groups = c("Cost per Capita","Overall Cost"),
                     plotLabels = c("Cost/Person: $", "Cost: $"),
                     digits = c(-1, -5),
                     dense = c(TRUE, FALSE),
                     legendLabels = c("legend1", "legend2"),
                     prefix=c("$", "$")
)
mapSettings2 <- list(filename = "./static_data/canadaMap.RData",
                    #palette = c("brewer"="Greens"),
                    palette = c("custom"="custom"),
                    layers = 1,
                    mapDataList = list("copdNumber"=0),
                    groups = c("Number of COPD"),
                    plotLabels = c("COPD Cases per Year: "),
                    digits = c(0),
                    dense = c(TRUE),
                    legendLabels = c("legend3"),
                    prefix=c("")
)