library(maps)
library(mapproj)
library(mapdata)
library(rgeos)
library(maptools)
library(sp)
library(raster)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(viridis)
library(plotly)
library(leaflet)
#library(cancensus)
#source("../R/helper_functions.R")

provinceConvert <- function(provinces, to, quebec=1){
  short <- c("AB", "BC", "SK", "MB", "ON", "QC", "NL", "NT", "NU", "PE", "YT", "NS", "NB")
  long <- c("Alberta", "British Columbia", "Saskatchewan", "Manitoba",
            "Ontario", "Québec", "Newfoundland and Labrador", "Northwest Territories",
            "Nunavut", "Prince Edward Island", "Yukon", "Nova Scotia",
            "New Brunswick")

  convert <- c()
  if(to=="toShort"){
    for(p in provinces){
      convert <- c(convert, short[which(long==p)])
    }
  } else {
    for(p in provinces){
      l <- long[which(short==p)]
      if(p=="QC"){
        l <- l[quebec]
      }
      convert <- c(convert, l)
    }
  }
  return(convert)
}

getCost <- function(data, provinces){
  pop <- c()
  for(i in 1:length(provinces)){
    prov <- provinces[i]
    cost_prov <- subset(data, ((province %in% prov)))
    cost_prov <- cost_prov$value
    if(length(cost_prov)==0){
      cost_prov <- 0
    }
    pop <- c(pop, cost_prov)
  }
  pop <- as.data.frame(pop)
  pop$provinces <- provinces
  return(pop)
}

getMap <- function(){
  can1<-getData('GADM', country="CAN", level=1)
  provinces <- unique(can1$NAME_1)
  prov_red <- can1$NAME_1
  mapExtent <- rbind(c(-156, 80), c(-68, 19))
  #newProj <- CRS("+proj=poly +lat_0=0 +lon_0=-100 +x_0=0
  #+y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  newProj <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  mapExtentPr <- spTransform(SpatialPoints(mapExtent,
                                           proj4string=CRS("+proj=longlat")),
                             newProj)

  can1Pr <- spTransform(can1, newProj)
  prov_red <- provinceConvert(prov_red, "toShort")
  can2 <- can1Pr[can1Pr$NAME_1 %in% provinces,]

  #can_simp <- gSimplify(can2, tol=10000)
  can_simp <- gSimplify(can2, tol=0.1)
  regions <- gBuffer(can_simp, byid=TRUE, width=0)
  regions <- SpatialPolygonsDataFrame(spTransform(regions,
                                                  CRS("+proj=longlat +ellps=sphere +no_defs")),
                                      data.frame(Region=names(regions),
                                                 row.names=names(regions),
                                                 stringsAsFactors=FALSE))
  mapFrame <- list(prov_red, can_simp)
  return(mapFrame)
}


drawMap2 <- function(data, dollarRange, prov_red, can_simp){

  pop_data <- getCost(data, prov_red)
  pop <- pop_data$pop
  col_range <- 50
  scale_size <- 20

  pal <- leaflet::colorNumeric(viridis_pal(option = "D")(scale_size), domain = range(min_pop, max_pop),
                               na.color="grey")

  regions$Pop <- pop
  prov2 <- provinceConvert(prov_red, to="long")
  regions$provinces <- prov2
  cost_labels <- round(regions$Pop, digits=-5)
  cost_labels <- formatC(cost_labels, big.mark=" ", digits=10)
  regions$labels <- cost_labels
  m <- leaflet() %>% setView(lng = -120, lat = 60, zoom = 4)  %>%
    addTiles(group="basemap") %>%
    addPolygons(data=regions, opacity=0.5, fillOpacity=0.8, group="cansimp",
                color="white", weight=0.8, fillColor=~pal(Pop),
                highlightOptions = highlightOptions(
                  color = "white", opacity = 1, weight = 2, fillOpacity = 1,
                  bringToFront = TRUE, sendToBack = TRUE),
                popup = paste(regions$provinces, "<br>",
                              "Cost: $", regions$labels, "<br>")) %>%
    addLayersControl(overlayGroups = c("basemap", "province", "cansimp")) %>%
    addLegend("bottomleft", pal = pal, values=regions$Pop,
              title = "Cost", group="cansimp",
              opacity = 1)
  return(m)

}








