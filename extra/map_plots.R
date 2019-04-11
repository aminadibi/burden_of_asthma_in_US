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
source("R/helper_functions.R")

provinceConvert <- function(provinces, to){
  short <- c("AB", "BC", "SK", "MB", "ON", "QC", "NL", "NT", "NU", "PE", "YT", "NS", "NB", "QC")
  long <- c("Alberta", "British Columbia", "Saskatchewan", "Manitoba",
            "Ontario", "Québec", "Newfoundland and Labrador", "Northwest Territories",
            "Nunavut", "Prince Edward Island", "Yukon", "Nova Scotia", 
            "New Brunswick", "Quebec")

  convert <- c()
  if(to=="toShort"){
    for(p in provinces){
      convert <- c(convert, short[which(long==p)])
    }
  } else {
    for(p in provinces){
      l <- long[which(short==p)]
      l <- l[1]
      convert <- c(convert, l)
    }
  }
  return(convert)
}

getCost <- function(data, provinces){
  pop <- c()
  prov_new <- c()
  for(i in 1:length(provinces)){
    prov <- provinces[i]
    cost_prov <- subset(data, ((province %in% prov)))
    cost_prov <- cost_prov$value
    if(length(cost_prov)==0){
      cost_prov <- 0
    }
    pop <- c(pop, cost_prov)
    prov_new <- c(prov_new, rep(prov, length(cost_prov)))
  }
  pop <- as.data.frame(pop)
  pop$provinces <- prov_new
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

drawMap <- function(data, dollarRange, prov_red, can_simp){
  
  pop_data <- getCost(data, prov_red)
  pop <- pop_data$pop
  col_range <- 50
  scale_size <- 10
  colfunc <- viridis_pal(option="C")
  col_scale <- colfunc(col_range+1)
  min_pop <- dollarRange[1]
  max_pop <- dollarRange[2]
  scale = (max_pop - min_pop)/col_range
  colors <- c()
  nodata <- c()
  for(i in 1:length(pop)){
    p <- pop[i]
    if(p-min_pop<0){
      nodata <- c(nodata, i)
    }
    col <- max(p - min_pop,0)/scale
    colors <- c(colors,col)
  }
  colors <- col_scale[round(colors)+1]
  colors[nodata] <- "grey"
  labels <- c()
  for(i in 0:10){
    lab <- round(min_pop + i*scale*col_range/scale_size)
    labels <- c(labels, lab)
  }
  
  can_simp.points = fortify(can_simp, region="ID")
  a=unique(can_simp.points$id)
  a=sort(a)
  a=as.numeric(a)
  colors <- colors[a]
  figures <- costToMill(pop)
  legend_labels <- c(" No Data", costToMill(labels))
  scale_data = data.frame(x1=rep(can_simp@bbox[1,1],scale_size+2), y1=rep(can_simp@bbox[2,1],scale_size+2), 
                          labels=legend_labels)
  
  col_scale2 <- c("grey",colfunc(scale_size+1))
  gg <- ggplot(can_simp.points, aes(x=long, y=lat, group=group))+
    geom_polygon(aes(fill=id)) +
    geom_path(color="white") +
    geom_point(data=scale_data, aes(x1, y1, colour=labels), inherit.aes=FALSE) +
    coord_equal() +
    labs(x="", y="") +
    guides(color=guide_legend(reverse=TRUE), fill=FALSE)+
    scale_fill_manual(values=colors, labels=figures[a], name="Provinces")+
    scale_colour_manual(values=col_scale2, name="Annual Cost")
  gg

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

# addPolygons(data=toronto, opacity=0.5, fillOpacity=0.5, group="province") %>%
# toronto <- get_census(dataset='CA16', regions=list(CMA="35535"),
#                       vectors="v_CA16_2397", level='CSD', quiet = TRUE, 
#                       geo_format = 'sp', labels = 'short')
# mypalette <- function(id){
#   t=as.numeric(id)
#   return(colors[t])
# }






