source('./R/helper_functions.R')
library(R6)
library(RColorBrewer)

CreateMap <- R6Class(
  "CreateMap", 
  public = list(
    
    mapDataList = NULL,
    groups = NULL,
    layers = NA,
    legendLabels = NULL,
    
    initialize = function(groups, layers, legendLabels,
                        mapDataList){
      
      stopifnot(is.character(legendLabels))
      stopifnot(is.numeric(layers), length(layers) == 1)

      self$mapDataList <- mapDataList
      self$groups <- groups
      self$layers <- layers
      self$legendLabels <- legendLabels
      },

    
    costType = function(layer, treatmentType, types, dense){
              mapLayer <- self$mapDataList[[layer]]
              if(types){
                mapLayer@costYear  <- subset(mapLayer@costYearNoType, ((type %in% treatmentType)))
              }
              newMapLayer <- getCostDensity(mapLayer, dense)
              mapLayer@regions$Pop <- newMapLayer@costDensity
              pop <- getCost(mapLayer@costYear, mapLayer@prov_red)
              prov2 <- provinceConvert(mapLayer@prov_red, to="long")
              mapLayer@regions$provinces <- prov2
              cost_labels <- round(mapLayer@regions$Pop, digits = mapLayer@digits)
              if(max(cost_labels)>1000000){
                cost_labels <- costToMill(cost_labels)
              }
              mapLayer@regions$labels <- cost_labels
              nodata <- which(mapLayer@regions$Pop==0)
              mapLayer@regions$Pop[nodata] = NA
              mapLayer@regions$labels[nodata] = "No Data"
              typeList <- list("provinces"=prov2, "labels"=cost_labels)
              return(typeList)
              
              
    },
    setupMap = function(){
      
      cat("~~~ Setting up Map ~~~", fill=T)
      for(i in 1:self$layers){
        mapLayer <- self$mapDataList[[i]]
        mapLayer@regions$Pop <- mapLayer@costDensity
        pop <- getCost(mapLayer@costYear, mapLayer@prov_red)
        prov2 <- provinceConvert(mapLayer@prov_red, to="long")
        mapLayer@regions$provinces <- prov2
        cost_labels <- round(mapLayer@regions$Pop, digits = mapLayer@digits)
        if(max(cost_labels)>1000000){
          cost_labels <- costToMill(cost_labels)
        }
        mapLayer@regions$labels <- cost_labels
        nodata <- which(mapLayer@regions$Pop==0)
        mapLayer@regions$Pop[nodata] = NA
        mapLayer@regions$labels[nodata] = "No Data"
        layerId = sapply(1:length(prov2), function(x){paste0("group",i,mapLayer@prov_red[x])})
        layerId2 = self$legendLabels[i]
        
        self$mapDataList[[i]] <- mapLayer
        
        
      }
      
      
      invisible(self)
    },
    
    drawMap = function(){
      m <- leaflet(options=leafletOptions(zoomControl=FALSE),
                   width="50%") %>% setView(lng = -100, lat = 60, zoom = 3)%>%
        addTiles(group="basemap")
      for(i in 1:self$layers){
        mapLayer <- self$mapDataList[[i]]
        pal <- leaflet::colorNumeric(
          mapLayer@pal,
          domain = range(mapLayer@min_pop, mapLayer@max_pop),
          na.color="grey")
        mapLayer@regions$Pop <- mapLayer@costDensity
        pop <- getCost(mapLayer@costYear, mapLayer@prov_red)
        prov2 <- provinceConvert(mapLayer@prov_red, to="long")
        mapLayer@regions$provinces <- prov2
        #cost_labels <- round(pop$pop, digits=mapLayer@digits)
        cost_labels <- round(mapLayer@regions$Pop, digits = mapLayer@digits)
        if(max(cost_labels)>1000000){
          cost_labels <- costToMill(cost_labels)
        }
        #cost_labels <- formatC(cost_labels, big.mark=" ", digits=10)
        mapLayer@regions$labels <- cost_labels
        nodata <- which(mapLayer@regions$Pop==0)
        mapLayer@regions$Pop[nodata] = NA
        mapLayer@regions$labels[nodata] = "No Data"
        layerId = sapply(1:length(prov2), function(x){paste0("group",i,x)})
        layerId2 = self$legendLabels[i]
        print(layerId2)
        m <- m %>% addPolygons(data=mapLayer@regions, opacity=0.5, fillOpacity=0.8, group=mapLayer@group,
                               color="white", weight=0.8, fillColor=~pal(Pop),layerId = layerId,
                               highlightOptions = highlightOptions(
                                 color = "white", opacity = 1, weight = 2, fillOpacity = 1,
                                 bringToFront = TRUE, sendToBack = TRUE)) %>%
          addLegend("bottomleft", pal = pal, values=c(mapLayer@min_pop, mapLayer@max_pop),
                    title = self$groups[i], group=self$groups[i],
                    opacity = 1, na.label="No Data", labFormat = myLabFormat(prefix=mapLayer@prefix,
                                                                             digits=mapLayer@digits),
                    layerId=layerId2)
        
        
      }
      m <- m %>% addLayersControl(overlayGroups = c(self$groups),
                                  options = layersControlOptions(collapsed=FALSE)) 
      if(self$layers>1){m <- m %>% hideGroup(self$groups[2:self$layers])}
      
      #                 htmlwidgets::onRender("
      #   function(el, x) {
      #     // Navigate the map to the user's location
      #     this.locate({setView: true});
      #   }
      # ")%>%
      #map.addLayer(console.log(e.layer));
      m <- m %>%
        htmlwidgets::onRender("
function(el,x){
                  this.on('baselayerchange',
                         function (e) {

                           this.locate({setView: true});
                           this.removeControl(legend1);
                            legend2.addTo(this);
                         })}")
      
      return(m)
    }
  
    
    
))

