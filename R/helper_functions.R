
costToMill <- function(cost_vector, char = TRUE){
  figures <- c()
  for(i in 1:length(cost_vector)){
    figure <- cost_vector[i]/1000000
    if(figure>=1000){
      figure <- cost_vector[i]/1000000000
      figure <- round(figure,2)
      figure <- formatC(figure, format = 'f', digits = 1)
      if(char){
        figure <- paste0(as.character(figure), "B")
      }
    } else if (cost_vector[i]==0) {
      figure <- "No Data"
    } else{
      figure <- round(figure, 2)
      figure <- formatC(figure, format = 'f', digits = 0)
      if(char){
        figure <- paste0(as.character(figure), "M")
      }
    }
    figures <- c(figures, figure)
  }
  return(figures)
}


provinceConvert <- function(provinces, to, quebec=1){
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
      if(p=="QC"){l <- l[quebec]}
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

customLegend <- function(x){
  return(0)
}

myLabFormat <- function(
  prefix = "", suffix = "", between = " &ndash; ", digits = 3, big.mark = ",",
  transform = identity
) {
  
  formatNum <- function(x) {
    x <- round(transform(x), digits)
   
    if(max(x)>1000000){
      x <- costToMill(x, char=TRUE)
    }

    return(x)
  }
  
  function(type, ...) {
    switch(
      type,
      numeric = (function(cuts) {
        paste0(prefix, formatNum(cuts), suffix)
      })(...), # nolint
      bin = (function(cuts) {
        n <- length(cuts)
        paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), suffix)
      })(...), # nolint
      quantile = (function(cuts, p) {
        n <- length(cuts)
        p <- paste0(round(p * 100), "%")
        cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
        # mouse over the legend labels to see the values (quantiles)
        paste0(
          "<span title=\"", cuts, "\">", prefix, p[-n], between, p[-1], suffix,
          "</span>"
        )
      })(...), # nolint
      factor = (function(cuts) {
        paste0(prefix, as.character(transform(cuts)), suffix)
      })(...) # nolint
    )
  }}

getCoordinates <- function(start, number, rows, columns, dist=2){
  
  coordinates <- matrix(data=0,number, 2)
  lat <- start[1]
  long <- start[2]
  rowCount <- 1
  for(i in 1:number){
    coordinates[i,1] <- lat
    coordinates[i,2] <- long
    if(rowCount==columns){
      lat <- lat + dist
      long <- start[2]
      rowCount <- 1
    } else{
      long <- long+dist
      rowCount <- rowCount+1
    }
    
    
    
  }
  return(coordinates)
}

startCoord <-list("BC"=c(56,-128),
                  "AB"=c(56, -118),
                  "SK"=c(56, -108),
                  "MB"=c(56, -100),
                  "ON"=c(51, -91),
                  "QC"=c(56,-73),
                  "NL"=c(54,-54),
                  "NS"=c(45,-63),
                  "PE"=c(46,-63),
                  "NB"=c(47,-67),
                  "YT"=c(64,-139),
                  "NU"=c(64,-126),
                  "NT"=c(64,-100))

mapIcons <- function(total, inpatient, outpatient, pharm, province){
  numberOfIcons = 10
  inpatient = round((inpatient/total)*numberOfIcons)
  outpatient = round((outpatient/total)*numberOfIcons)
  pharm = round((pharm/total)*numberOfIcons)
  fill = c(inpatient, outpatient, pharm)
  iconList = list()
  iconColorList = c("blue", "purple", "yellow")
  for(i in 1:3){
    icons <- awesomeIcons(
      icon = 'user',
      iconColor = iconColorList[i],
      library = 'fa'
      
    )
    fillType = paste0("fill",i)
    iconList[[fillType]]=icons
    iconList[paste0(fillType, "Number")] = fill[i]
  }
  return(iconList)
}

sout <- function(...){
  arguments <- list(...)
  string <- ""
  for(arg in arguments){
    string <- paste0(string, " ", arg)
  }
  cat(string, fill=TRUE)
  
}



