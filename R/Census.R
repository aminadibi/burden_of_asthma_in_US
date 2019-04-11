setClass(
  # Set the name for the class
  "censusData",

  # Define the slots
  slots = c(
    filename = "character",
    provinces = "character",
    provinceOrder = "numeric",
    population = "numeric",
    provinceKeys = "numeric",
    year = "numeric",
    data = "data.frame"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    population = rep(0,13),
    provinces = as.character(rep(0,13)),
    provinceOrder = rep(0,13)
  ),

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)


setGeneric(name="readFile",
           def=function(object, filename)
           {
             standardGeneric("readFile")
           }
)
setMethod(f="readFile",signature="censusData",
          definition=function(object, filename){
            data <- read.csv(filename, sep=",", header=T)
            object@data <- data
            object@filename <- filename
            return(object)
          }
)
setGeneric(name="setProvinces",
           def=function(object)
           {
             standardGeneric("setProvinces")
           }
)
setMethod(f="setProvinces",signature="censusData",
          definition=function(object){
            if(length(object@data)==0){
              print("No data found. Please call readFile first.")
            } else {
              provinces = object@data$Geographic.name[1:14]
            }
            provinceOrder = seq(1,length(provinces),1)
            c = which(provinces=="Canada")
            d = which(provinceOrder==c)
            p = as.character(provinces[-c])
            po = provinceOrder[-d]
            names(po) = p
            object@provinces = p
            object@provinceOrder = po

          return(object)
          }
)
setGeneric(name="getPopulation",
           def=function(object)
           {
             standardGeneric("getPopulation")
           }
)
setMethod(f="getPopulation",signature="censusData",
          definition=function(object){
            if(object@provinces[1]=="0"){
              print("No provinces found. Please call setProvinces first.")
            } else{
            pop = object@data$Population..2016[object@provinceOrder]
            names(pop) = object@provinces
            object@population = pop
            }
            return(object)
            }
)


