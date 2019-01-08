library(readr)
setClass(
  # Set the name for the class
  "costData",

  # Define the slots
  slots = c(
    filename = "character",
    provinces = "character",
    provinceOrder = "numeric",
    population = "numeric",
    year = "numeric",
    data = "data.frame"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    population = rep(0,13),
    provinces = as.character(rep(0,13)),
    provinceOrder = rep(0,13)
  )

  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
)


setGeneric(name="readRDS",
           def=function(object, filename)
           {
             standardGeneric("readRDS")
           }
)
setMethod(f="readRDS",signature="costData",
          definition=function(object, filename){
            data <- read_rds(filename)
            object@data <- data
            return(object)
          }
)
setGeneric(name="setProvinces2",
           def=function(object)
           {
             standardGeneric("setProvinces2")
           }
)
setMethod(f="setProvinces2",signature="costData",
          definition=function(object){
            if(length(object@data)==0){
              print("No data found. Please call readFile first.")
            } else {
              provinces = object@data$Geographic.name[1:14]
            }
            provinceOrder = seq(1,length(provinces),1)
            c = which(provinces=="Canada")
            d = which(provinceOrder==c)
            object@provinces = as.character(provinces[-c])
            object@provinceOrder = provinceOrder[-d]
            return(object)
          }
)
setGeneric(name="getPopulation2",
           def=function(object)
           {
             standardGeneric("getPopulation2")
           }
)
setMethod(f="getPopulation2",signature="costData",
          definition=function(object){
            if(object@provinces[1]=="0"){
              print("No provinces found. Please call setProvinces first.")
            } else{
              pop = object@data$Population..2016[object@provinceOrder]
              object@population = pop
            }
            return(object)
          }
)


