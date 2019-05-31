library(R6)


ColumnOptionsObject <- R6Class(
    "ColumnOptionsObject",
    public = list(

        # Fields
        optionTypeList = NULL,
        columnOptionsList = NULL,

        # Constructor
        initialize = function(
            optionTypeList,
            columnOptionsList
        ){
            self$optionTypeList = optionTypeList
            self$columnOptionsList = columnOptionsList
        },

        generateAllColumnOptions = function(dataSubClasses, columnTypes){
            i = 1
            generatedOptions = list()
            for(columnOptions in self$columnOptionsList) {
                optionType = self$optionTypeList[i]
                columnType = columnTypes[i]
                generatedOptions = c(
                    generatedOptions,
                    list(self$generateColumnOptions(i, dataSubClasses, columnType, optionType)))
                i = i + 1
            }
            return(generatedOptions)
        },
        generateColumnOptions = function(i, dataSubClasses, columnType, optionType){
            if(optionType=="given") {
                return(c(self$columnOptionsList[[i]]))
            } else if(optionType=="generate") {
                index = which(dataSubClassNames==columnType)
                if(dataSubClasses[[index]]$hasPrettyOptions){
                    return(c(dataSubClasses[[index]]$prettyOptions))
                } else {
                    return(c(dataSubClasses[[index]]$options))
                }
            } else if(optionType=="mixed") {
                gen = self$generateColumnOptions(i, dataSubClasses, columnType, "generate")
                given = self$generateColumnOptions(i, dataSubClasses, columnType, "given")
                return(c(given, gen))
            }
        }
    )
)
