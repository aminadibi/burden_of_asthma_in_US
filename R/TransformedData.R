source('./R/helper_functions.R')
#source('../R/initialize.R')
library(R6)



TransformedData <- R6Class(
    "TransformedData",
    public = list(

        # Fields
        data = NULL,

        # Constructor
        initialize = function(
            data
        ){
            self$data = data
        },

        #' @title addDataColumn
        #' @description
        #' REQUIRES: data is a data frame
        #' @param groupByColumn column name to group by
        #' @param groupType function to group by, one of {interval, comparison}
        #' @param newColumnName string: name of the new column
        #' @param intervalSize integer: size of the interval to group by
        #' @param string boolean: whether the column contains strings
        groupDataColumn = function(groupByColumn, groupType, newColumnName, intervalSize = NULL,
                                   intervalNames = NULL, string = FALSE) {

            groupTypes = c("interval", "comparison")
            groupType = match.arg(groupType, groupTypes, several.ok = FALSE)
            column = self$data[[groupByColumn]]

            groups = switch(groupType,
                            interval = self$groupByInterval(intervalSize, column, intervalNames, string),
                            comparison = self$comparison(column))

            self$data[[newColumnName]] = groups
        },

        #' @param intervalSize integer: size of the interval to group by
        #' @param column data column
        #' @param intervalNames string list: list of names for each interval
        #' @param string boolean: whether the column contains strings
        groupByInterval = function(intervalSize, column, intervalNames, string = FALSE) {
            if(string) {
                columnOptions = as.character(unique(column))
                column = match(as.character(column), columnOptions)
            } else {
                column = as.numeric(as.character(column))
            }
            lower = min(column, na.rm = TRUE)
            lowerModulo = lower %% intervalSize
            lowerDivisors = c()

            for(congruenceClass in 0:(intervalSize - 1)) {
                b = lower + congruenceClass
                lowerDivisor = floor(b / intervalSize)
                lowerDivisors = c(lowerDivisors, lowerDivisor)
            }

            groups = c()
            for(cell in column) {
                modulo = cell %% intervalSize
                divisor = floor(cell / intervalSize)
                index = (modulo - lowerModulo) %% intervalSize + 1
                group = divisor - lowerDivisors[index] + 1
                group = intervalNames[group]
                groups = c(groups, group)
            }
            return(groups)

        }
    )
)




