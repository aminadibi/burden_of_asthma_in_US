
shinyGoogleChart = function(tabNumber, tabItemsList, input, appData, rawData, chartType){

    labels = c("QALYs Lost", "Indirect Cost", "Direct Cost", "Year Combined", "Age Groups")
    values = c("qalysLost", "indirectCost", "directCost", "yearCombined", "ageCombined")
    labelValueList = data.frame(labels, values)

    selectedTabItem = tabItemsList[[tabNumber]]
    sidebarShownIds = selectedTabItem$sidebarShownIds
    googleChartId = tabItemsList[[tabNumber]]$googleChartOutputId
    year = input[[sidebarShownIds[5]]]
    xAxisLabel = input[[sidebarShownIds[1]]]
    xAxis = convertLabelToValue(xAxisLabel, labelValueList)
    yAxisLabel = input[[sidebarShownIds[2]]]
    yAxis = convertLabelToValue(yAxisLabel, labelValueList)
    state1 = input[[sidebarShownIds[3]]]
    state2 = input[[sidebarShownIds[4]]]
    colorLabel = input[[sidebarShownIds[6]]]
    sizeLabel = input[[sidebarShownIds[7]]]
    color = convertLabelToValue(colorLabel, labelValueList)
    size = convertLabelToValue(sizeLabel, labelValueList)
    id = appData$tabs$column0[[tabNumber]]

    if (colorLabel == "None") {
        color = NULL
    }
    if (sizeLabel == "None") {
        size = NULL

    }
    if (year == "All Years" || xAxis == "yearCombined" || yAxis == "yearCombined") {
        year = c(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                 36, 37, 38)
    } else {
        year = as.numeric(year) - 2000
    }
    data = rawData$subsetData(
        rawData$transformedData,
        list("State", c(state1, state2)),
        list("Sex", c("Female", "Male")),
        list(
            "Age",
            c(
                "15 to 19 years",
                "20 to 24 years",
                "25 to 29 years",
                "30 to 34 years",
                "35 to 39 years",
                "40 to 44 years",
                "45 to 49 years",
                "50 to 54 years",
                "55 to 59 years",
                "60 to 64 years",
                "65 to 69 years",
                "70 to 74 years",
                "75 to 79 years",
                "80 to 84 years",
                "85 and over"
            )
        ),
        list("Year", year)
    )

    chart = googleChart(
        title = "",
        elementId = googleChartId,
        chartType = chartType,
        headerTitles = c(id, xAxisLabel, yAxisLabel, colorLabel, sizeLabel),
        data = data,
        column0 = id,
        column1 = xAxis,
        column2 = yAxis,
        column3 = color,
        column4 = size,
        keyColumns = appData$data$transformNames,
        valueColumns = appData$data$valueNames,
        transform = TRUE
    )
    return(chart)
}



convertLabelToValue = function(label, labelValueFrame) {

    values = labelValueFrame$values
    labels = labelValueFrame$labels
    for(index in 1:length(values)) {
        labelKey = as.vector(labels[index])
        value = as.vector(values[index])
        if(label == labelKey) {
            return(value)
        }
    }
    return(label)
}


