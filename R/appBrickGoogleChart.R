
shinyGoogleChart = function(tabNumber, tabItemsList, input, appData, rawData, chartType){
    selectedTabItem = tabItemsList[[tabNumber]]
    sidebarShownIds = selectedTabItem$sidebarShownIds
    googleChartId = tabItemsList[[tabNumber]]$googleChartOutputId
    year = input[[sidebarShownIds[5]]]
    xAxis = input[[sidebarShownIds[1]]]
    yAxis = input[[sidebarShownIds[2]]]
    state1 = input[[sidebarShownIds[3]]]
    state2 = input[[sidebarShownIds[4]]]
    color = input[[sidebarShownIds[6]]]
    size = input[[sidebarShownIds[7]]]
    id = appData$tabs$column0[[tabNumber]]

    if (color == "None") {
        color = NULL
    }
    if (size == "None") {
        size = NULL

    }
    if (year == "All Years") {
        year = c(19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
                 36, 37, 38)
    } else {
        year = as.numeric(year) - 2000
    }
    data = rawData$subsetData(
        rawData$cleanedData,
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
                "80 to 84 years"
            )
        ),
        list("Year", year)
    )

    chart = googleChart(
        title = "",
        elementId = googleChartId,
        chartType = chartType,
        headerTitles = c(id, xAxis, yAxis, color, size),
        data = data,
        column0 = id,
        column1 = xAxis,
        column2 = yAxis,
        column3 = color,
        column4 = size,
        keyColumns = appData$data$classNames,
        valueColumns = appData$data$valueNames,
        transform = TRUE
    )
    return(chart)
}



