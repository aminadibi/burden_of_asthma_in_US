shinyPlotly = function(tabItemDash){
        sout("~~~ Making Graph ~~~")
        dataSubClassNames = c("Year", "State", "Sex", "Age")
        dashGraph <- DashGraph$new(dataSubClassNames)
        p <- reactive({
            selectedTabItem <- tabItemsList[[input$selectedTab]]
            if (selectedTabItem$dropdown) {
                dropdownSelected = input[[selectedTabItem$dropdownId]]
            } else {
                dropdownSelected = selectedTabItem$dropdownChoices
            }
            arguments = list()

            count = 3
            print(count)
            for (i in 1:length(dataSubClassNames)) {
                shown = selectedTabItem$sidebarShownIds[i]
                hidden = selectedTabItem$sidebarHiddenBoxIds[i]
                if (input[[shown]] == "All" ||
                    is.null(input[[hidden]])) {
                    arguments[[count]] <- list(dataSubClassNames[i], "total")
                } else if (input[[shown]] == "Select") {
                    arguments[[count]] <- list(dataSubClassNames[i], input[[hidden]])
                }
                count = count + 1

            }
            arguments[[1]] <- rawData
            arguments[[2]] <- dropdownSelected
            print(dropdownSelected)
            print(arguments)
            do.call(dashGraph$drawGraph, args = arguments)
            # dashGraph$drawGraph(rawData, dropdownSelected, list("Year", 19), list("Sex", "Female"),
            #                     list("State", c("Iowa", "Washington")))
        })
        p()
    })
