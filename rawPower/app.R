#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet.providers)
library(leaflet.extras)


dataset <- read.csv("~/2018data.csv", header = TRUE)
dataset$nuclear <- as.numeric(dataset$nuclear)
dataset$geo <- as.numeric(dataset$geo)
dataset$totGen <- rowSums(dataset[,6:15])
dataset$percentCoal <- dataset$coal / dataset$totGen
dataset$percentOil <- dataset$oil / dataset$totGen
dataset$percentGas <- dataset$gas / dataset$totGen
dataset$percentNuclear <- dataset$nuclear / dataset$totGen
dataset$percentHydro <- dataset$hydro / dataset$totGen
dataset$percentBio <- dataset$bio / dataset$totGen
dataset$percentWind <- dataset$wind / dataset$totGen
dataset$percentSolar <- dataset$solar / dataset$totGen
dataset$percentGeo <- dataset$geo / dataset$totGen
dataset$percentOther <- dataset$other / dataset$totGen
dataset$renew <- dataset$hydro + dataset$bio + dataset$wind + dataset$solar + dataset$geo
dataset$nonRenew <- dataset$coal + dataset$oil + dataset$gas + dataset$nuclear + dataset$other
dataset$pcRenew <- dataset$renew / dataset$totGen
dataset$pcNonRenew <- dataset$nonRenew / dataset$totGen
dataset$primary <- colnames(dataset[,6:15])[max.col(dataset[,6:15], ties.method = 'first')]
dataset$primary <- factor(dataset$primary)
dataset <- subset(dataset, !(is.na(dataset$lat) & !(is.na(dataset$long))))
dataset18 <- subset(dataset, dataset$coal != 'NA' & dataset$oil != 'NA' & dataset$gas != 'NA' & dataset$nuclear != 'NA' & dataset$hydro != 'NA' 
                    & dataset$bio != 'NA' & dataset$wind != 'NA' & dataset$solar != 'NA' & dataset$geo != 'NA' & dataset$other != 'NA')

dataset2 <- read.csv('~/2000data.csv', header = TRUE)
dataset2$totGen <- rowSums(dataset2[,6:15])
dataset2$percentCoal <- dataset2$coal / dataset2$totGen
dataset2$percentOil <- dataset2$oil / dataset2$totGen
dataset2$percentGas <- dataset2$gas / dataset2$totGen
dataset2$percentNuclear <- dataset2$nuclear / dataset2$totGen
dataset2$percentHydro <- dataset2$hydro / dataset2$totGen
dataset2$percentBio <- dataset2$bio / dataset2$totGen
dataset2$percentWind <- dataset2$wind / dataset2$totGen
dataset2$percentSolar <- dataset2$solar / dataset2$totGen
dataset2$percentGeo <- dataset2$geo / dataset2$totGen
dataset2$percentOther <- dataset2$other / dataset2$totGen
dataset2$renew <- dataset2$hydro + dataset2$bio + dataset2$wind + dataset2$solar + dataset2$geo
dataset2$nonRenew <- dataset2$coal + dataset2$oil + dataset2$gas + dataset2$nuclear + dataset2$other
dataset2$pcRenew <- dataset2$renew / dataset2$totGen
dataset2$pcNonRenew <- dataset2$nonRenew / dataset2$totGen
dataset2$primary <- colnames(dataset2[,6:15])[max.col(dataset2[,6:15], ties.method = 'first')]
dataset2 <- subset(dataset2, dataset2$lat != 'NA' & dataset2$long != 'NA')
dataset00 <- subset(dataset2, dataset2$coal != 'NA' & dataset2$oil != 'NA' & dataset2$gas != 'NA' & dataset2$nuclear != 'NA' & dataset2$hydro != 'NA' 
                    & dataset2$bio != 'NA' & dataset2$wind != 'NA' & dataset2$solar != 'NA' & dataset2$geo != 'NA' & dataset2$other != 'NA')

dataset3 <- read.csv('~/2010data.csv', header = TRUE)
dataset3$totGen <- rowSums(dataset3[,6:15])
dataset3$percentCoal <- dataset3$coal / dataset3$totGen
dataset3$percentOil <- dataset3$oil / dataset3$totGen
dataset3$percentGas <- dataset3$gas / dataset3$totGen
dataset3$percentNuclear <- dataset3$nuclear / dataset3$totGen
dataset3$percentHydro <- dataset3$hydro / dataset3$totGen
dataset3$percentBio <- dataset3$bio / dataset3$totGen
dataset3$percentWind <- dataset3$wind / dataset3$totGen
dataset3$percentSolar <- dataset3$solar / dataset3$totGen
dataset3$percentGeo <- dataset3$geo / dataset3$totGen
dataset3$percentOther <- dataset3$other / dataset3$totGen
dataset3$renew <- dataset3$hydro + dataset3$bio + dataset3$wind + dataset3$solar + dataset3$geo
dataset3$nonRenew <- dataset3$coal + dataset3$oil + dataset3$gas + dataset3$nuclear + dataset3$other
dataset3$pcRenew <- dataset3$renew / dataset3$totGen
dataset3$pcNonRenew <- dataset3$nonRenew / dataset3$totGen
dataset3 <- subset(dataset3, dataset3$lat != 'NA' & dataset3$long != 'NA')
dataset3$primary <- colnames(dataset3[,6:15])[max.col(dataset3[,6:15], ties.method = 'first')]
dataset10 <- subset(dataset3, dataset3$coal != 'NA' & dataset3$oil != 'NA' & dataset3$gas != 'NA' & dataset3$nuclear != 'NA' & dataset3$hydro != 'NA' 
                    & dataset3$bio != 'NA' & dataset3$wind != 'NA' & dataset3$solar != 'NA' & dataset3$geo != 'NA' & dataset3$other != 'NA')


ilData <- subset(dataset18, dataset18$abb == 'IL')
colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                        '#999999', '#990000', '#000000'), domain = dataset18$primary)


years <- c(2000, 2010, 2018)
years2 <- c(2000, 2010, 2018)
states <- setNames(state.abb, state.name)[state.name]
states <- append(states, c("Washington DC" = "DC"))
states2 <- setNames(state.abb, state.name)[state.name]
states2 <- append(states, c("Washington DC" = "DC"))
sources <- c("coal", "oil", "gas", "nuclear", "hydro", "bio", "wind", "solar", "geo", "other", "Renewable", "Non-Renewable")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Raw Power"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE, 
                     # menu on the left hand side
                     sidebarMenu(
                         menuItem("Illinois Map", tabName = "ill", icon = icon("map-pin")),
                         menuItem("State Comparisons", tabName = "states", icon = icon("equals")),
                         menuItem("US Comparisons", tabName = "usa", icon = icon("flag-usa")),
                         menuItem("About", tabName = "credits", icon = icon("check"))
                     ),
                     # dropdown menus
                     sidebarMenuOutput("menu"),
                     selectInput("Year", "Select the first year to visualize", years, selected = 2000),
                     selectInput("Year2", "Select the second year to visualize", years2, selected = 2018),
                     selectInput("State", "Select one state", states, selected = "IL"),
                     selectInput("State2", "Select another state", states, selected = "IL")
    ),
    dashboardBody(
        tabItems(
            # Illinois leaflet map for 2018
            tabItem(tabName = "ill",
                    fluidRow(
                        column(3,
                               checkboxGroupInput("checkGroup1",
                                                  h4("Select which energy source to filter"),
                                                  choices = c("All", sources),
                                                  selected = "All")  
                        ),
                        column(8,
                               box(title = "Leaflet Map of Illinois Power Plants in 2018", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map1")
                               )
                        )
                    )
            ),
            # different state comparisons
            tabItem(tabName = "states",
                    fluidRow(
                        column(1,
                               checkboxGroupInput("checkGroup2",
                                                  h4("Select which energy source to filter"),
                                                  choices = c("All", sources),
                                                  selected = "All")  
                        ),
                        column(5,
                               box(title = "Map 1", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map2")
                               )
                        ),
                        column(1,
                               checkboxGroupInput("checkGroup3",
                                                  h4("Select which energy source to filter"),
                                                  choices = c("All", sources),
                                                  selected = "All")  
                        ),
                        column(5,
                               box(title = "Map 2", solidHeader = TRUE,
                                   status = "primary", width = 12,
                                   leafletOutput("map3")
                               )
                        ),
                        column(1,
                               checkboxGroupInput("checkGroupBoth",
                                                  h4("Select which energy source to filter"),
                                                  choices = c("All", sources),
                                                  selected = "All")
                    )
                    )
            ),
            # US Map
            tabItem(tabName = "usa",
                    column(1,
                           checkboxGroupInput("checkGroup4",
                                              h4("Select which energy source to filter"),
                                              choices = c("All", sources))  
                    ),
                    column(8,
                           box(title = "US Map", solidHeader = TRUE,
                               status = "primary", width = 12,
                               leafletOutput("map4")
                           )
                    ),
                    column(10, 
                            sliderInput("slider1", '', min = 0, max = 35000000, value = c(0, 35000000), width = 100 
                            ),
                            h2("Use the first year and state options to control the map")
                    )
            ),
            #about
            tabItem(tabName = "credits",
                    h1("About"),
                    h2("Created by: Matthew Ghuneim"),
                    h3("This was the second project for CS 424 Spring 2021. Given 3 data files, I did preprocessing and cleaning in both Python and R. The original data file is located here: https://www.epa.gov/egrid/download-data")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # check box reactive functions
    checkGroup1 <- reactive({
        if ("All" %in% input$checkGroup1){
            dataset18
        }
        else if ("Renewable" %in% input$checkGroup1){
            dataset18 <- subset(dataset18, dataset18$primary %in% c('hydro', 'bio', 'wind', 'solar', 'geo'))
        }
        else if ("Non-Renewable" %in% input$checkGroup1){
            dataset18 <- subset(dataset18, dataset18$primary %in% c('coal', 'oil', 'gas', 'nuclear', 'other'))
        }
        else {
            dataset18 <- subset(dataset18, dataset18$primary %in% input$checkGroup1)
        }
    })
    
    checkGroup2 <- reactive({
        if (input$Year == '2000'){
            newdata <- subset(dataset00, dataset00$abb == input$State)
        }
        if (input$Year == '2010'){
            newdata <- subset(dataset10, dataset10$abb == input$State)
        }
        if (input$Year == '2018'){
            newdata <- subset(dataset18, dataset18$abb == input$State)
        }
        if (!is.null(newdata)){
            if ("All" %in% input$checkGroup2){
                newdata
            }
            else if ("Renewable" %in% input$checkGroup2){
                newdata <- subset(newdata, newdata$primary %in% c('hydro', 'bio', 'wind', 'solar', 'geo'))
            }
            else if ("Non-Renewable" %in% input$checkGroup2){
                newdata <- subset(newdata, newdata$primary %in% c('coal', 'oil', 'gas', 'nuclear', 'other'))
            }
            else {
                newdata <- subset(newdata, newdata$primary %in% input$checkGroup2)
            }
        }
    })
    
    checkGroup3 <- reactive({
        if (input$Year2 == '2000'){
            newdata2 <- subset(dataset00, dataset00$abb == input$State2)
        }
        if (input$Year2 == '2010'){
            newdata2 <- subset(dataset10, dataset10$abb == input$State2)
        }
        if (input$Year2 == '2018'){
            newdata2 <- subset(dataset18, dataset18$abb == input$State2)
        }
        
        if (!is.null(newdata2)){
            if ("All" %in% input$checkGroup3){
                newdata2
            }
            else if ("Renewable" %in% input$checkGroup3){
                newdata2 <- subset(newdata2, newdata2$primary %in% c('hydro', 'bio', 'wind', 'solar', 'geo'))
            }
            else if ("Non-Renewable" %in% input$checkGroup3){
                newdata2 <- subset(newdata2, newdata2$primary %in% c('coal', 'oil', 'gas', 'nuclear', 'other'))
            }
            else {
                newdata2 <- subset(newdata2, newdata2$primary %in% input$checkGroup3)
            }
        }
    })
    
    checkGroup4 <- reactive({
        
        if (input$Year == '2000'){
            newdata2 <- subset(dataset00, dataset00$abb == input$State)
        }
        if (input$Year == '2010'){
            newdata2 <- subset(dataset10, dataset10$abb == input$State)
        }
        if (input$Year == '2018'){
            newdata2 <- subset(dataset18, dataset18$abb == input$State)
        }
        
        if (!is.null(newdata2)){
            newdata2 <- subset(newdata2, newdata2$totGen >= input$slider1[1] & newdata2$totGen <= input$slider1[2])
            if ("All" %in% input$checkGroup4){
                newdata2
            }
            else if ("Renewable" %in% input$checkGroup3){
                newdata2 <- subset(newdata2, newdata2$primary %in% c('hydro', 'bio', 'wind', 'solar', 'geo'))
            }
            else if ("Non-Renewable" %in% input$checkGroup3){
                newdata2 <- subset(newdata2, newdata2$primary %in% c('coal', 'oil', 'gas', 'nuclear', 'other'))
            }
            else {
                newdata2 <- subset(newdata2, newdata2$primary %in% input$checkGroup4)
            }
        }
    })
        
    
    output$map1 <- renderLeaflet({
        ilData <- checkGroup1()
        ilMap <- subset(ilData, ilData$abb == 'IL')
        leaflet(ilMap) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addCircles(lng=ilMap$long, lat=ilMap$lat, color = ~colorPalette(ilMap$primary), popup=paste(ilMap$name)) %>%
            addResetMapButton() %>%
            setView(-89, 40, zoom = 6) %>%
            addLegend("bottomright", colorPalette, values = ilMap$primary, title = "Energy Sources", opacity = 1)
    })
    
    output$map2 <- renderLeaflet({
        stateData <- checkGroup2()
        colorPalette2 <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                '#999999', '#990000', '#000000'), domain = stateData$primary)
        leaflet(stateData) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addProviderTiles(providers$Stamen.Toner) %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addCircles(lng=stateData$long, lat=stateData$lat, color = ~colorPalette2(stateData$primary), popup=paste('Name:', stateData$name, "<br>", 
                                                                                                                     'Percent Renewable:', scales::percent(stateData$pcRenew), '<br>', 
                                                                                                                     'Percent Non-Renewable:', scales::percent(stateData$pcNonRenew), '<br>')) %>%
            addResetMapButton() %>%
            addLegend("bottomright", colorPalette2, values = stateData$primary, title = "Energy Sources", opacity = 1)
    })
    
    output$map3 <- renderLeaflet({
        stateData <- checkGroup3()
        colorPalette2 <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                 '#999999', '#990000', '#000000'), domain = stateData$primary)
        leaflet(stateData) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addProviderTiles(providers$Stamen.Toner) %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addCircles(lng=stateData$long, lat=stateData$lat, color = ~colorPalette2(stateData$primary), popup=paste('Name:', stateData$name, "<br>", 
                                                                                                                     'Percent Renewable:', scales::percent(stateData$pcRenew), '<br>', 
                                                                                                                     'Percent Non-Renewable:', scales::percent(stateData$pcNonRenew), '<br>')) %>%
            addResetMapButton() %>%
            addLegend("bottomright", colorPalette2, values = stateData$primary, title = "Energy Sources", opacity = 1)
    })
    
    output$map4 <- renderLeaflet({
        stateData <- checkGroup4()
        colorPalette2 <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                                 '#999999', '#990000', '#000000'), domain = stateData$primary)
        leaflet(stateData) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addProviderTiles(providers$Stamen.Toner) %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
            addCircles(lng=stateData$long, lat=stateData$lat, color = ~colorPalette2(stateData$primary), popup=paste('Name:', stateData$name, "<br>", 
                                                                                                                     'Percent Renewable:', scales::percent(stateData$pcRenew), '<br>', 
                                                                                                                     'Percent Non-Renewable:', scales::percent(stateData$pcNonRenew), '<br>')) %>%
            addResetMapButton() %>%
            setView(-95.665, 37.6, zoom = 4) %>%
            addLegend("bottomright", colorPalette2, values = stateData$primary, title = "Energy Sources", opacity = 1)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
