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


dataset <- read.csv("~/2018data.csv", header = TRUE)
dataset$nuclear <- as.numeric(dataset$nuclear)
dataset$geo <- as.numeric(dataset$geo)
dataset$totGen <- rowSums(dataset[6:15])
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

dataset <- na.omit(dataset)


ilData <- subset(dataset, (dataset$abb == 'IL'))

colorPalette <- colorFactor(palette = c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7',
                                        '#999999', '#990000', '#000000'), levels = c(dataset$coal, dataset$oil, dataset$gas, 
                                                                                     dataset$nuclear, dataset$hydro, dataset$bio, dataset$wind, 
                                                                                     dataset$solar, dataset$geo, dataset$other))
if (dataset$coal > 0){
colorPalette2 <- colorFactor(palette = '#E69F00', domain = ilData$coal)
}

ilMap <- leaflet(ilData) %>%
         addTiles() %>%
         addCircles(lng=ilData$long, lat=ilData$lat, color = ~colorPalette2(coal), popup=ilData$name)




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
