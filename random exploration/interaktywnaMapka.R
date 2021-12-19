
library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(leaflet)

df <- read.csv("Pointstest.csv")
df <- df %>% mutate(long = Longitude / 1e7, lat = Latitude / 1e7) %>% 
    mutate(time = as.Date(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01")))


ui <- fluidPage(

    titlePanel("Mapka"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("DatesMerge",
                        "Choose date:",
                        min = min(df$time),
                        max = max(df$time),
                        value = median(df$time),
                        timeFormat="%Y-%m-%d")
        ),

        mainPanel(
           leafletOutput("distPlot")
        )
    )
)


server <- function(input, output) {

    output$distPlot <- renderLeaflet({
        data <- df %>% filter(time == input$DatesMerge)
        map3 <- leaflet(data) %>% addTiles() %>% addMarkers(~long, ~lat, popup = ~X)
        nn <- nrow(data)-1
        for (i in 1:nn) 
            map3 <- map3 %>% 
            addPolylines(lat=c(data[i,]$lat,data[i+1,]$lat),lng=c(data[i,]$long,data[i+1,]$long))
        map3
    })
}


shinyApp(ui = ui, server = server)
