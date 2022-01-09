library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(leaflet)
library(shinyTime)

df2 <- read.csv("PlacesVisitedtestData.csv")  %>% select(-PlaceId, -X)
 
df <- read.csv("PointstestData.csv")
dfa <- read.csv("ActivitySegmenttestData.csv") %>% 
    select(-X, -Distance) %>% 
    rename(startlong = StartingLongitude, startlat = StartingLatitude,
           endlong = EndingLongitude, endlat = EndingLatitude, 
           starttime = StartingtimeStampInMS, endtime = EndtimeStampInMS)

dfg1 <- dfa %>% select(startlong, startlat, starttime, User) %>%
    rename(Longitude = startlong, Latitude = startlat, TimeStampInMS = starttime)

dfg2 <- dfa %>% select(endlong, endlat, endtime, User) %>%
    rename(Longitude = endlong, Latitude = endlat, TimeStampInMS = endtime)

dfg <- rbind(dfg1, dfg2)

df <- df %>% select(-X) %>% rbind(dfg) %>%
    mutate(Activity = "UNKNOWN_ACTIVITY_TYPE", Place = NA) %>%
    mutate(long = Longitude / 1e7, lat = Latitude / 1e7, .keep = "unused") %>% 
    mutate(data = as.Date(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"))) %>%
    mutate(hm = format(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"), "%H:%M")) %>%
    mutate(Color = case_when(
        User == "User1" ~ "red",
        User == "User2" ~ "purple"
    )) %>% arrange(User, TimeStampInMS)


for (i in 1:nrow(dfa)){
    ind <- which(df$TimeStampInMS >= dfa[i,]$starttime & df$TimeStampInMS <= dfa[i,]$endtime)
    if(length(ind) > 1){
        ind <- ind[1:length(ind) - 1]
        df[ind,]$Activity <- dfa[i,]$ActivityType
    }
}

for (i in 1:nrow(df2)){
    places <- which(df$TimeStampInMS >= df2[i,]$StartTimeStampInMS & df$TimeStampInMS <= df2[i,]$EndTimeStampInMS)
    if(length(places) > 0){
        df[places,]$Place <- df2[i,]$Name
    }
}



df <- df %>% mutate(ActivityColor = case_when(
    Activity == "WALKING" ~ "blue",
    Activity == "IN_PASSENGER_VEHICLE" ~ "purple",
    Activity == "IN_TRAM" ~ "yellow",
    Activity == "IN_TRAIN" ~ "darkblue",
    Activity == "UNKNOWN_ACTIVITY_TYPE" ~ "grey",
    Activity == "IN_BUS" ~ "orange",
    Activity == "IN_SUBWAY" ~ "black"
))


server <- function(input, output, session) {

    output$distPlot <- renderLeaflet({
        
        df2 <- df %>% 
            filter(User %in% input$users) %>%
            filter(data == input$DatesMerge &
                       hm > format(as.POSIXct(input$timeS), "%H:%M") &
                       hm < format(as.POSIXct(input$timeE), "%H:%M"))
        
        map3 <- leaflet(df2) %>%
            setView(lng = 21.017532, lat = 52.237049, zoom = 7) %>%
            addAwesomeMarkers(~long,
                              ~lat,
                              icon = makeAwesomeIcon(
                                  icon = ~if_else(is.na(Place), "diamond", "map-marker"),
                                  library = "glyphicon",
                                  markerColor = ~Color,
                                  iconColor = "black"),
                              popup = ~Place)
        nn <- nrow(df2)-1
        if (nn > 0){
            for (i in 1:nn)
                if (df2$User[i] == df2$User[i+1]){
                    map3 <- map3 %>% 
                        addPolylines(lat=c(df2[i,]$lat,df2[i+1,]$lat),
                                     lng=c(df2[i,]$long,df2[i+1,]$long),
                                     color = df2[i,]$ActivityColor,
                                     popup = ~paste("Distance: ", round(raster::pointDistance(c(df2[i,]$long,
                                                                                               df2[i,]$lat),
                                                                                             c(df2[i+1,]$long,
                                                                                               df2[i+1,]$lat),
                                                                                             lonlat = TRUE), 0), "m"))
                }
            }
    
        esri <- c(grep("^OpenStreetMap.Mapnik", providers, value = TRUE),
          grep("^Esri.WorldImagery", providers, value = TRUE))

        for (provider in esri) {
            map3 <- map3 %>% addProviderTiles(provider, group = provider)
        }
        

        map3 %>%
            addLayersControl(baseGroups = names(esri),
                             options = layersControlOptions(collapsed = FALSE)) %>%
            addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
                       position = "bottomleft") %>%
            htmlwidgets::onRender("
                function(el, x) {
                    var myMap = this;
                    myMap.on('baselayerchange',
                        function (e) {
                            myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                        })
                }") %>%  addLegend(
                    position = "bottomright",
                    colors = unique(df$ActivityColor),
                    labels = unique(df$Activity), opacity = 1,
                    title = "Means of transport"
                )

    })
    
}

ui <- fluidPage(
    titlePanel("Page1")
)


ui2 <- fluidPage(
    titlePanel("Page2")
)

ui3 <- fluidPage(
    titlePanel("Mapka"),
    sidebarLayout(
        sidebarPanel(
            dateInput("DatesMerge",
                        "Choose date:",
                        min = min(df$data),
                        max = max(df$data),
                        value = median(df$data),
                        format="yyyy-mm-dd"),
            timeInput("timeS", "Start time:", minute.steps = 5),
            timeInput("timeE", "End time:", minute.steps = 5),
            checkboxGroupInput("users", 
                               "Users:", 
                               choices = unique(df$User),
                               selected = 1),
            submitButton("Update View", icon("sync"))
        ),

        mainPanel(
           leafletOutput("distPlot")
           #leafletOutput("plot2")
        )
    )
)

app_ui <- navbarPage(
    title = "Projekt TWD 2",
    tabPanel("Page1", ui),
    tabPanel("Page2", ui2),
    tabPanel("Page3", ui3),
    theme = bslib::bs_theme(bootswatch = "cosmo")
)

shinyApp(app_ui, server)
