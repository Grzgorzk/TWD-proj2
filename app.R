library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(leaflet)
library(shinyTime)
library(ggplot2)
library(forcats)
library(stringi)
library(tm)
library(shinydashboard)
library(shinyWidgets)
library(plotly)

activitydf <- read.csv("data/dataCSV/ActivitySegmentalmostFinal.csv")

pointsdf <- read.csv("data/dataCSV/PointsalmostFinal.csv")

placevisitdf <- read.csv("data/dataCSV/PlacesVisitedalmostFinal.csv", encoding = "UTF-8") 

activity2 <- activitydf %>%  mutate(StartingHour = format(as.POSIXct(StartingtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"), "%H")) %>%
    mutate(StartingMinute = format(as.POSIXct(StartingtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"), "%M")) %>%
    mutate(EndHour = format(as.POSIXct(EndtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"), "%H")) %>%
    mutate(EndMinute = format(as.POSIXct(EndtimeStampInMS / 1000, origin="1970-01-01",tz = "Europe/Warsaw"	), "%M")) %>% 
    mutate(DurationInMinutes = (EndtimeStampInMS-StartingtimeStampInMS)/60000) %>% 
    mutate(MiddleOfActivityHour = 
               as.numeric(format(as.POSIXct((EndtimeStampInMS+StartingtimeStampInMS)/2000, origin="1970-01-01",tz = "Europe/Warsaw"), "%H"))) %>% 
    mutate(MiddleOfActivitiMin=as.numeric(format(as.POSIXct((EndtimeStampInMS+StartingtimeStampInMS)/2000, origin="1970-01-01",tz = "Europe/Warsaw"), "%M"))) %>% 
    mutate(MiddleOfActivity = MiddleOfActivityHour+MiddleOfActivitiMin/60) %>% 
    mutate(Weekday=format(as.POSIXct((EndtimeStampInMS+StartingtimeStampInMS)/2000, origin="1970-01-01",tz = "Europe/Warsaw"), "%a")) %>% 
    mutate(dates = as.Date(as.POSIXct(StartingtimeStampInMS / 1000, origin="1970-01-01"))) 

df2 <- read.csv("data/dataCSV/PlacesVisitedalmostFinal.csv", encoding = "UTF-8")  %>% select(-PlaceId, -X)

df <- read.csv("data/dataCSV/PointsalmostFinal.csv", encoding = "UTF-8")
dfa <- read.csv("data/dataCSV/ActivitySegmentalmostFinal.csv", encoding = "UTF-8") %>% 
    select(-X, -Distance) %>% 
    rename(startlong = StartingLongitude, startlat = StartingLatitude,
           endlong = EndingLongitude, endlat = EndingLatitude, 
           starttime = StartingtimeStampInMS, endtime = EndtimeStampInMS)

dfg1 <- dfa %>% select(startlong, startlat, starttime, User) %>%
    rename(Longitude = startlong, Latitude = startlat, TimeStampInMS = starttime)

dfg2 <- dfa %>% select(endlong, endlat, endtime, User) %>%
    rename(Longitude = endlong, Latitude = endlat, TimeStampInMS = endtime)

dfg <- rbind(dfg1, dfg2)

#rbind(dfg) %>%

df <- df %>% select(-X) %>%
    mutate(Activity = "UNKNOWN_ACTIVITY_TYPE", Place = NA) %>%
    mutate(long = Longitude / 1e7, lat = Latitude / 1e7, .keep = "unused") %>% 
    mutate(data = as.Date(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"))) %>%
    mutate(hm = format(as.POSIXct(TimeStampInMS / 1000, origin="1970-01-01"), "%H:%M")) %>%
    mutate(Color = case_when(
        User == "User1" ~ "red",
        User == "User2" ~ "purple"
    )) %>% arrange(User, TimeStampInMS)


for (i in 1:nrow(dfa)){
    ind <- which(df$TimeStampInMS >= dfa[i,]$starttime &
                     df$TimeStampInMS <= dfa[i,]$endtime &
                     df$User == dfa[i,]$User)
    if(length(ind) > 1){
        ind <- ind[1:length(ind) - 1]
        df[ind,]$Activity <- dfa[i,]$ActivityType
    }
}

# for (i in 1:nrow(df2)){
#     places <- which(df$TimeStampInMS >= df2[i,]$StartTimeStampInMS &
#                         df$TimeStampInMS <= df2[i,]$EndTimeStampInMS &
#                         df$User == df2[i,]$User)
#     if(length(places) > 0){
#         df[places,]$Place <- df2[i,]$Name
#     }
# }
for (i in 1:nrow(df2)){
    places <- which((df$lat == df2[i,]$Latitude / 1e7) & (df$long == df2[i,]$Longitude / 1e7))
    df[places,]$Place <- df2[i,]$Name              
}

df <- df %>% mutate(ActivityColor = case_when(
    Activity == "WALKING" ~ "blue",
    Activity == "IN_PASSENGER_VEHICLE" ~ "purple",
    Activity == "IN_TRAM" ~ "yellow",
    Activity == "IN_TRAIN" ~ "darkblue",
    Activity == "UNKNOWN_ACTIVITY_TYPE" ~ "grey",
    Activity == "IN_BUS" ~ "orange",
    Activity == "IN_SUBWAY" ~ "black",
    Activity == "SKIING" ~ "white"
))

acti <- activitydf %>%
    mutate(StartTime = as.POSIXct(StartingtimeStampInMS/1000, origin = "1970-01-01")) %>%
    mutate(EndTime = as.POSIXct(EndtimeStampInMS/1000, origin = "1970-01-01")) %>%
    mutate(data = as.Date(StartTime)) %>%
    group_by(User, data) %>%
    summarise(dystans = sum(Distance)/1000) %>%
    mutate(cum_distance = cumsum(dystans))

daty <- sort(unique(acti$data))


server <- function(input, output, session) {
    
    
    output$distanceBox <- renderInfoBox({
        infoBox(
            "Total distance", paste0(25 + input$count, "%"), icon = icon("road"),
            color = "purple"
        )
    })
    
    output$FirstGKPlot <- renderPlotly({
        
        
        
        activity3<- activity2 %>% filter(User %in% input$Users) %>% 
          filter(DurationInMinutes<=300)
        activity3$Weekday <- factor(activity3$Weekday, c("Mon","Tue","Wed", "Thu", "Fri", "Sat", "Sun"))
        if(input$weekday=="day"){
            p<-ggplot(activity3, aes(y=Distance/1000, x=MiddleOfActivity, group=User, color=User, size=sqrt(DurationInMinutes)/3))+
                geom_point()+
                ylab("Distance in km")+ 
                scale_size_identity()+
                xlab("Middle time of activity (hours)")+
                ylim(0,40)}
        
        else{
            p<-ggplot(activity3, aes(y=Distance/1000, x=Weekday, group=User, color=User, size=sqrt(DurationInMinutes)/3))+
                geom_jitter(height = 0, width=0.3)+ 
                scale_size_identity()+
                ylab("Distance in km")+
                xlab("weekday of activity")+
                ylim(0,40)
        }
        p
    })
    
    output$SecondGKPlot<- renderPlot({
      Users<-c('User1', 'User2', 'User3')
      newdf<-data.frame(Users) 
      newdf$PartyTime<-c(
        sum(activity2 %>% filter((StartingHour<4)&(User=="User1")&(DurationInMinutes<300)) %>% select(DurationInMinutes)),
        sum(activity2 %>% filter((StartingHour<4)&(User=="User2")&(DurationInMinutes<300)) %>% select(DurationInMinutes)),
        sum(activity2 %>% filter((StartingHour<4)&(User=="User3")&(DurationInMinutes<300)) %>% select(DurationInMinutes))
      )
      
      
      p<- ggplot(newdf, aes(y=Users, x=PartyTime)) + geom_col(fill="darkblue")+coord_flip()
      p
      
      
      
    })
    
    output$Mapka <- renderLeaflet({
        
        dfm <- df %>% 
            filter(User %in% input$users) %>%
            filter(data == input$DatesMerge &
                       hm > format(as.POSIXct(input$timeS), "%H:%M") &
                       hm < format(as.POSIXct(input$timeE), "%H:%M"))
        
    
        if (length(dfm$User) == 0){
            map3 <- leaflet(dfm) %>% setView(lng = 21.017532, lat = 52.237049, zoom = 7)
        }
        else{
            map3 <- leaflet(dfm) %>% addAwesomeMarkers(~long,
                                  ~lat,
                                  icon = makeAwesomeIcon(
                                      icon = ~if_else(is.na(Place), "diamond", "map-marker"),
                                      library = "glyphicon",
                                      markerColor = ~Color,
                                      iconColor = "black"),
                                  popup = ~Place)
            nn <- nrow(dfm)-1
            if (nn > 0){
                for (i in 1:nn)
                    if (dfm$User[i] == dfm$User[i+1]){
                        map3 <- map3 %>% 
                            addPolylines(lat=c(dfm[i,]$lat,dfm[i+1,]$lat),
                                         lng=c(dfm[i,]$long,dfm[i+1,]$long),
                                         color = dfm[i,]$ActivityColor,
                                         popup = ~paste("Distance: ", round(raster::pointDistance(c(dfm[i,]$long,
                                                                                                    dfm[i,]$lat),
                                                                                                  c(dfm[i+1,]$long,
                                                                                                    dfm[i+1,]$lat),
                                                                                                  lonlat = TRUE), 0), "m"))
                    }
            }
            
        }
            map3 %>% addProviderTiles("OpenStreetMap.Mapnik", group = "Map View") %>%
                addProviderTiles("Esri.WorldImagery", group = "Satellite View") %>%
                addLayersControl(baseGroups = c("Map View", "Satellite View"),
                                 options = layersControlOptions(collapsed = FALSE)) %>%
                addLegend(
                    position = "bottomright",
                    colors = unique(df$ActivityColor),
                    labels = tolower(
                        stri_replace_all_regex(unique(df$Activity),
                                               pattern = c("IN_|_TYPE", "_"),
                                               replacement = c(""," "),
                                               vectorize_all = FALSE)),
                    opacity = 1,
                    title = "Mode of transport"
                    )
        
        
    })
    
    
    output$carbon <- plotly::renderPlotly({
        df <- activitydf %>%
            mutate(sladPoj = case_when(ActivityType ==  "WALKING" ~ 0   ,
                                       ActivityType == "IN_PASSENGER_VEHICLE" ~ 96,
                                       ActivityType =="IN_TRAM" ~ 35,
                                       ActivityType =="IN_TRAIN" ~ 41,
                                       ActivityType =="IN_BUS" ~52,
                                       ActivityType =="IN_SUBWAY" ~ 31,
                                       ActivityType =="UNKNOWN_ACTIVITY_TYPE" ~ 0,
                                       TRUE ~ 0)
            ) %>%
            mutate(sladW = sladPoj*Distance/1000) %>% 
            mutate(StartTime = as.POSIXct(StartingtimeStampInMS/1000, origin = "1970-01-01")) %>% 
            mutate(EndTime = as.POSIXct(EndtimeStampInMS/1000, origin = "1970-01-01")) %>% 
            filter(ActivityType != "WALKING" & ActivityType != "SKIING")
        
        
        df %>% 
            filter( StartTime > as.POSIXct(input$days[1]) & EndTime < as.POSIXct(input$days[2]+1)) %>% 
            mutate(ActivityType = case_when(ActivityType == "IN_PASSENGER_VEHICLE" ~ "IN PASSENGER VEHICLE",
                                            TRUE ~ "OTHER" ,
            )) %>% 
            group_by(User, ActivityType) %>%
            summarise(CarbonFootprint = sum(sladW)/1000)  %>% 
            mutate(ActivityType = fct_reorder(ActivityType, CarbonFootprint, .desc = FALSE)) %>% 
            ggplot(aes(x = User,y = CarbonFootprint, fill = ActivityType)) +
            geom_col() +
            theme_bw() +
            labs(title = "Estimated carbon footprint of our travels",
                 y = "Carbon footprint [kg]",
                 x = "")+
            coord_flip()
        
        
    })
    

    
    output$TypAktywnosci <- plotly::renderPlotly({
        
        df <- activitydf %>% 
            group_by(User, ActivityType) %>% 
            summarise(odl = sum(Distance)/1000, czas = (sum(EndtimeStampInMS)-sum(StartingtimeStampInMS))/(1000*60*60)  ) %>% 
            filter(User == input$whichUser) %>% 
            rename(value = input$kategoria) %>% 
            mutate(ActivityType = fct_reorder(ActivityType, value, .desc = F)) 
        
        p <- ggplot(df, aes(x = ActivityType, y=value) ) +
            geom_col( ) +
            theme_bw()+
            scale_x_discrete(breaks=df$ActivityType,
                             labels=stri_replace_all_fixed(df$ActivityType, '_', ' ')) +
            coord_flip()
        
        if(input$kategoria == "odl"){
            p <- p +
                labs(title = "Distance travelled by Activity Type",
                     y = "Distance[km]",
                     x = "")
        }else{
            p <- p +
                labs(title = "Time spent by Activity Type",
                     y = "Time[h]",
                     x = "")
        }
        
        p
    })
    
    output$liniowy <- plotly::renderPlotly({
        
        x <- data.frame(User = c("User3", "User3"), data = c(daty[1], daty[2]), dystans = c(0, 0), cum_distance = c(0,0))
        
        acti <- rbind(acti, x)
        
        acti %>%
            ggplot(aes(x=data, y = cum_distance, color=User)) +
            geom_line()+
            labs(title = "Total distance travelled",
                 y = "Distance[km]",
                 x = "")+
            theme_bw() +
            scale_x_continuous(breaks= daty[seq(1, 29, by=4)])+
            theme(axis.text.x = element_text(angle = 45))
    }
    )
}

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Mapka", tabName = "mapka", icon = icon("map")),
        menuItem("GrzegorzK", tabName = "grzegorzk", icon = icon("running")),
        menuItem("MarcelW", tabName = "marcelw", icon = icon("road"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "mapka",
                fluidRow(
                    column(width = 3, box(width = NULL,
                        dateInput("DatesMerge",
                                  "Choose date:",
                                  min = min(df$data),
                                  max = max(df$data),
                                  value = median(df$data),
                                  format="yyyy-mm-dd"),
                        timeInput("timeS", "Start time:", minute.steps = 5),
                        timeInput("timeE", "End time:", minute.steps = 5),
                    #     checkboxGroupInput("users", 
                    #                        "Users:", 
                    #                        choices = unique(df$User),
                    #                        selected = 1, inline = TRUE)
                    # ),
                    selectInput("users", "User",
                                choices = unique(df$User), selected = 1)),
                    infoBoxOutput("distanceBox", width = NULL),
                    infoBox("New Orders1", 10 * 2, icon = icon("credit-card"), width = NULL),
                    infoBox("New Orders1", 10 * 2, icon = icon("credit-card"), width = NULL)
                    ),
                    box(width = 8,
                        leafletOutput("Mapka", height = 600)
                    )
                    )
        ),
        
        tabItem(tabName = "grzegorzk",
                fluidRow(
                    box(
                        checkboxGroupInput("Users", "Users", c("User1", "User2", "User3"), c("User1","User2", "User3")),
                        radioButtons("weekday", "Do you want to see week perspective or day perspective?", c("week", "day"))
                    )
                ),
                fluidRow(
                    box(
                        plotlyOutput("FirstGKPlot")
                    ),
                    box(
                        plotOutput("SecondGKPlot")
                    )
                )
        ),
        tabItem(tabName = "marcelw",
                fluidRow(
                    box(
                        selectInput(
                            inputId = "whichUser",
                            label = "Choose user:",
                            choices = list("User1", "User2", "User3"),
                            selected = "User1"
                        ),
                        
                        radioButtons("kategoria", 
                                     "Select parameter:",
                                     choiceNames = c("Distance", "Time"),
                                     choiceValues = c("odl", "czas"),
                                     selected = "odl"
                        )
                    ),
                    box(
                        dateRangeInput(
                            inputId = "days",
                            label = "Choose time:",
                            start = daty[1],
                            end = daty[length(daty)],
                            min = daty[1],
                            max = daty[length(daty)],
                            format = "yyyy-mm-dd"
                        )
                        
                    )),
                
                
                fluidRow(
                    box(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput("TypAktywnosci")
                        )    
                    ),
                    box(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput("carbon")
                        )
                    )
                ),
                fluidRow(
                    box(
                        shinycssloaders::withSpinner(
                            plotly::plotlyOutput("liniowy")
                        )    
                    ))
        )
        
    )
)


app_ui <- dashboardPage(
    dashboardHeader(title = "Nasza aplikacja"),
    sidebar,
    body
)

shinyApp(app_ui, server)
