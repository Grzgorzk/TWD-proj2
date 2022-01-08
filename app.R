library(shiny)
library(dplyr)
library(sp)
library(maptools)
library(leaflet)
library(shinyTime)



server <- function(input, output, session) {
    
    
}

ui <- fluidPage(
    titlePanel("Page1")
)


ui2 <- fluidPage(
    titlePanel("Page2")
)

ui3 <- fluidPage(
    titlePanel("Page3")
)

app_ui <- navbarPage(
    title = "Projekt TWD 2",
    tabPanel("Page1", ui),
    tabPanel("Page2", ui2),
    tabPanel("Page3", ui3),
    theme = bslib::bs_theme(bootswatch = "cosmo")
)

shinyApp(app_ui, server)
