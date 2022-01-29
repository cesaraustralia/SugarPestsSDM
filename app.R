# remotes::install_github("Appsilon/shiny.react")
# remotes::install_github("Appsilon/shiny.fluent")
library(tidyverse)
library(shiny)
# library(shiny.fluent)
library(shinythemes)
library(leaflet)
library(leafsync)
# library(terra)

my_map <- function(x){
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=x[1], lat=x[2], popup="The birthplace of R")
  m
}

y <- c(174.968, 37.852)
x <- c(0.112281, 51.523001)


ui <- shinyUI(
  navbarPage("Sugar Biosecurity",
             selected = "Prediction-Maps", 
             theme = shinytheme("slate"),

             # Panel 1 -----------------------------------------------------------------
             
             tabPanel("Prediction-Maps",

                      sync(my_map(x), my_map(y), no.initial.sync = TRUE)    

                      
             )

             # Panel 2 -----------------------------------------------------------------

             
  )
)


server <- function(input, output){
  
}

shinyApp(ui, server)

