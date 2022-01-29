# remotes::install_github("Appsilon/shiny.react")
# remotes::install_github("Appsilon/shiny.fluent")
library(tidyverse)
library(shiny)
# library(shiny.fluent)
library(shinythemes)
library(leaflet)
library(leafsync)
# library(terra)
library(mapview)

# # example raster
# r <- terra::rast("data/travel_layer.tif/travel_time_to_cities_1.tif") %>%
#   terra::crop(d) %>%
#   terra::aggregate(facet = 5) %>%
#   raster::raster()
# plot(r)
r <- raster::raster("data/toyraster.tif")
r[r > 50000] <- NA

# my_map <- function(x){
#   m <- leaflet() %>%
#     addTiles() %>%  # Add default OpenStreetMap map tiles
#     addRasterImage(x, opacity = 0.8)
#     # addMarkers(lng=x[1], lat=x[2], popup="The birthplace of R")
#   m
# }

y <- c(174.968, 37.852)
x <- c(0.112281, 51.523001)


ui <- shinyUI(
  navbarPage("Sugar Biosecurity",
             selected = "Prediction maps", 
             theme = shinytheme("journal"),

             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Prediction maps",

               selectInput(inputId = "select_map", 
                           label = "Select prediction map", 
                           choices = c("P. saccharicida")),
               
               leafletOutput("map")
                      
             ),

             # Panel 2 -----------------------------------------------------------------
             tabPanel(
               "Compare maps",
               
               selectInput(inputId = "select_map", 
                           label = "Select left map", 
                           choices = c("P. saccharicida")),
               
               uiOutput(outputId = "maps")
               
             ),
             

             # Panel 3 -----------------------------------------------------------------
             tabPanel("Info",
               HTML("Some information about modelling and species data comes here!")
             )
             
  )
)


server <- function(input, output){
  
  output$map <- renderLeaflet({
   leaflet() %>% 
      addTiles()
  })
  
  output$maps <- renderUI({
    leafsync::sync(mapview(r), mapview(r), no.initial.sync = TRUE)
    
  })
  
}

shinyApp(ui, server)

