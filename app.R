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
library(shinyWidgets)

# # example raster
r <- raster::raster("data/toyraster.tif")
r[r > 50000] <- NA


## read species data
sp_all <- sf::st_read("data/species_data.gpkg")
# set a color palette
species_palette <- colorFactor(palette = viridis::inferno(length(unique(sp_all$species))),
                               domain = unique(sp_all$species))

ui <- shinyUI(
  navbarPage("Sugar Biosecurity",
             selected = "Prediction maps", 
             theme = shinytheme("journal"),

             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Prediction maps",

               splitLayout(

                 selectizeInput(inputId = "select_map1", 
                                label = "Select prediction map",
                                options = list(dropdownParent = 'body',
                                               create = 0),
                                choices = c("P. saccharicida")),
                 
                 switchInput(inputId = "split", 
                             label = "Split view", 
                             value = FALSE),
                 
                 uiOutput("select2")
                 # checkboxInput(inputId = "split", 
                 #               label = "Split view", 
                 #               value = FALSE, 
                 #               width = NULL)
               ),
               
               # map prediction map
               uiOutput("maps")
                      
             ),

             # Panel 2 -----------------------------------------------------------------
             tabPanel(
               "Species maps",
               
               leafletOutput("map", height = 600)
               
             ),
             

             # Panel 3 -----------------------------------------------------------------
             tabPanel("Info",
               includeHTML("modelling_info.html")
             )
             
  )
)


server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      # addMarkers(data = sp_points)
      addCircleMarkers(
        data = sp_all,
        radius = 6,
        stroke = FALSE,
        label = ~species,
        color = ~species_palette(species),
        fillOpacity = 0.4
      )
      
  })
  
  output$select2 <- renderUI({
    if(input$split){
      selectizeInput(inputId = "select_map2", 
                     label = "Select prediction map",
                     options = list(dropdownParent = 'body',
                                    create = 0),
                     choices = c("P. saccharicida"))
    }
  })
  
  output$maps <- renderUI({
    
    if(input$split){
      leafsync::sync(mapview(r), mapview(r), no.initial.sync = TRUE)
      
    } else{
      renderLeaflet(mapview::mapview(r)$map)
    }
    
  })
  
}

shinyApp(ui, server)

