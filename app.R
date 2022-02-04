# remotes::install_github("Appsilon/shiny.react")
# remotes::install_github("Appsilon/shiny.fluent")
# library(shiny.fluent)
library(tidyverse)
library(shiny)
library(shinythemes)
# library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(leafsync)
library(mapview)
library(raster)
# library(terra)

source("Rsource/SwitchButton.R")

# species list
species_list <- c(
  "P. saccharicida",
  "C. infuscatellus",
  "E. flavipes",
  "S. excerptalis",
  "S. grisescens",
  "Y. flavovittatus"
)

## read species data
sp_all <- sf::st_read("data/occ_data.gpkg")
# set a color palette
sp_palette <- colorFactor(
  palette = viridis::inferno(length(unique(sp_all$species))),
  domain = unique(sp_all$species)
)

# render mapview doesn't work; this function works
myRenderMapview <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) 
    expr = substitute(mapview:::mapview2leaflet(expr))
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env, 
                                 quoted = TRUE)
}
n <- 0


ui <- shinyUI(
  navbarPage("Sugar Biosecurity",
             selected = "Prediction maps", 
             theme = "button.css",

             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Prediction maps",

               splitLayout(

                 selectizeInput(inputId = "select_map1", 
                                label = "Select species map",
                                options = list(dropdownParent = 'body',
                                               create = 0),
                                choices = species_list),
                 
                 # imported function
                 switchButton(inputId = "split",
                              label = "Split view",
                              value = FALSE,
                              col = "GB",
                              type = "TF"),
               
                 
                 uiOutput("select2")
                 
               ),
               
               # map prediction maps
               uiOutput("maps") %>%
                 withSpinner(color = "#428bca")# "#0dc5c1"
                      
             ),

             # Panel 2 -----------------------------------------------------------------
             tabPanel(
               "Occurrence maps",
               
               leafletOutput("map", height = 600)
               
             ),
             


             # Panel 3 -----------------------------------------------------------------
             tabPanel("Seasonal abundance",
              HTML("This page will be added latter!")        
             ),
             
             # Panel 4 -----------------------------------------------------------------
             tabPanel("Info",
                      uiOutput("info")
               
             )
             
  )
)


server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        data = sp_all,
        radius = 6,
        stroke = FALSE,
        label = ~species,
        color = ~sp_palette(species),
        fillOpacity = 0.4
      ) %>% 
      addLegend(position = "bottomleft", 
                pal = sp_palette,
                values = sp_all$species,
                title = "Species",
                opacity = 0.8
      )
      
  })
  
  output$select2 <- renderUI({
    if(input$split){
      selectizeInput(inputId = "select_map2", 
                     label = "Select species map",
                     options = list(dropdownParent = 'body',
                                    create = 0),
                     choices = species_list)
    }
  })
  
  map1 <- reactive({
    if(!is.null(input$select_map1)){
      occurrence <- raster::raster(paste0("predictions/", 
                                 gsub(". ", "_", input$select_map1), 
                                 ".tif"))
      mapview(occurrence,
              col.regions = terrain.colors(10, rev = TRUE),
              na.color = NA, height = 600
      )
    }
  })
  
  map2 <- reactive({
    if(!is.null(input$select_map2)){
      occurrence <- raster::raster(paste0("predictions/", 
                                          gsub(". ", "_", input$select_map2), 
                                          ".tif"))
      mapview(occurrence,
              col.regions = terrain.colors(10, rev = TRUE),
              na.color = NA, height = 600
      )
    }
  })
  

  # the maps
  output$maps <- renderUI({
    
    if(input$split){
      
      n <- n + 1
      if(n > 1) browser()
      
      
      # req(map2())
      
      leafsync::sync(map1(), map2(), no.initial.sync = TRUE)
      
    } else{
      myRenderMapview(map1())
    }
    
  })
  
  
    
  # render HTML page
  # getPage <- function(){
  #   return(includeHTML("modelling_info.html"))
  # }
  output$info <- renderUI({ includeMarkdown("modelling_info.md") })
  
}

shinyApp(ui, server)
