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

# load the switch code
source("Rsource/SwitchButton.R")

# this will be deleted later
# posterior_pred <- read.csv("data/posterior_pred.csv") %>% 
#   mutate(ymd = as.Date(ymd))
# stan_data <- read.csv("data/stan_data.csv") %>% 
#   mutate(ymd = as.Date(ymd))

# species list
species_list <- c(
  "P. saccharicida",
  "S. excerptalis",
  "C. auricilia",
  "C. infuscatellus",
  "E. flavipes",
  "S. grisescens",
  "Y. flavovittatus"
)

# countries
countries <- terra::vect("data/countries.gpkg")

# host list
host_list <-  c("none", "sugar cane", "barley", "maize", "oats", "rice", "sorghum", "wheat")

## read species data
df_files <- dir("database", full.names = T)
timestamps <- file.info(df_files)$ctime

latest_df <- which.max(timestamps)

sp_all <- read_csv(df_files[latest_df])

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


ui <- shinyUI(
  navbarPage("Sugar Biosecurity",
             selected = "Prediction maps",
             theme = shinytheme("flatly"),
             
             # Panel 1 -----------------------------------------------------------------
             tabPanel(
               "Prediction maps",
               includeCSS("www/button.css"),
               
               splitLayout(
                 
                 verticalLayout(
                   
                   selectizeInput(inputId = "select_map1", 
                                  label = "Select species map",
                                  options = list(dropdownParent = 'body',
                                                 create = 0),
                                  choices = species_list),
                   
                   selectizeInput(inputId = "select_host1", 
                                  label = "Select host plant",
                                  options = list(dropdownParent = 'body',
                                                 create = 0),
                                  choices = host_list)
                 ),
                 
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
                 withSpinner(color = "#2C3E50", type = 6)# "#0dc5c1"
               
             ),
             
             # Panel 2 -----------------------------------------------------------------
             tabPanel(
               "Occurrence maps",
               
               leafletOutput("map", height = 600)
               
             ),
             
             # Panel 3 -----------------------------------------------------------------
             tabPanel(
               "Upload data",
               
               fluidPage(
                 shinyjs::useShinyjs(),
                 fluidRow(
                   actionButton("add_button", "Add", icon("plus")),
                   downloadButton("download_button", "Download", icon("download"))
                 ),
                 br(),
                 DT::dataTableOutput("outtbl")
               )
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
        data = sp_all %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84"),
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
      verticalLayout(
        selectizeInput(inputId = "select_map2", 
                       label = "Select species map",
                       options = list(dropdownParent = 'body',
                                      create = 0),
                       choices = species_list),
        selectizeInput(inputId = "select_host2", 
                       label = "Select host plant",
                       options = list(dropdownParent = 'body',
                                      create = 0),
                       choices = host_list)
      )
    }
  })
  
  map1 <- reactive({
    if(!is.null(input$select_map1)){
      occurrence <- setNames(raster::raster(paste0("predictions/", 
                                                   gsub(". ", "_", input$select_map1), 
                                                   ".tif")),
                             "Habitat Suitability")
      
      map_p <- mapview(occurrence,
                       col.regions = RColorBrewer::brewer.pal(9, "YlOrBr"),
                       na.color = NA, height = 600, at = seq(0,1, 0.1),
                       layer.name = paste0("Habitat Suitability (", input$select_map1, sep = ")"),
                       query.digits = 2
      )
      
      if(!input$select_host1 == "none"){
        host1 <- sf::st_read(paste0("host_shp/", 
                                    input$select_host1, 
                                    ".gpkg")) %>%
          dplyr::mutate(host1 = input$select_host1)
        
        map_p <- map_p +
          mapview(host1, legend = FALSE, alpha.regions = .4, color = "darkgrey", label = host1,
                  layer.name = input$select_host1)
      }
      
      map_p
    }
  })
  
  map2 <- reactive({
    if(!is.null(input$select_map2)){
      occurrence <- setNames(raster::raster(paste0("predictions/", 
                                                   gsub(". ", "_", input$select_map2), 
                                                   ".tif")),
                             "Habitat Suitability")
      
      map_p <- mapview(occurrence,
                       col.regions = RColorBrewer::brewer.pal(9, "YlOrBr"),
                       na.color = NA, height = 600, at = seq(0,1, 0.1),
                       layer.name = paste0("Habitat Suitability (", input$select_map2, sep = ")"),
                       query.digits = 2
      )
      
      if(!input$select_host2 == "none"){
        host2 <- sf::st_read(paste0("host_shp/", 
                                    input$select_host2, 
                                    ".gpkg")) %>%
          dplyr::mutate(host2 = input$select_host2)
        
        map_p <- map_p +
          mapview(host2, legend = FALSE, alpha.regions = .4, color = "darkgrey", label = host2,
                  layer.name = input$select_host2)
      }
      
      map_p
    }
  })
  
  
  # the maps
  output$maps <- renderUI({
    
    if(input$split){
      
      req(map2())
      
      leafsync::sync(map1(), map2(), no.initial.sync = TRUE)
      
    } else{
      myRenderMapview(map1())
    }
    
  })
  
  # Render the table view
  output$outtbl <- DT::renderDataTable({
      
      DT::datatable(
        sp_all,
        filter = "top",
        options = list(scrollX = T)
        # ,rownames = F
      )
    })
  
  ## Add new row ##
  observeEvent(input$add_button, priority = 20, {
    showModal(modalDialog(div(
      id = ("entry_form"),
      tags$head(tags$style(".modal-dialog{ width:1600px}")),
      #Modify the width of the dialog
      tags$head(tags$style(
        HTML(".shiny-split-layout > div {overflow: visible}")
      )),
      #Necessary to show the input options
      fluidPage(
        splitLayout(textInput(inputId = "col_species", label = "Species", value = ''),
                    numericInput(inputId = "col_longitude", label = "Longitude", value = 0),
                    numericInput(inputId = "col_latitude", label = "Latitude", value = 0),
                    textInput(inputId = "col_source", label = "Your name", value = '')),
        actionButton(
          inputId = "submit_add",
          label = "Submit to database",
          icon = icon("database")
        ),
        easyClose = TRUE
      )
    )))
  })
  
  # Submit new data to table
  observeEvent(input$submit_add, priority = 20, {
    
    new_row <-
      tibble(species = input$col_species,
             longitude = input$col_longitude,
             latitude = input$col_latitude,
             source = input$col_source)
    
    new_row$country = terra::extract(countries, new_row[,c(2,3)])[,2]
    
    # update table
    sp_all <-
      bind_rows(sp_all,
                new_row)
    
    # save table
    new_df_name <- paste0("database/sugarcane_pests_database_", lubridate::today(), ".csv")
    
    
    n = 0
    while(new_df_name %in% df_files){
      n = n + 1
      new_df_name <- paste0("database/sugarcane_pests_database_", lubridate::today(), "_", n, ".csv")
    }
    
    write_csv(sp_all,
              new_df_name)
    
    showNotification(
      sprintf("Entry successfully added to table"),
      duration = 3,
      closeButton = FALSE,
      type = "message"
    )
    
    ## update outputs
    # set a color palette
    sp_palette <- colorFactor(
      palette = viridis::inferno(length(unique(sp_all$species))),
      domain = unique(sp_all$species)
    )
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addCircleMarkers(
          data = sp_all %>% sf::st_as_sf(coords = c("longitude", "latitude"), crs = "WGS84"),
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
    
    output$outtbl <- 
      DT::renderDataTable({
        
        DT::datatable(
          sp_all,
          filter = "top",
          options = list(scrollX = T)
          # ,rownames = F
        )
      })
    
  })
  
  ## Download timestamped csv of selected data set ##
  output$download_button <- downloadHandler(
    filename <- function() {
      paste0("sugarcane_pests_database_", lubridate::today(), ".csv")
    },
    content <- function(file) {
      write.csv(sp_all[input$outtbl_rows_all,], file, row.names = FALSE)
    }
  )
  
  # render HTML page
  # getPage <- function(){
  #   return(includeHTML("modelling_info.html"))
  # }
  output$info <- renderUI({ includeHTML("modelling_info.html") })
  
}

shinyApp(ui, server)
