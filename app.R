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
  "C. infuscatellus",
  "E. flavipes",
  "S. excerptalis",
  "S. grisescens",
  "Y. flavovittatus"
)

# host list
host_list <-  c("none", "sugar cane", "barley", "maize", "oats", "rice", "sorghum", "wheat")

## read species data
sp_all <- sf::st_read("data/sp_all.gpkg")
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
             # 
             # 
             # 
             # # Panel 3 -----------------------------------------------------------------
             # tabPanel(
             #   "Seasonal abundance",
             #   
             #   splitLayout(
             #   selectizeInput(inputId = "select_sp", 
             #                  label = "Select species",
             #                  options = list(dropdownParent = 'body',
             #                                 create = 0),
             #                  choices = c("P. saccharicida")),
             #   
             #   # imported function
             #   switchButton(inputId = "showunc",
             #                label = "Show uncertainty",
             #                value = FALSE,
             #                col = "GB",
             #                type = "TF"),
             #   
             #   ),
             #   
             #   plotOutput("ggplt")       
             # ),
             # 
             # 
             # tabPanel(
             #   "Pathways",
             #   HTML("This will be filled.")
             # ),
             
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
  
  
  # seasonal abundance
  output$ggplt <- renderPlot({
    # ggplot(data = posterior_pred, 
    #        aes(x = ym, y = med, group = 1)) +
    #   geom_point() +
    #   geom_path() +
    #   geom_ribbon(aes(ymin = med - 2*sd, ymax = med + 2*sd), alpha = 0.1) +
    #   geom_point(data = stan_data, aes(x =ym, y = num), color = "red") +
    #   theme_bw() +
    #   theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
    #   labs(x = "Time", y = "Total observed Perkinsiella")
    g <- ggplot(data = posterior_pred, aes(x = ymd)) +
      geom_point(aes(x = ymd, y = med, group = 1, color = "Predicted"),
                 size = 2, shape = 16, data = posterior_pred) +
      geom_path(aes(x = ymd, y = med, group = 1), alpha = 0.8, data = posterior_pred) +
      geom_point(aes(x = ymd, y = num, color = "Observed"),
                 shape = 5, size = 3, data = stan_data) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.9)) +
      scale_x_date(date_breaks = "3 months",
                   date_labels = "%b %Y") +
      labs(x = "Date", y = "Total observed Perkinsiella", color = "")
    
    if(input$showunc){
      g <- g +
        geom_ribbon(aes(ymin = ifelse(med - 2 * sd < 0, 0, med - 2 * sd),
                        ymax = med + 2 * sd, 
                        group = 1),
                    alpha = 0.2,
                    data = posterior_pred)
    }
    
    plot(g)
  })
  
  # render HTML page
  # getPage <- function(){
  #   return(includeHTML("modelling_info.html"))
  # }
  output$info <- renderUI({ includeHTML("modelling_info.html") })
  
}

shinyApp(ui, server)
