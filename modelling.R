library(rgbif)
library(leaflet)
library(tidyverse)
library(sf)
library(terra)


# get species data --------------------------------------------------------
# my species for modelling
species <- c("Perkinsiella saccharicida", "Scirpophaga excerptalis")
species[1]

# download GBIF occurrence data for this species; this takes time if there are many data points!
gbif_data <- occ_data(scientificName = species[2], 
                      hasCoordinate = TRUE, 
                      limit = 2000)

# take a look at the downloaded data:
gbif_data


# get the columns that matter for mapping and cleaning the occurrence data:
sp_coords <- gbif_data$data %>% 
  dplyr::select(decimalLongitude, decimalLatitude, individualCount,
                occurrenceStatus, coordinateUncertaintyInMeters, institutionCode,
                references) %>% 
  mutate(long = decimalLongitude,
         lat = decimalLatitude)

myspecies_coords



sp_points <- st_as_sf(sp_coords, coords = c("long", "lat"))
sp_points


leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = sp_points)


# raster data -------------------------------------------------------------
# bias layer
bias_layer <- geodata::travel_time(path = "data/travel_layer.tif")
plot(bias_layer)

# bioclim layers
clim <- geodata::worldclim_global(var = "tmin", 
                                  res = 0.5, 
                                  path = "data/bioclim.tif")
plot(clim)


plot(clim[[1]])
plot(sp_points$geometry, add = TRUE)

leaflet() %>% 
  addTiles() %>% 
  addRasterImage(raster::raster(clim[[1]])) %>% 
  addMarkers(data = sp_points)

r <- clim[[1]] %>% 
  terra::aggregate(fact = 5) %>%
  raster::raster()

library(mapview)

mapview(r)









