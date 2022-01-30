library(rgbif)
library(leaflet)
library(tidyverse)
library(sf)
library(terra)

# get species data --------------------------------------------------------
# my species for modelling
species <- c(
  "Scirpophaga excerptalis",
  "Sesamia grisescens",
  "Chilo auricilius",
  "Chilo infuscatellus",
  "Eumetopina flavipes",
  "Matsumuratettix hiroglyphicus",
  "Yamatotettix flavovittatus",
  "Perkinsiella saccharicida",
  "Perkinsiella thompsoni"
)

get_species <- function(x, genus = FALSE){
  out <- x %>% 
    str_split(" ") %>% 
    unlist()
  if(genus){
    return(out[1])
  } else{
    out[2]
  }
}


splist <- list()
for(i in seq_along(species)){
  
  gbif_data <- geodata::sp_occurrence(genus = get_species(species[i], TRUE), 
                                      species = get_species(species[i], FALSE))
  
  # take a look at the downloaded data:
  # gbif_data %>% View()
  
  if(is.null(gbif_data)) next
  
  sp_coords <- gbif_data %>% 
    dplyr::select(lon, lat, status = occurrenceStatus, 
                  country, species, genus, family) %>% 
    drop_na(lon, lat)
  
  if(nrow(sp_coords) < 2) next
  
  sp_points <- st_as_sf(sp_coords, coords = c("lon", "lat"))
  # sp_points
  
  splist[[i]] <- sp_points
}

# check the number of species
unlist(map(splist, nrow))

# combine all species data
sp_all <- splist %>% 
  do.call(bind_rows, .) %>% 
  st_set_crs(4326)

head(sp_all)
nrow(sp_all)


species_palette <- colorFactor(palette = viridis::inferno(length(unique(sp_all$species))),
                               domain = unique(sp_all$species))
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




# raster data -------------------------------------------------------------
# download bias layer
bias_layer <- geodata::travel_time(path = "data/travel_layer.tif",
                                   size = 1,
                                   up = TRUE)
plot(bias_layer)

# download bioclim layers
bioclim <- geodata::worldclim_global(var = "bio", 
                                     res = 0.5, 
                                     path = "data/bioclim.tif")
plot(bioclim)


plot(bioclim[[1]])
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







# # download GBIF occurrence data for this species; this takes time if there are many data points!
# gbif_data <- occ_data(scientificName = species[2], 
#                       hasCoordinate = TRUE, 
#                       limit = 20000)
# # get the columns that matter for mapping and cleaning the occurrence data:
# sp_coords <- gbif_data$data %>% 
#   dplyr::select(decimalLongitude, decimalLatitude, individualCount,
#                 occurrenceStatus, coordinateUncertaintyInMeters, institutionCode,
#                 references) %>% 
#   mutate(long = decimalLongitude,
#          lat = decimalLatitude)


