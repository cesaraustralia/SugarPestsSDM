# library(tidyverse)
# library(leaflet)
# library(geodata)
# library(terra)
# library(sf)
# 
# # test a species
# gbif_data <- geodata::sp_occurrence(genus = "Perkinsiella", 
#                                     species = "*")
# 
# sp_coords <- gbif_data %>% 
#   dplyr::select(lon, lat, status = occurrenceStatus, 
#                 country, species, genus, family) %>% 
#   drop_na(lon, lat)
# 
# sp_points <- st_as_sf(sp_coords, coords = c("lon", "lat"))
# bg_mask <- st_read("data/background_mask.gpkg")
# 
# leaflet(bg_mask) %>% 
#   addTiles() %>% 
#   addPolygons(opacity = 0.4, color = NA) %>% 
#   addCircleMarkers(
#     data = sp_points,
#     radius = 4,
#     stroke = FALSE,
#     color = "red",
#     label = ~species,
#     fillOpacity = 0.4
#   )
# 
# 
# # TGB points --------------------------------------------------------------
# species <- c(
#   "Scirpophaga excerptalis",
#   "Sesamia grisescens",
#   "Chilo auricilius",
#   "Chilo infuscatellus",
#   "Eumetopina flavipes",
#   "Matsumuratettix hiroglyphicus",
#   "Yamatotettix flavovittatus",
#   "Perkinsiella saccharicida",
#   "Perkinsiella thompsoni"
# )
# 
# bglist <- list()
# for(i in seq_along(species)){
#   
#   # download the whole genus data
#   gbif_data <- geodata::sp_occurrence(genus = get_species(species[i], TRUE), 
#                                       species = "*")
#   
#   # take a look at the downloaded data:
#   # gbif_data %>% View()
#   
#   if(is.null(gbif_data)) next
#   
#   gns_coords <- gbif_data %>% 
#     dplyr::select(lon, lat, status = occurrenceStatus, 
#                   country, species, genus, family) %>% 
#     drop_na(lon, lat)
#   
#   if(nrow(gns_coords) < 2) next
#   
#   gns_points <- st_as_sf(gns_coords, coords = c("lon", "lat"))
#   # sp_points
#   
#   bglist[[i]] <- gns_points
# }
# 
# # check the number of species
# unlist(map(bglist, nrow))
# 
# # combine all species data
# bg_all <- bglist %>% 
#   do.call(bind_rows, .) %>% 
#   st_set_crs(4326)
# 
# st_write(bg_all, "data/admin/bgs.gpkg")
# 
# bg_mask <- st_read("data/background_mask.gpkg")
# 
# ## mask to background countries
# # R couldn't mask the layer! QGIS did the job!
# tgb <- st_read("data/tgbs.gpkg")
# head(tgb)
# nrow(tbg)
# plot(st_geometry(tgb))
# 
# leaflet(bg_mask) %>% 
#   addTiles() %>% 
#   addPolygons(opacity = 0.4, color = NA) %>% 
#   addCircleMarkers(
#     data = tgb,
#     radius = 4,
#     stroke = FALSE,
#     color = "red",
#     label = ~species,
#     fillOpacity = 0.4
#   )
# 
# 
# # admin layers ------------------------------------------------------------
# # download admin layes
# admin <- geodata::world(resolution = 1, level = 0, path = "data/admin")
# plot(admin)
# terra::writeVector(admin, filename = "data/admin/world")
# 
# leaflet(sf::st_as_sf(admin)) %>% 
#   addTiles() %>% 
#   addPolygons()
# 
# 
# usa <- geodata::gadm(country = "USA", level = 1, path = "data/admin")
# plot(usa)
# terra::writeVector(usa, filename = "data/admin/usa")
# 
# chn <- geodata::gadm(country = "CHN", level = 1, path = "data/admin")
# plot(chn)
# terra::writeVector(chn, filename = "data/admin/chn")
# 
# 
