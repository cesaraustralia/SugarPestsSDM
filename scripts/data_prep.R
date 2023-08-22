# library(rgbif)
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

gbif_data <- geodata::sp_occurrence(genus = "Perkinsiella",
                                    species = "*")

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

plot(bioclim[[1]])
plot(sp_all$geometry, add = TRUE)

leaflet() %>%
  addTiles() %>%
  addRasterImage(raster::raster(bioclim[[1]])) %>%
  addMarkers(data = sp_all)

r <- bioclim[[1]] %>%
  terra::aggregate(fact = 5) %>%
  raster::raster()

library(mapview)

mapview(r)


# Crop raster layers ------------------------------------------------------
fls <- list.files("data/bioclim.tif/wc2.1_30s/",
                  pattern = ".tif$",
                  full.names = TRUE)
bioclim <- terra::rast(fls)
plot(bioclim)

bg_mask <- st_read("data/background_mask.gpkg")
plot(st_geometry(bg_mask))

# mask raster layers one-by-one
for(i in 1:nlyr(bioclim)){
  masked <- terra::mask(bioclim[[i]], vect(bg_mask))
  terra::writeRaster(masked, paste0("data/bg_layers/", names(bioclim)[i], ".tif"), overwrite = T)
  print(i)
}


# KDE for background sampling ---------------------------------------------
# loading required libraries
library(spatialEco)
library(terra)
library(disdat)
library(dismo)
library(sf)

# read TGB data
tgbs <- st_read("data/tgbs.gpkg")
# read a raster mask for the region
rs <- terra::rast("data/bg_layers/wc2.1_30s_bio_1.tif")

plot(rs)
plot(st_geometry(tgbs), add = TRUE, pch = 16, cex = 0.3)

# remove duplicated points in raster cells
samplecellID <- terra::cellFromXY(rs, st_coordinates(tgbs)) 
dup <- duplicated(samplecellID)
tgbsp <- tgbs[!dup, ]

nrow(tgbs)
nrow(tgbsp)
# st_write(tgbsp, "data/tgbs_reduced.gpkg")

tgb_kde <- spatialEco::sp.kde(x = sf::as_Spatial(tgbsp),
                              bw = 10, # degree
                              newdata = raster::raster(rs),
                              standardize = TRUE,
                              scale.factor = 10000)
plot(tgb_kde)


# create background data with accessibility bias --------------------------
library(dismo)
library(sf)

# read species data
sp_all <- st_read("data/occ_data.gpkg")
# accessibility map
access <- terra::rast("data/accessibility_masked.tif")
plot(access)

# standardize the map
setMinMax(access)
rmin <- terra::minmax(access)[1]
rmax <- terra::minmax(access)[2]
rmin; rmax;
access_std <- (rmax - access) / (rmax - rmin)
plot(access_std)

# reading the distance map
distr <- rast("data/occ_distance.tif")
# distr <- distr^2 # increase the effect of distance
plot(distr)
rmin <- terra::minmax(distr)[1]
rmax <- terra::minmax(distr)[2]
rmin; rmax;
dist_std <- (rmax - distr) / (rmax - rmin)
plot(dist_std)

dist_std <- terra::crop(dist_std, access_std)
access_std <- terra::crop(access_std, dist_std)
bias_layer <- (dist_std ^ 4) * access_std
# terra::writeRaster(bias_layer, "data/bias_layer.tif")


## system can't handle this large raster
## sample the raster to reduce time
tm <- Sys.time()
bmask <- raster::sampleRegular(raster::raster(bias_layer), 
                               size = 1e7, 
                               asRaster = TRUE)
Sys.time() - tm
plot(bmask)

# rmin <- raster::minValue(bmask)
# rmax <- raster::maxValue(bmask)
# rmin; rmax;
# bg_mask_std <- (bmask - rmin) / (rmax - rmin)
# plot(bg_mask_std)
# 
# raster::minValue(bg_mask_std)
# raster::maxValue(bg_mask_std)

# bgmask <- terra::spatSample(access_std, 
#                             size = 1000000, 
#                             as.raster = fals,
#                             method = "random") %>% 
#   raster::raster()
# create sparate bg records for each species
countries <- list("Scirpophaga excerptalis" = c("India", "Pakistan", "Bangladesh", "Nepal", "Bhutan", "Myanmar", "Thailand", "Laos", "China", "Taiwan", "Japan", "Thailand", "Cambodia", "Vietnam", "Malaysia", "Singapore", "Indonesia", "East Timor", "Papua New Guinea", "Solomon Islands", "Micronesia", "New Caledonia"),
                  "Sesamia grisescens" = c("Indonesia", "Papua New Guinea"),
                  "Chilo infuscatellus" = c("India", "Pakistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Bangladesh", "Nepal", "Bhutan", "Myanmar", "Thailand", "Laos", "China", "Taiwan", "South Korea", "North Korea", "Thailand", "Cambodia", "Vietnam", "Malaysia", "Singapore", "Indonesia", "East Timor", "Papua New Guinea", "Philippines", "Brunei"),
                  "Eumetopina flavipes" = c("Malaysia", "Brunei", "Indonesia", "Philippines", "Australia", "Papua New Guinea", "Indonesia", "Solomon Islands", "New Caledonia"),
                  "Yamatotettix flavovittatus" = c("China", "Indonesia", "Japan", "South Korea", "North Korea", "Loas", "Malaysia", "Myanmar", "Papua New Guinea", "Taiwan", "Thailand", "Brunei"),
                  "Perkinsiella saccharicida" = c("Australia", "Malaysia", "Indonesia", "Brunei", "Philippines", "Papua New Guinea", "India", "Sri Lanka", "Taiwan", "China", "United States", "Mexico", "Costa Rica", "Ecaudor"))


bg_df <- data.frame()
tm <- Sys.time()
for(i in seq_along(unique(sp_all$species))){
  
  bmask_i <- mask(rast(bmask),
                  geodata::world(path = "data/") %>%
                    st_as_sf("MULTIPOLYGON") %>%
                    st_transform("WGS84") %>%
                    filter(NAME_0 %in% countries[[i]]) %>%
                    vect())
  
  samples <- dismo::randomPoints(raster(bmask_i), 
                                 n = 1000, 
                                 prob = TRUE)
  
  bg_df <- samples %>% 
    as.data.frame() %>%
    mutate(species = unique(sp_all$species)[i]) %>% 
    bind_rows(bg_df)
  
  print(unique(sp_all$species)[i])
}
Sys.time() - tm

head(bg_df)
nrow(bg_df)

# combine background data with species data
sp_all <- sf::st_read("data/occ_data.gpkg")
sp_all <- sp_all %>% 
  mutate(occ = 1,
         wt = 1) %>% 
  dplyr::select(occ, species, wt)

# function to rename geometry column in sf
rename_geometry <- function(g, name){
  current = attr(g, "sf_column")
  names(g)[names(g)==current] = name
  st_geometry(g)=name
  g
}

species_data <- bg_df %>% 
  mutate(occ = 0, 
         wt = 100000) %>% 
  st_as_sf(coords = c("x", "y")) %>%
  rename_geometry(name = "geom") %>% 
  bind_rows(sp_all, .)
head(species_data)
nrow(species_data)

st_write(species_data, "data/species_data.gpkg", append = T)


# extract data ------------------------------------------------------------
# list.files("data/bg_layers/", full.names = T) %>% 
#   rast() %>% 
#   plot()
# 
# covar <- c("wc2.1_30s_bio_1.tif", 
#            "wc2.1_30s_bio_3.tif", 
#            "wc2.1_30s_bio_5.tif", 
#            "wc2.1_30s_bio_6.tif", 
#            "wc2.1_30s_bio_12.tif",
#            "wc2.1_30s_bio_14.tif",
#            "wc2.1_30s_bio_15.tif")

rst <- rast(lapply(list.files("data/bg_layers/", full.names = T), function(x)
  rast(x) %>% scale())) %>% 
  setNames(c("bio_01", "bio_10", "bio_11",  "bio_12", "bio_13", "bio_14", "bio_15",
             "bio_16", "bio_17", "bio_18", "bio_19", "bio_02", "bio_03", "bio_04", 
             "bio_05", "bio_06", "bio_07", "bio_08", "bio_09"))
# rst[["bio_12"]] <- log(rst[["bio_12"]] + 1) # add 1 to avoid -Inf in log
# rst[["bio_14"]] <- log(rst[["bio_14"]] + 1) # add 1 to avoid -Inf in log
plot(rst)

# read EVI layer
evi <- rast("data/evi/evi_virt.vrt") %>% 
  terra::resample(rst[[1]]) %>% 
  terra::scale() %>% 
  setNames("evi")

rst <- c(rst, evi)
plot(rst)

for(i in 1:nlyr(rst)){
  terra::writeRaster(
    rst[[i]], 
    paste0("data/raster_scaled/", names(rst)[i], ".tif"),
    overwrite = T
  )
  print(names(rst)[i])
}




extr <- c(
  xmin = 60,
  xmax = 180,
  ymin = -45,
  ymax = 54
)

species_data <- st_read("data/admin/species_data.gpkg") %>% 
  st_crop(extr) ##*** crop east ***##
rst <- rast(list.files("data/raster_scaled/", full.names = TRUE))

# create the training date for modelling
model_data <-  terra::extract(rst, vect(species_data)) %>% 
  mutate(occ = species_data$occ,
         species = as.factor(species_data$species),
         wt = species_data$wt) %>% 
  dplyr::select(-ID) %>%
  drop_na()

head(model_data)
table(model_data$occ)
table(model_data$species)

nrow(model_data)
nrow(species_data)


# pca ---------------------------------------------------------------------
# covar <- c("wc2.1_30s_bio_4.tif", "wc2.1_30s_bio_5.tif",
#            "wc2.1_30s_bio_6.tif", "wc2.1_30s_bio_12.tif",
#            "wc2.1_30s_bio_15.tif")
# files <- paste0("data/bg_layers/", covar)

extr <- c(
  xmin = 60,
  xmax = 180,
  ymin = -45,
  ymax = 54
)

files <- list.files("data/raster_scaled/", full.names = TRUE)
files

rst <- rast(files) %>% 
  # terra::crop(extr) %>% 
  terra::aggregate(fact = 5)

# principal components of a SpatRaster
set.seed(4326)
pca <- values(spatSample(rst, 100000, as.raster=TRUE)) %>% 
  na.omit() %>% 
  as.data.frame() %>%
  prcomp()
plot(pca)

rast_pca <- predict(rst, pca)
plot(rast_pca[[1:4]])

# read species occurrence and background samples
species_data <- st_read("data/species_data.gpkg")

# create the training date for modelling
model_data <- terra::extract(rast_pca, vect(species_data)) %>% 
  mutate(occ = species_data$occ,
         species = as.factor(species_data$species),
         wt = species_data$wt) %>% 
  drop_na()

head(model_data)
nrow(model_data)
table(model_data$occ)
table(model_data$species)









