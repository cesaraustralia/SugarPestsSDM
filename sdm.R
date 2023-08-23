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
  "Chilo infuscatellus",
  "Eumetopina flavipes",
  "Yamatotettix flavovittatus",
  "Perkinsiella saccharicida"
)

# countries for modelling
countries <- list(
  "Scirpophaga excerptalis" = c("India", "Pakistan", "Bangladesh", "Nepal", "Bhutan", "Myanmar", "Thailand", "Laos", "China", "Taiwan", "Japan", "Thailand", "Cambodia", "Vietnam", "Malaysia", "Singapore", "Indonesia", "East Timor", "Papua New Guinea", "Solomon Islands", "Micronesia", "New Caledonia"),
  "Sesamia grisescens" = c("Indonesia", "Papua New Guinea"),
  "Chilo infuscatellus" = c("India", "Pakistan", "Afghanistan", "Tajikistan", "Uzbekistan", "Bangladesh", "Nepal", "Bhutan", "Myanmar", "Thailand", "Laos", "China", "Taiwan", "South Korea", "North Korea", "Thailand", "Cambodia", "Vietnam", "Malaysia", "Singapore", "Indonesia", "East Timor", "Papua New Guinea", "Philippines", "Brunei"),
  "Eumetopina flavipes" = c("Malaysia", "Brunei", "Indonesia", "Philippines", "Australia", "Papua New Guinea", "Indonesia", "Solomon Islands", "New Caledonia"),
  "Yamatotettix flavovittatus" = c("China", "Indonesia", "Japan", "South Korea", "North Korea", "Laos", "Malaysia", "Myanmar", "Papua New Guinea", "Taiwan", "Thailand", "Brunei"),
  "Perkinsiella saccharicida" = c("Australia", "Malaysia", "Indonesia", "Brunei", "Philippines", "Papua New Guinea", "India", "Sri Lanka", "Taiwan", "China", "United States", "Mexico", "Costa Rica", "Ecaudor", "Réunion")
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
genlist <- list()
for(i in seq_along(species)){
  
  gbif_data <- geodata::sp_occurrence(genus = get_species(species[i], TRUE),
                                      species = "*")
  
  if(is.null(gbif_data)) next
  
  sp_coords <- gbif_data %>%
    dplyr::select(lon, lat, status = occurrenceStatus,
                  country, species, genus, family) %>%
    drop_na(lon, lat)
  
  if(nrow(sp_coords) < 2) next
  
  sp_points <- st_as_sf(sp_coords, coords = c("lon", "lat"))
  
  splist[[i]] <- sp_points %>% filter(.data$species == .env$species[i])
  genlist[[i]] <- sp_points
}

# check the number of species
unlist(map(splist, nrow))
unlist(map(genlist, nrow))

# combine all species data
sp_all <- splist %>%
  do.call(bind_rows, .) %>%
  st_set_crs(4326)

tgbs <- genlist %>%
  do.call(bind_rows, .) %>%
  st_set_crs(4326)

write_sf(sp_all, "data/sp_all.gpkg")
write_sf(tgbs, "data/tgbs.gpkg")

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
# download bioclim layers
bioclim <- geodata::worldclim_global(var = "bio",
                                     res = 0.5,
                                     path = "data/bioclim.tif")

plot(bioclim[[1]])
plot(sp_all$geometry, add = TRUE)

# leaflet() %>%
#   addTiles() %>%
#   addRasterImage(raster::raster(bioclim[[1]])) %>%
#   addMarkers(data = sp_all)
# 
# r <- bioclim[[1]] %>%
#   terra::aggregate(fact = 5) %>%
#   raster::raster()
# 
# library(mapview)
# 
# mapview(r)


# Crop raster layers ------------------------------------------------------
fls <- list.files("data/bioclim.tif/wc2.1_30s/",
                  pattern = ".tif$",
                  full.names = TRUE)
bioclim <- terra::rast(fls)
plot(bioclim)

for(i in seq_along(species)){
  sf_use_s2(F)
  
  bg_mask <- geodata::world(path = "data/") %>%
    st_as_sf("MULTIPOLYGON") %>%
    st_transform("WGS84") %>%
    filter(NAME_0 %in% countries[[i]]) %>%
    summarise()
  
  sf_use_s2(T)
  
  # mask raster layers one-by-one
  for(k in 1:nlyr(bioclim)){
    masked <- terra::mask(bioclim[[k]], vect(bg_mask))
    terra::writeRaster(masked, paste0("data/bg_layers/", species[i], "/", names(bioclim)[k], ".tif"), overwrite = T)
    print(k)
  }
  
  print(species[i]) 
}


# KDE for background sampling ---------------------------------------------
# loading required libraries
library(spatialEco)
library(terra)
library(disdat)
library(dismo)
library(sf)

for(i in seq_along(species)){
  
  # read TGB data
  tgbs <- st_read("data/tgbs.gpkg") %>% filter(.data$species == .env$species[i])
  # read a raster mask for the region
  rs <- terra::rast(paste0("data/bg_layers/", species[i], "/wc2.1_30s_bio_1.tif"))
  
  if(!i == 6){
    rs <- rs %>%
      crop(geodata::world(path = "data/") %>%
             st_as_sf("MULTIPOLYGON") %>%
             st_transform("WGS84") %>%
             filter(NAME_0 %in% countries[[i]]) %>%
             summarise() %>%
             vect())
    
    tgbs <- tgbs %>%
      filter(!is.na(extract(rs, st_coordinates(tgbs))))
    
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
    # plot(tgb_kde)
  } else {
    rs1 <- rs %>%
      crop(ext(0, 180, -50, 50))
    
    tgbs1 <- tgbs %>%
      filter(!is.na(extract(rs1, st_coordinates(tgbs))))
    
    # remove duplicated points in raster cells
    samplecellID <- terra::cellFromXY(rs1, st_coordinates(tgbs1)) 
    dup <- duplicated(samplecellID)
    tgbsp <- tgbs1[!dup, ]
    
    nrow(tgbs1)
    nrow(tgbsp)
    # st_write(tgbsp, "data/tgbs_reduced.gpkg")
    
    tgb_kde1 <- spatialEco::sp.kde(x = sf::as_Spatial(tgbsp),
                                  bw = 10, # degree
                                  newdata = raster::raster(rs1),
                                  standardize = TRUE,
                                  scale.factor = 10000)
    
    rs2 <- rs %>%
      crop(ext(-180, 0, -50, 50))
    
    tgbs2 <- tgbs %>%
      filter(!is.na(extract(rs2, st_coordinates(tgbs))))
    
    # remove duplicated points in raster cells
    samplecellID <- terra::cellFromXY(rs2, st_coordinates(tgbs2)) 
    dup <- duplicated(samplecellID)
    tgbsp <- tgbs2[!dup, ]
    
    nrow(tgbs2)
    nrow(tgbsp)
    # st_write(tgbsp, "data/tgbs_reduced.gpkg")
    
    tgb_kde2 <- spatialEco::sp.kde(x = sf::as_Spatial(tgbsp),
                                   bw = 10, # degree
                                   newdata = raster::raster(rs2),
                                   standardize = TRUE,
                                   scale.factor = 10000)
    
    tgb_kde <- terra::merge(rast(tgb_kde1), rast(tgb_kde2))
  }
  
  terra::writeRaster(tgb_kde, paste0("data/bias_layers/", species[i], ".tif"), overwrite = T)
}


# create background data with sampling bias --------------------------

bg_df <- data.frame()
tm <- Sys.time()
for(i in seq_along(species)){
  bmask_i <- rast(paste0("data/bias_layers/", species[i], ".tif"))
  
  samples <- dismo::randomPoints(raster(bmask_i), 
                                 n = 1000, 
                                 prob = TRUE)
  
  bg_df <- samples %>% 
    as.data.frame() %>%
    mutate(species = species[i]) %>% 
    bind_rows(bg_df)
  
  print(species[i])
}
Sys.time() - tm

head(bg_df)
nrow(bg_df)

# read species data
sp_all <- st_read("data/sp_all.gpkg")
# combine background data with species data
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
  bind_rows(sp_all)
head(species_data)
nrow(species_data)

st_write(species_data, "data/species_data.gpkg", append = FALSE)


# scale data ------------------------------------------------------------
rst <- rast(lapply(list.files("data/bioclim.tif/wc2.1_30s/",
                              pattern = ".tif$",
                              full.names = TRUE), function(x)
  rast(x) %>% scale())) %>% 
  setNames(c("bio_01", "bio_10", "bio_11",  "bio_12", "bio_13", "bio_14", "bio_15",
             "bio_16", "bio_17", "bio_18", "bio_19", "bio_02", "bio_03", "bio_04", 
             "bio_05", "bio_06", "bio_07", "bio_08", "bio_09"))
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

# pca ---------------------------------------------------------------------
files <- list.files("data/raster_scaled/", full.names = TRUE)
files

rst <- rast(files) %>%
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

write_csv(model_data, "data/model_data.csv")

# modelling ---------------------------------------------------------------------
library(mgcv)
library(caret)
library(biomod2)

model_data <- read_csv("data/model_data.csv") %>%
  mutate(species = as.factor(species))

set.seed(42)
trainIndex <- createDataPartition(model_data$species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
# head(trainIndex)

# calculating the case weights (equal weights)
# the order of weights should be the same as presences and backgrounds in the training data
prNum <- as.numeric(table(model_data[trainIndex,]$occ)["1"]) # number of presences
bgNum <- as.numeric(table(model_data[trainIndex,]$occ)["0"]) # number of backgrounds
iwt <- ifelse(model_data[trainIndex,]$occ == 1, 1, prNum / bgNum)

modelPS <- bam(
  occ ~
    s(PC1, bs = "tp", k = 10, m = 2) +
    s(PC1, species, bs = "fs", m = 1) +
    s(PC2, bs = "tp", k = 10, m = 2) +
    s(PC2, species, bs = "fs", m = 1) +
    s(PC3, bs = "tp", k = 10, m = 2) +
    s(PC3, species, bs = "fs", m = 1) +
    s(PC4, bs = "tp", k = 10, m = 2) +
    s(PC4, species, bs = "fs", m = 1) +
    s(species, bs = "re"),
  data = model_data[trainIndex,],
  method = "fREML",
  weights = iwt,
  family = binomial(link = "cloglog"),
  discrete = TRUE,
  control = gam.control(trace = FALSE), 
  drop.unused.levels = FALSE
)

summary(modelPS)
# gratia::draw(modelPS)
plot(modelPS, pages = 1, rug = FALSE, shade = TRUE)

gam.check(modelPS)

test_df <- model_data[-trainIndex,] %>%
  mutate(pred = predict(modelPS,
                        model_data[-trainIndex,],
                        type = "response"))

plot(pROC::roc(test_df$occ, test_df$pred),
     main = paste0("AUC = ", round(pROC::auc(test_df$occ, test_df$pred), 3)))

thresh_all <- test_df %>%
  group_by(species, occ) %>%
  summarise(thresh = quantile(pred, probs = 0.1, na.rm = T)) %>%
  filter(occ == 1) %>%
  pull(thresh)

names(thresh_all) <- levels(as.factor(model_data$species))

# spatial prediction ------------------------------------------------------
aus_SA2 <- read_sf("data/2021_Census/SA2_2021_AUST_GDA2020.shp")

sf_use_s2(F)
aus <- st_union(aus_SA2)
sf_use_s2(T)

rst <- rast_pca %>%
  crop(aus_SA2 %>% vect()) %>%
  mask(aus_SA2 %>% vect())

plot(rst[[1:4]])

# make species rasters
facts <- list(species = levels(as.factor(model_data$species)))

for(i in facts$species){
  spname <- i
  r <- rst[[1]]
  r[] <- spname
  spr <- mask(r, rst[[1]])
  names(spr) <- "species"
  
  rast_pred <- c(rst, spr)
  
  prediction <- terra::predict(object = rast_pred, 
                               model = modelPS,
                               type = "response"
  )
  
  rast_lab <- str_split(i, " ")[[1]]
  rast_lab[1] <- substr(rast_lab[1], 1, 1)
  rast_lab <- paste(rast_lab, collapse = "_")
  
  newpred <- raster::raster(prediction)
  raster::writeRaster(newpred, paste0("predictions//", rast_lab, ".tif"), overwrite = TRUE)
  
  newpred_b <- newpred
  newpred_b[] <- ifelse(newpred_b[] > thresh_all[i], 1, NA)
  newpred_b <- as.polygons(rast(newpred_b))
  
  writeVector(newpred_b, paste0("predictions_binary//", rast_lab, ".gpkg"), overwrite = TRUE)
}

# mask by host plants ------------------------------------------------------
commodities <- read_sf("data/CLUM_Commodities_2020.shp")

all_comms <- c("barley", "maize", "oats", "rice", "sorghum", "sugar cane", "wheat")

sf_use_s2(FALSE)

for(i in seq_along(all_comms)){
  write_sf(commodities %>%
             filter(Commod_dsc %in% all_comms[i]) %>%
             summarise,
           paste0("host_shp/", all_comms[i], ".gpkg"),
           overwrite = T)
}

sf_use_s2(TRUE)
# 
# ### Chilo infuscatellus ###
# write_sf(commodities %>%
#            filter(Commod_dsc %in% c("barley", "maize", "oats", "rice", "sorghum", "sugar cane")),
#          "host_shp/C_infuscatellus.gpkg", overwrite = T)
# 
# ### Eumetopina flavipes ###
# write_sf(commodities %>%
#            filter(Commod_dsc %in% c("sugar cane")),
#          "host_shp/E_flavipes.gpkg", overwrite = T)
# 
# ### Perkinsiella saccharicida ###
# write_sf(commodities %>%
#            filter(Commod_dsc %in% c("rice", "sorghum", "maize", "sugar cane")),
#          "host_shp/P_saccharicida.gpkg", overwrite = T)
# 
# ### Scirpophaga excerptalis ###
# write_sf(commodities %>%
#            filter(Commod_dsc %in% c("wheat", "sugar cane")),
#          "host_shp/S_excerptalis.gpkg", overwrite = T)
# 
# ### Sesamia grisescens ###
# write_sf(commodities %>%
#            filter(Commod_dsc %in% c("rice", "sugar cane")),
#          "host_shp/S_grisescens.gpkg", overwrite = T)
# 
# ### Yamatotettix flavovittatus ###
# write_sf(commodities %>%
#            filter(Commod_dsc %in% c("sugar cane")),
#          "host_shp/Y_flavovittatus.gpkg", overwrite = T)
# 
