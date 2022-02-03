library(tidyverse)
library(mgcv)
library(terra)

# modelling with covariates -----------------------------------------------
spname <- "Perkinsiella saccharicida"

modelA <- bam(
  occ ~ s(bio_04, bs  = "tp", k = 10) +
    s(bio_05, bs  = "tp", k = 10) +
    s(bio_06, bs  = "tp", k = 10) +
    s(bio_12, bs  = "tp", k = 10) +
    s(bio_15, bs  = "tp", k = 10),
  data = model_data[model_data$species == spname, ],
  method = "fREML",
  family = binomial(link = "cloglog"), 
  weights = model_data$wt[model_data$species == spname],
  discrete = TRUE,
  control = gam.control(trace = FALSE), 
  drop.unused.levels = FALSE
)

summary(modelA)
gratia::draw(modelA)

gam.check(modelA)



modelP <- bam(
  occ ~ s(species, bs = "re") +
    s(bio_04, bs  = "tp", k = 10) +
    s(bio_05, bs  = "tp", k = 10) +
    s(bio_06, bs  = "tp", k = 10) +
    s(bio_12, bs  = "tp", k = 10) +
    s(bio_15, bs  = "tp", k = 10),
  data = model_data,
  method = "fREML",
  family = binomial(link = "cloglog"), 
  weights = model_data$wt,
  discrete = TRUE,
  control = gam.control(trace = FALSE), 
  drop.unused.levels = FALSE
)

summary(modelP)
gratia::draw(modelP)

gam.check(modelP)



modelPS <- bam(
  occ ~ s(bio_03, bs = "tp", k = 10, m = 2) +
    s(bio_03, species, bs = "fs", m = 1) +
    s(bio_05, bs = "tp", k = 10, m = 2) +
    s(bio_05, species, bs = "fs", m = 1) +
    # s(bio_06, bs = "tp", k = 10, m = 2) +
    # s(bio_06, species, bs = "fs", m = 1) +
    s(bio_12, bs = "tp", k = 10, m = 2) +
    s(bio_12, species, bs = "fs", m = 1),
    # s(bio_15, bs = "tp", k = 10, m = 2) +
    # s(bio_15, species, bs = "fs", m = 1),
  data = model_data,
  method = "fREML",
  family = binomial(link = "cloglog"),
  weights = model_data$wt,
  # select = TRUE,
  discrete = TRUE,
  control = gam.control(trace = FALSE), 
  drop.unused.levels = FALSE
)

summary(modelPS)
# gratia::draw(modelPS)
plot(modelPS, pages = 1, rug = FALSE, shade = TRUE)

gam.check(modelPS)


# data.frame(
#   mod1 = predict(modelPS, type = "response")[1:6],
#   mod2 = predict(modelPS, type = "response", exclude = "s(species)")[1:6],
#   mod3 = predict(modelPS, newdata = head(model_data)[,1:8], type = "response")
# )

# spatial prediction ------------------------------------------------------
covar <- c("wc2.1_30s_bio_4.tif", "wc2.1_30s_bio_5.tif",
           "wc2.1_30s_bio_6.tif", "wc2.1_30s_bio_12.tif",
           "wc2.1_30s_bio_15.tif")

extr <- c(
  xmin = 60,
  xmax = 180,
  ymin = -45,
  ymax = 54
)

rst <- rast(paste0("data/bg_layers/", covar)) %>% 
  setNames(c("bio_04", "bio_05", "bio_06", "bio_12", "bio_15")) %>% 
  terra::crop(extr) %>% 
  terra::aggregate(fact = 5)

plot(rst)

# make species raster
spname <- "Perkinsiella saccharicida"
r <- rst[[1]]
r[] <- spname
spr <- mask(r, rst[[1]])
names(spr) <- "species"
plot(spr)

rast_pred <- c(rst, spr)
plot(rast_pred)

facts <- list(species = levels(as.factor(model_data$species)))
prediction <- terra::predict(object = rast_pred,
                             model = modelPS, 
                             # type = "response",
                             factors = facts)
plot(prediction)
plot(exp(prediction))
plot(terra::app(prediction, fun = plogis))

plot(st_geometry(sp_all[sp_all$species == spname, ]), add = TRUE)



# -------------------------------------------------------------------------
# modelling with pca ------------------------------------------------------
spname <- "Perkinsiella saccharicida"

modelA <- bam(
  occ ~ s(PC1, bs  = "tp", k = 10) +
    s(PC2, bs  = "tp", k = 10) +
    s(PC3, bs  = "tp", k = 10) +
    s(PC4, bs  = "tp", k = 10),
  data = model_data[model_data$species == spname, ],
  method = "fREML",
  family = binomial(link = "cloglog"), 
  weights = model_data$wt[model_data$species == spname],
  discrete = TRUE,
  control = gam.control(trace = FALSE), 
  drop.unused.levels = FALSE
)

summary(modelA)
gratia::draw(modelA)

gam.check(modelA)

# get number of backgrounds
num_bg <- as.numeric(table(model_data$occ)[1])

modelP <- gam(
  occ ~ s(species, bs = "re") +
    s(PC1, bs  = "tp", k = 10) +
    s(PC2, bs  = "tp", k = 10) +
    s(PC3, bs  = "tp", k = 10) +
    s(PC4, bs  = "tp", k = 10),
  data = model_data,
  method = "REML",
  family = binomial(link = "cloglog"), 
  weights = model_data$wt
  # weights = ifelse(model_data$occ == 1, 1, 1 / 10000)
  # discrete = TRUE,
  # control = gam.control(trace = FALSE), 
  # drop.unused.levels = FALSE
)

summary(modelP)
gratia::draw(modelP)

gam.check(modelP)



modelPS <- bam(
  occ ~ s(PC1, bs = "tp", k = 10, m = 2) +
    s(PC1, species, bs = "fs", m = 1) +
    s(PC2, bs = "tp", k = 10, m = 2) +
    s(PC2, species, bs = "fs", m = 1) +
    s(PC3, bs = "tp", k = 10, m = 2) +
    s(PC3, species, bs = "fs", m = 1) +
    s(PC6, bs = "tp", k = 10, m = 2) +
    s(PC6, species, bs = "fs", m = 1) +
    s(species, bs = "re"),
  data = model_data,
  method = "fREML",
  family = binomial(link = "cloglog"),
  weights = ifelse(model_data$wt == 1, 1, 1e6),
  # select = TRUE,
  discrete = TRUE,
  control = gam.control(trace = FALSE), 
  drop.unused.levels = FALSE
)

summary(modelPS)
# gratia::draw(modelPS)
plot(modelPS, pages = 1, rug = FALSE, shade = TRUE)

gam.check(modelPS)

# spatial prediction ------------------------------------------------------
extr <- c(
  xmin = 60,
  xmax = 180,
  ymin = -45,
  ymax = 54
)

rst <- rast_pca %>% 
  terra::crop(extr)
# terra::aggregate(fact = 5)

plot(rst)

# make species raster
spname <- "Perkinsiella saccharicida"
r <- rst[[1]]
r[] <- spname
spr <- mask(r, rst[[1]])
names(spr) <- "species"
plot(spr)

rast_pred <- c(rst, spr)
plot(rast_pred)

facts <- list(species = levels(as.factor(model_data$species)))
prediction <- terra::predict(object = rast_pred, 
                             model = modelPS,   ##******************
                             # type = "response",
                             factors = facts)
plot(prediction)
plot(terra::app(prediction, fun = plogis))
plot(exp(prediction) * 2500000)

plot(st_geometry(sp_all[sp_all$species == spname, ]), add = TRUE)
plot(st_geometry(sp_all), add = TRUE)


library(mapview)
# plot in mapview
newpred <- raster::raster(exp(mixmod_pred) * 25000)
# raster::writeRaster(newpred, "predictions//P_saccharicida.tif", overwrite = TRUE)
mapview::mapview(newpred, 
                 col.regions = terrain.colors(10, rev = TRUE),
                 na.color = NA)



# -------------------------------------------------------------------------
# linear mixed models -----------------------------------------------------
library(terra)

extr <- c(
  xmin = 60,
  xmax = 180,
  ymin = -45,
  ymax = 54
)

nrst <- rst %>% 
  terra::crop(extr) %>% 
  terra::aggregate(fact = 5)

# make species raster
spname <- "Perkinsiella saccharicida"
r <- nrst[[1]]
r[] <- spname
r <- mask(r, nrst[[1]])
names(r) <- "species"
rast_pred <- c(nrst, r)
plot(rast_pred)



library(lme4)

tm <- Sys.time()
mixmodel <- lme4::glmer(
  occ ~ (1 | species) + bio_01 + bio_03 + (bio_01 + bio_03 | species) +
    (evi * bio_12),
  data = model_data,
  family = binomial(link = "cloglog"),
  control = glmerControl(calc.derivs = FALSE),
  weights = ifelse(model_data$wt == 1, 1, 1e6)
)
Sys.time() - tm

summary(mixmodel)

mixmod_pred <- terra::predict(object = rast_pred, 
                              model = mixmodel,
                              # type = "response",
                              allow.new.levels = TRUE)
plot(mixmod_pred)
plot(terra::app(mixmod_pred, fun = plogis))
plot(exp(mixmod_pred) * 2500000)



