library(tidyverse)
library(mgcv)
library(terra)



modPS <- bam(pts ~ s(open_prop, bs = "tp", k = 15, m = 2) + 
              s(open_prop, ID, bs = "fs", m = 1) + 
              s(demRange, bs = "tp", k = 15, m = 2) + 
              s(demRange, ID, bs = "fs", m = 1), 
            data = data, 
            method = "fREML", 
            family = binomial(link = "cloglog"), 
            weights = w, 
            discrete = TRUE, 
            control = gam.control(trace = FALSE), 
            drop.unused.levels = FALSE)


modPS2 <- gam(pts ~ s(open_prop, bs="tp", k=15, m=2) +
                t2(open_prop, ID, bs=c("tp","re"), ord=2, m=1) +
                s(open_prop, ID, bs="re") +
                s(demRange, bs="tp", k=15, m=2) +
                t2(demRange, ID, bs=c("tp","re"), ord=2, m=1) +
                s(demRange, ID, bs="re") +
                s(ID, bs="re"), 
              data = data,
              method = "REML",
              family = binomial(link = "cloglog"), 
              weights = w, 
              discrete = FALSE,
              control = gam.control(trace = FALSE), 
              drop.unused.levels = FALSE)

