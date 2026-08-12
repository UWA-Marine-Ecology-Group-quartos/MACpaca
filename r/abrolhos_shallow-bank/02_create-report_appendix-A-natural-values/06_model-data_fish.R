###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Fish data synthesis
# Task:    Model fish data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
###

rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park
years <- config$years

combine_benthos <- config$combine_benthos
benthos_label <- if (combine_benthos) paste(years, collapse = "_") else NA

library(mgcv)
library(tidyverse)
library(terra)
library(sf)
library(predicts)
library(patchwork)
library(FSSgam)
library(CheckEM)

tidy_maxn <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-count.rds")) %>% # TODO check outlier removal
  #dplyr::filter(geoscience_roughness < 4) %>% # Remove outliers in roughness
  glimpse()

# Re-set the predictors for modeling----
names(tidy_maxn)
pred.vars <- c("reef", "geoscience_depth","geoscience_aspect" , "geoscience_roughness", "geoscience_detrended")

# TODO Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(tidy_maxn[ , pred.vars]), 2)

# TODO Review of individual predictors for even distribution---
CheckEM::plot_transformations(pred.vars = pred.vars, dat = tidy_maxn)

# TODO Check to make sure Response vector has not more than 80% zeros---
unique.vars <- unique(as.character(tidy_maxn$response))

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy_maxn[which(tidy_maxn$response == unique.vars[i]), ]
  if(length(which(temp.dat$count == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars

# Run the full subset model selection----
savedir <- paste0("output/model-output/", park, "/fish/maxn/")
factor.vars <- c("year") # TODO set factors, drop year if only one year of data
out.all     <- list()
var.imp     <- list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- as.data.frame(tidy_maxn[which(tidy_maxn$response == resp.vars[i]), ])
  Model1  <- gam(count ~ s(geoscience_depth, k = 3, bs = 'cr'),
                 family = tw(),  data = use.dat) # TODO check family

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = "geoscience_aspect",
                                  k = 3, # TODO check this, maybe add cov.cutoff
                                  factor.smooth.interactions = F, # TODO check this
                                  max.predictors = 5 # TODO check this
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T,
                            r2.type = "dev")
  names(out.list)

  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i   <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all <- c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp <- c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2

  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name = as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model,all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}
    dev.off()
  }
}

# Save model fits, data, and importance scores---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits   <- do.call("rbind",out.all)
all.var.imp    <- do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))

# Do FSS for B20
tidy_b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-b20.rds")) %>%
  # dplyr::filter(geoscience_roughness < 4) %>% # TODO check, make same as above
  glimpse()

# # Re-set the predictors for modeling----
names(tidy_b20)
pred.vars <- c("reef", "geoscience_depth","geoscience_aspect" , "geoscience_roughness", "geoscience_detrended")

# TODO Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(tidy_b20[ , pred.vars]), 2)

# TODO Review of individual predictors for even distribution---
CheckEM::plot_transformations(pred.vars = pred.vars, dat = tidy_b20)

# TODO Check to make sure Response vector has not more than 80% zeros----
unique.vars <- unique(tidy_b20$response)

resp.vars <- character()
for(i in 1:length(unique.vars)){
  temp.dat <- tidy_b20[which(tidy_b20$response == unique.vars[i]), ]
  if(length(which(temp.dat$count == 0)) / nrow(temp.dat) < 0.8){
    resp.vars <- c(resp.vars, unique.vars[i])}
}
resp.vars

# Run the full subset model selection----
savedir <- paste0("output/model-output/", park, "/fish/length/")
name_b20 <- paste(name,"b20", sep = "_")
out.all <- list()
var.imp <- list()
factor.vars <- c("year") # TODO check, drop year if only one year of data

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat = as.data.frame(tidy_b20[which(tidy_b20$response==resp.vars[i]),])
  Model1  <- gam(count ~ s(geoscience_depth, k = 3, bs = 'cr'),
                 tw(),  data = use.dat) # TODO check family

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = "geoscience_aspect",
                                  k = 3, # TODO check this, maybe add cov.cutoff
                                  factor.smooth.interactions = F, # TODO check this
                                  max.predictors = 5 # TODO check this
  )
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T,
                         r2.type = "dev")
  names(out.list)

  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2

  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name_b20, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}
    dev.off()
  }
}

# Model fits and importance---
names(out.all) = resp.vars
names(var.imp) = resp.vars
all.mod.fits = do.call("rbind", out.all)
all.var.imp = do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name_b20, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name_b20, "all.var.imp.csv", sep = "_"), sep = "/"))

# read in
fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  glimpse()

## TODO Select best models from above then write them below (check all.mod.fits and all.var.imp)
# For each response, carefully write the selected model choosing model type (family),
# predictor variables, factor variables, k and bs
#
## Models selected using the simplest model within delta AICc 2 of the top model.
## Family is tw() throughout to match the family the model selection was run
## under - AICc values are not comparable across families, so switching here
## would mean the selected model was chosen under different assumptions.
##
## Year was offered to the candidate set for all four responses. Only species
## richness retained it (importance 0.993); year importance was 0 for total
## abundance and 0.294 for CTI. Predictions are still written per year so that
## the plotting scripts have one surface per survey - for the three metrics
## without a year term those surfaces are identical between years.

# Total abundance - depth alone (delta 0, wi 0.715). The only other model within
# 2 adds roughness for +0.006 deviance explained.
m_abundance <- gam(count ~ s(geoscience_depth, k = 3, bs = "cr"),
                   data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                   family = tw())
summary(m_abundance)

# Species richness - only one model fell within delta AICc 2, so there is no
# simpler alternative. 2021 has 24 BRUV deployments against 120 in 2025, and the
# 2021 samples are confined to NPZ6, so part of the year effect is spatial.
m_richness <- gam(count ~ s(geoscience_aspect, k = 3, bs = "cc") +
                    s(reef, k = 3, bs = "cr") +
                    year,
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = tw())
summary(m_richness)

# CTI - depth alone (delta 0). Three other models within 2 each add one term for
# ~0.01 deviance explained. Deviance explained is only 14% - interpret with care.
m_cti <- gam(count ~ s(geoscience_depth, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = tw())
summary(m_cti)

# B20 - reef alone (delta 0, wi 0.308). Deviance explained is 8.6% and the models
# within delta 2 are near-indistinguishable - treat the surface as indicative.
# NOTE reef is the only predictor, so no MESS mask is applied to this layer.
# TODO re-check the b20 table now that year is in the candidate set
m_b20 <- gam(count ~ s(reef, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "b20"),
             family = tw())
summary(m_b20)

# Read predictor rasters to predict onto (bathymetry derivatives etc.)
preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
plot(preds)

# Predictors as a dataframe for modelling
preddf <- preds %>%
  as.data.frame(xy = TRUE, na.rm = TRUE) %>%
  glimpse()

# Extract status to predict onto (same as habitat script)
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos")) %>% # TODO select marine parks in your area
  dplyr::filter(zone_type %in% c("Sanctuary Zone (IUCN VI)",
                                 "National Park Zone (IUCN II)")) %>%
  dplyr::mutate(status = "No-Take") %>%
  vect()

# Points for extraction
predv <- vect(preddf, geom = c("x", "y"), crs = "epsg:4326")

# Add status (No-Take / Fished) to prediction dataframe
preddf_s <- cbind(preddf, terra::extract(marine_parks, predv)) %>%
  dplyr::mutate(status = as.factor(ifelse(is.na(status), "Fished", "No-Take"))) %>%
  glimpse()

## ------------------------------------------------------------
## ADD YEAR-SPECIFIC REEF FOR FISH MODELLING
## ------------------------------------------------------------
# One reef surface per fish year. If benthos was pooled there is a single
# combined-years habitat file, so every fish year reuses that same reef surface.
if (combine_benthos) {
  reef_r <- readRDS(paste0("output/model-output/", park, "/habitat/",
                           name, "_predicted-habitat_", benthos_label, ".rds")) %>%
    terra::subset("p_reef.fit")
  names(reef_r) <- "reef"
  reef_by_year <- setNames(rep(list(reef_r), length(years)), years)
} else {
  reef_by_year <- setNames(lapply(years, function(y) {
    r <- readRDS(paste0("output/model-output/", park, "/habitat/",
                        name, "_predicted-habitat_", y, ".rds")) %>%
      terra::subset("p_reef.fit")
    names(r) <- "reef"; r
  }), years)
}

# Build one prediction frame per fish year, each with its reef surface
preddf_sy <- purrr::map_dfr(years, function(yy) {
  cbind(preddf_s,
        terra::extract(reef_by_year[[as.character(yy)]], predv)[, "reef", drop = FALSE]) %>%
    dplyr::mutate(year = yy)
}) %>%
  dplyr::mutate(year = factor(year, levels = levels(fabund$year)))

count(preddf_sy, year)

## ------------------------------------------------------------
## PREDICT FISH METRICS FOR BOTH YEARS
## ------------------------------------------------------------

predicted_fish <- cbind(
  preddf_sy,
  "p_abundance" = mgcv::predict.gam(m_abundance, preddf_sy, type = "response", se.fit = TRUE),
  "p_richness"  = mgcv::predict.gam(m_richness,  preddf_sy, type = "response", se.fit = TRUE),
  "p_cti"       = mgcv::predict.gam(m_cti,       preddf_sy, type = "response", se.fit = TRUE),
  "p_b20"       = mgcv::predict.gam(m_b20,       preddf_sy, type = "response", se.fit = TRUE)
) %>%
  glimpse()

## ------------------------------------------------------------
## RASTERISE FISH PREDICTIONS BY YEAR (same format as habitat)
## ------------------------------------------------------------
for (yy in years) {

  prasts <- rast(
    predicted_fish %>%
      dplyr::filter(as.character(year) %in% as.character(yy)) %>%
      dplyr::select(x, y, starts_with("p_")),
    crs = "epsg:4326"
  )

  plot(prasts, main = yy)
  print(summary(prasts))
}

# Calculate MESS and mask predictions

resp.vars <- c("p_abundance", "p_richness", "p_cti", "p_b20")
pred.years <- years

for (y in seq_along(pred.years)) {

  this_year <- pred.years[y]
  print(this_year)

  xy <- fabund %>%
    dplyr::filter(as.character(year) == this_year) %>%
    dplyr::transmute(x = longitude_dd, y = latitude_dd)

  for (i in seq_along(resp.vars)) {

    print(resp.vars[i])
    mod <- get(str_replace_all(resp.vars[i], "p_", "m_"))

    temppred <- predicted_fish %>%
      dplyr::filter(as.character(year) == this_year) %>%
      dplyr::select(x, y,
                    paste0(resp.vars[i], ".fit"),
                    paste0(resp.vars[i], ".se.fit")) %>%
      rast(crs = "epsg:4326")

    geo.vars <- names(mod$model)[startsWith(names(mod$model), "geoscience")]

    if (length(geo.vars) > 0) {

      xr  <- subset(preds, geo.vars)

      dat <- terra::extract(xr, xy) %>%
        dplyr::select(-ID) %>%
        as.data.frame()

      # drop rows with NA covariates
      dat <- dat[stats::complete.cases(dat), , drop = FALSE]

      if (nrow(dat) == 0) {
        message("No complete covariate rows for ", resp.vars[i],
                " (", this_year, "). Skipping mask.")
        temppred_m <- temppred

      } else if (length(geo.vars) == 1) {

        # --- univariate mask: keep only cells within observed range ---
        vmin <- min(dat[[1]], na.rm = TRUE)
        vmax <- max(dat[[1]], na.rm = TRUE)

        maskrast <- xr[[1]]
        maskrast <- terra::ifel(maskrast >= vmin & maskrast <= vmax, 1, NA)

        maskrast <- terra::crop(maskrast, temppred)
        temppred_m <- terra::mask(temppred, maskrast)

      } else {

        # --- multivariate MESS (works fine for >=2 predictors) ---
        messrast <- predicts::mess(xr, dat) %>%
          terra::clamp(lower = -0.01, values = FALSE) %>%
          terra::crop(temppred)

        temppred_m <- terra::mask(temppred, messrast)
      }

    } else {
      message("No geoscience predictors in model for ", resp.vars[i],
              " (", this_year, "). Skipping MESS mask.")
      temppred_m <- temppred
    }

    if (i == 1) {
      preddf_m <- temppred_m
    } else {
      preddf_m <- c(preddf_m, temppred_m)   # <- combine layers
    }

  }

  plot(preddf_m)

  saveRDS(preddf_m,
          paste0("output/model-output/", park, "/fish/",
                 name, "_predicted-fish_", this_year, ".rds"))

  writeRaster(preddf_m,
              paste0("output/model-output/", park, "/fish/",
                     names(preddf_m), "_predicted_", this_year, ".tif"),
              overwrite = TRUE)
}
