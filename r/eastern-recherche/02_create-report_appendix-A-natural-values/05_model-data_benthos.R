###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis
# Task:    Model habitat data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer
# Date:    June 2024
###

rm(list=ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

## TODO Run below to install FSSgam package
# if (!requireNamespace("remotes", quietly = TRUE)) {
#   install.packages("remotes")
# }
# remotes::install_github("beckyfisher/FSSgam_package")

library(CheckEM)
library(tidyverse)
library(mgcv)
library(FSSgam)
library(patchwork)
library(terra)
library(sf)

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

# Bring in and format the data ----
habi <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  # dplyr::filter(geoscience_roughness < 20) %>% # AL adjusted
  glimpse()

model_dat <- habi %>%
  pivot_longer(cols = c(macroalgae, sand, rock, sessile_invertebrates, reef, seagrasses),
               names_to = "response", values_to = "number") %>%
  glimpse()

# Set predictor variables ----
pred.vars <- c("geoscience_depth", "geoscience_aspect", "geoscience_roughness", "geoscience_detrended")

# TODO Check for correlation of predictor variables - remove anything highly correlated (>0.95) ----
round(cor(model_dat[ , pred.vars]), 2)

# TODO Review of individual predictors for even distribution ----
CheckEM::plot_transformations(pred.vars = pred.vars, dat = model_dat)

# TODO Check to make sure Response vector has not more than 80% zeros ----
(unique.vars = unique(as.character(model_dat$response)))

unique.vars.use = character()
for(i in 1:length(unique.vars)){
  temp.dat = model_dat[which(model_dat$response == unique.vars[i]),]
  if(length(which(temp.dat$number == 0))/nrow(temp.dat) < 0.8){
    unique.vars.use = c(unique.vars.use, unique.vars[i])}
}

unique.vars.use

# # Or you can force in your own variables, you might need reef for fish predictions
# unique.vars.use <- c("macroalgae",
#                      "sand",
#                      "rock",
#                      "sessile_invertebrates",
#                      "reef",
#                      "seagrasses")

# Run the full subset model selection ----
outdir    <- paste0("output/model-output/", park, "/habitat/")
out.all   <- list()
var.imp   <- list()
resp.vars <- unique.vars.use

# Loop through the FSS function for each habitat response ----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- model_dat[model_dat$response == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
  Model1  <- gam(cbind(number, (total_pts - number)) ~
                   s(geoscience_depth, bs = 'cr'),
                 family = binomial("logit"), data = use.dat) # TODO check family

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = NULL, # year removed - pooled benchmark
                                  cyclic.vars = c("geoscience_aspect"),
                                  k = 3, # TODO check this
                                  cov.cutoff = 0.7,
                                  max.predictors = 4 # TODO check this
  )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T,
                            r2.type = "dev")
  names(out.list)

  out.list$failed.models
  mod.table <- out.list$mod.data.out
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))

  # Plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])

    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}
    dev.off()
  }
}

# Model fits and importance ----
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- list_rbind(out.all, names_to = "response")
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste0(outdir, name, "_abiotic_all.mod.fits.csv"))
write.csv(all.var.imp,         file = paste0(outdir, name, "_abiotic_all.var.imp.csv"))

## TODO Select best models from above then write them below (check all.mod.fits and all.var.imp)
# For each response, carefully write the selected model choosing model type (family),
# predictor variables, k and bs. Year removed - pooled benchmark approach.

# Sand - best model: all four predictors
m_sand <- gam(cbind(sand, total_pts - sand) ~
                s(geoscience_aspect, k = 3, bs = "cc") +
                s(geoscience_depth, k = 3, bs = "cr") +
                s(geoscience_detrended, k = 3, bs = "cr") +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

# Rock - too rare to model
# m_rock <- gam(cbind(rock, total_pts - rock) ~
#                 s(geoscience_aspect, k = 5, bs = "cc") +
#                 s(geoscience_detrended, k = 5, bs = "cr") +
#                 s(geoscience_roughness, k = 5, bs = "cr"),
#               data = habi, method = "REML", family = binomial("logit"))
# summary(m_rock)

# Macroalgae - best model: all four predictors
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 s(geoscience_aspect, k = 3, bs = "cc") +
                 s(geoscience_depth, k = 3, bs = "cr") +
                 s(geoscience_detrended, k = 3, bs = "cr") +
                 s(geoscience_roughness, k = 3, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

# Seagrass - not in best models
# m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
#                     s(geoscience_aspect, k = 5, bs = "cc") +
#                     s(geoscience_depth, k = 5, bs = "cr") +
#                     s(geoscience_detrended, k = 5, bs = "cr"),
#                   data = habi, method = "REML", family = binomial("logit"))
# summary(m_seagrass)

# Inverts - best model: all four predictors
m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   s(geoscience_aspect, k = 3, bs = "cc") +
                   s(geoscience_depth, k = 3, bs = "cr") +
                   s(geoscience_detrended, k = 3, bs = "cr") +
                   s(geoscience_roughness, k = 3, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))
summary(m_inverts)

# Reef - best model: all four predictors
m_reef <- gam(cbind(reef, total_pts - reef) ~
                s(geoscience_aspect, k = 3, bs = "cc") +
                s(geoscience_depth, k = 3, bs = "cr") +
                s(geoscience_detrended, k = 3, bs = "cr") +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)

# Read predictor rasters to predict onto ----
preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
preddf <- preds %>%
  as.data.frame(xy = T, na.rm = T)

# Extract status to predict onto
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) %>% # TODO select marine parks in your area
  dplyr::filter(zone_type %in% c("Sanctuary Zone (IUCN VI)",
                                 "National Park Zone (IUCN II)")) %>%
  dplyr::mutate(status = "No-Take") %>%
  vect() %>%
  glimpse()

predv <- vect(preddf, geom = c("x", "y"), crs = "epsg:4326")

preddf_s <- cbind(preddf, terra::extract(marine_parks, predv)) %>%
  dplyr::mutate(status = as.factor(ifelse(is.na(status), "Fished", "No-Take"))) %>%
  glimpse()

# Predict onto single pooled grid (no year duplication) ----
# Reef is predicted and saved alongside the three habitat classes because
# 06_model-data_fish.R uses it as a predictor and 07 plots it. It is excluded
# from the dominant-habitat layer and the combined SE below - reef is
# macroalgae + rock + sessile invertebrates by construction, so it would win
# every cell in a which.max() and would double-count in the mean SE.
resp.vars <- c("p_sand", "p_macro", "p_inverts", "p_reef")

predhab <- cbind(preddf_s,
                 "p_macro"   = predict(m_macro,   preddf_s, type = "response", se.fit = T),
                 "p_sand"    = predict(m_sand,     preddf_s, type = "response", se.fit = T),
                 "p_inverts" = predict(m_inverts,  preddf_s, type = "response", se.fit = T),
                 "p_reef"    = predict(m_reef,     preddf_s, type = "response", se.fit = T)
) %>%
  glimpse()

# Labels and colours for dominant habitat outputs
dom_labels <- c(
  sand    = "Sand",
  macro   = "Macroalgae",
  inverts = "Sessile invertebrates"
)

# Helper: create dominant class raster from *.fit layers only
benthos_dom_tag <- function(r) {

  fit_lyrs <- names(r)[
    grepl("\\.fit$", names(r)) &
      !grepl("\\.se\\.fit$", names(r)) &
      !grepl("^p_reef\\.fit$", names(r))
  ]

  r_fit <- terra::subset(r, fit_lyrs)

  dom <- terra::which.max(r_fit)

  levels(dom) <- data.frame(
    ID = seq_along(fit_lyrs),
    dom_tag = sub("^p_", "", sub("\\.fit$", "", fit_lyrs))
  )

  names(dom) <- "dom_tag"
  dom
}

normalise <- function(x) {
  xmin <- terra::global(x, "min", na.rm = TRUE)[1, 1]
  xmax <- terra::global(x, "max", na.rm = TRUE)[1, 1]

  if (isTRUE(all.equal(xmin, xmax))) {
    return(x * NA_real_)
  }

  (x - xmin) / (xmax - xmin)
}

# MESS masking and raster output - pooled across years ----

# Reference point cloud: all samples from both years combined
xy_all <- habi %>%
  dplyr::transmute(x = longitude_dd, y = latitude_dd)

preddf_m <- NULL

for (resp_var in resp.vars) {

  print(resp_var)

  mod <- get(stringr::str_replace(resp_var, "^p_", "m_"))

  temppred <- predhab %>%
    dplyr::select(
      x, y,
      dplyr::all_of(paste0(resp_var, ".fit")),
      dplyr::all_of(paste0(resp_var, ".se.fit"))
    ) %>%
    terra::rast(crs = "epsg:4326")

  geo.vars <- names(mod$model)[startsWith(names(mod$model), "geoscience")]

  dat <- terra::extract(terra::subset(preds, geo.vars), xy_all) %>%
    dplyr::select(-ID)

  messrast <- predicts::mess(terra::subset(preds, geo.vars), dat) %>%
    terra::clamp(lower = -0.01, values = FALSE) %>%
    terra::crop(temppred)

  temppred_m <- terra::mask(temppred, messrast)

  preddf_m <- if (is.null(preddf_m)) temppred_m else c(preddf_m, temppred_m)
}

plot(preddf_m)

# Add dominant habitat layer
dom_rast <- benthos_dom_tag(preddf_m)

# Combined standard error ----
# Reef deliberately excluded - see the note above benthos_dom_tag()
se_rasts <- terra::subset(
  preddf_m,
  c("p_macro.se.fit", "p_sand.se.fit", "p_inverts.se.fit")
)

se_rasts_norm <- terra::rast(
  lapply(1:terra::nlyr(se_rasts), function(i) normalise(se_rasts[[i]]))
)
names(se_rasts_norm) <- names(se_rasts)

mean_se <- terra::mean(se_rasts_norm, na.rm = TRUE)
names(mean_se) <- "mean_se"

# Stack fits + se.fits + dominant habitat + combined SE
preddf_m2 <- c(preddf_m, dom_rast, mean_se)

# Data frame for ggplot categorical tiles
pred_dom_df <- as.data.frame(dom_rast, xy = TRUE, na.rm = TRUE) %>%
  dplyr::mutate(
    dom_tag = unname(dom_labels[as.character(dom_tag)]),
    dom_tag = factor(
      dom_tag,
      levels = c("Sand", "Macroalgae", "Sessile invertebrates", "Reef")
    )
  )

# Optional sanity check
print(table(pred_dom_df$dom_tag, useNA = "ifany"))

plot(dom_rast)
plot(mean_se)

# Write outputs - single pooled benchmark prediction ----
writeRaster(
  preddf_m,
  paste0("output/model-output/", park, "/habitat/", names(preddf_m), "_predicted.tif"),
  overwrite = TRUE
)

saveRDS(
  se_rasts_norm,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-se-normalised.rds")
)

writeRaster(
  se_rasts_norm,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-se-normalised.tif"),
  overwrite = TRUE
)

saveRDS(
  mean_se,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-mean-se.rds")
)

writeRaster(
  mean_se,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-mean-se.tif"),
  overwrite = TRUE
)

saveRDS(
  preddf_m2,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-habitat.rds")
)

saveRDS(
  pred_dom_df,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-dominant-habitat.rds")
)

writeRaster(
  preddf_m2,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-habitat-with-dominant.tif"),
  overwrite = TRUE
)

writeRaster(
  dom_rast,
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-dominant-habitat.tif"),
  overwrite = TRUE
)
