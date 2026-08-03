###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Habitat data synthesis
# Task:    Model habitat data using the full subsets approach from @beckyfisher/FSSgam
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
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
years <- unlist(config$years)  # coerce YAML list -> atomic vector (avoids silent NA bugs downstream)
combine_benthos <- config$combine_benthos

# Label used to save/read pooled benthos outputs (single combined file)
# instead of one file per year. Ignored when combine_benthos is FALSE.
benthos_label <- if (combine_benthos) paste(years, collapse = "_") else NA

# This version of the script implements the POOLED benthos path only - all
# habitat responses (including reef) are modelled with years combined and
# written out as a single set of files labelled "<year1>_<year2>".
# 06_model-data_fish.R keeps years separate and reuses this one reef surface.
if (!isTRUE(combine_benthos)) {
  stop("combine_benthos is FALSE in 00_config.yml but this script models benthos pooled. ",
       "Set combine_benthos: TRUE, or use the per-year version of this script.")
}

habitat_label <- benthos_label

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

# Bring in and format the data----
habi <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::mutate(year = droplevels(factor(as.character(year)))) %>%
  #dplyr::filter(geoscience_roughness < 4) %>% # TODO Filter outliers - check and adjust
  glimpse()

model_dat <- habi %>%
  pivot_longer(cols = c(macroalgae, sand, rock, sessile_invertebrates, reef, seagrasses),
               names_to = "response", values_to = "number") %>%
  glimpse()

# Set predictor variables---
pred.vars <- c("geoscience_depth", "geoscience_aspect", "geoscience_roughness", "geoscience_detrended")

# TODO Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(model_dat[ , pred.vars]), 2)

# TODO Review of individual predictors for even distribution---
CheckEM::plot_transformations(pred.vars = pred.vars, dat = model_dat)

# TODO Check to make sure Response vector has not more than 80% zeros---
(unique.vars = unique(as.character(model_dat$response)))

unique.vars.use = character()
for(i in 1:length(unique.vars)){
  temp.dat = model_dat[which(model_dat$response == unique.vars[i]),]
  if(length(which(temp.dat$number == 0))/nrow(temp.dat)< 0.8){
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

# Run the full subset model selection----
outdir    <- paste0("output/model-output/", park, "/habitat/")
out.all   <- list()
var.imp   <- list()
resp.vars <- unique.vars.use
factor.vars <- c(NULL) # TODO set factors - if one year of data or combining years set as null

# Loop through the FSS function for each Abiotic taxa----
for(i in 1:length(resp.vars)){
  print(resp.vars[i])
  use.dat <- model_dat[model_dat$response == resp.vars[i],]
  use.dat   <- as.data.frame(use.dat)
  Model1  <- gam(cbind(number, (total_pts - number)) ~
                   s(geoscience_depth, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat) # TODO check family

  model.set <- generate.model.set(use.dat = use.dat,
                                  test.fit = Model1,
                                  pred.vars.cont = pred.vars,
                                  pred.vars.fact = factor.vars,
                                  cyclic.vars = c("geoscience_aspect"),
                                  k = 3, # TODO check this
                                  cov.cutoff = 0.7, # TODO need to check - Fisher recommends 0.28
                                  max.predictors = 4 # TODO check this
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
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))



  # plot the best models
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

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- list_rbind(out.all, names_to = "response")
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste0(outdir, name, "_abiotic_all.mod.fits.csv"))
write.csv(all.var.imp,         file = paste0(outdir, name, "_abiotic_all.var.imp.csv"))

## TODO Select best models from above then write them below (check all.mod.fits and all.var.imp)
# For each response, carefully write the selected model choosing model type (family),
# predictor variables, factor variables, k and bs
#
# ALL responses (sand/macroalgae/inverts/reef) are pooled across years - no
# "year" term and no "by = year" smooths anywhere. Survey years sampled
# different spatial areas, so a single combined benthos surface is the
# defensible product. The fish script reuses this one reef surface for every
# fish year.

# Sand - pooled across years
m_sand <- gam(cbind(sand, total_pts - sand) ~
                s(geoscience_aspect, k = 3, bs = "cc") +
                s(geoscience_depth, k = 3, bs = "cr") +
                s(geoscience_detrended, k = 3, bs = "cr") +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

# Rock - too rare to model, no candidate models returned
# m_rock <- gam(cbind(rock, total_pts - rock) ~
#                 s(geoscience_aspect, k = 5, bs = "cc")  +
#                 s(geoscience_detrended, k = 5, bs = "cr") +
#                 s(geoscience_roughness, k = 5, bs = "cr"),
#               data = habi, method = "REML", family = binomial("logit"))
# summary(m_rock)

# Macroalgae - pooled across years
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 s(geoscience_aspect, k = 3, bs = "cc") +
                 s(geoscience_depth, k = 3, bs = "cr") +
                 s(geoscience_detrended, k = 3, bs = "cr") +
                 s(geoscience_roughness, k = 3, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

# Seagrass - not modelled (no candidate models returned)
# m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
#                     s(geoscience_aspect, k = 5, bs = "cc")  +
#                     s(geoscience_depth, k = 5, bs = "cr") +
#                     s(geoscience_detrended, k = 5, bs = "cr"),
#                   data = habi, method = "REML", family = binomial("logit"))
# summary(m_seagrass)

# Inverts - pooled across years, simplest model within 2 delta AICc
m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   s(geoscience_aspect, k = 3, bs = "cc") +
                   s(geoscience_depth, k = 3, bs = "cr") +
                   s(geoscience_detrended, k = 3, bs = "cr") +
                   s(geoscience_roughness, k = 3, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))
summary(m_inverts)

# Reef - pooled across years, full model (all covariates important = 1).
m_reef <- gam(cbind(reef, total_pts - reef) ~
                s(geoscience_aspect, k = 3, bs = "cc") +
                s(geoscience_depth, k = 3, bs = "cr") +
                s(geoscience_detrended, k = 3, bs = "cr") +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)

# Read predictor rasters to predict onto
preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
preddf <- preds %>%
  as.data.frame(xy = T, na.rm = T)

# Extract status to predict onto
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(.data$name %in% "Eastern Recherche") %>%
  dplyr::filter(zone_type %in% c("Sanctuary Zone (IUCN VI)",
                                 "National Park Zone (IUCN II)")) %>%
  dplyr::mutate(status = "No-Take") %>%
  vect()

predv <- vect(preddf, geom = c("x", "y"), crs = "epsg:4326")

preddf_s <- cbind(preddf, terra::extract(marine_parks, predv)) %>%
  dplyr::mutate(status = as.factor(ifelse(is.na(status), "Fished", "No-Take"))) %>%
  glimpse()

# Predict onto the single pooled grid. No year expansion is needed - none of the
# four models has a `year` term, so there is exactly one surface per response.
predhab <- cbind(preddf_s,
                 "p_macro"    = predict(m_macro, preddf_s, type = "response", se.fit = T),
                 "p_sand"     = predict(m_sand, preddf_s, type = "response", se.fit = T),
                 "p_inverts"  = predict(m_inverts, preddf_s, type = "response", se.fit = T),
                 "p_reef"     = predict(m_reef, preddf_s, type = "response", se.fit = T)
) %>%
  glimpse()

# Calculate MESS and mask predictions ----
resp.vars <- c("p_sand", "p_macro", "p_inverts", "p_reef")

# ONE pooled output, labelled "<year1>_<year2>" (e.g. 2022_2025). This is the
# file 06_model-data_fish.R reads its reef surface from.
pred.labels <- habitat_label

# Helper: years are pooled, so there is no year subsetting to do - every sample
# and every prediction cell belongs to the single combined output. Kept as a
# function so the loop below is unchanged from the per-year version.
rows_for <- function(label) {
  list(
    habi    = habi,
    predhab = predhab
  )
}

# Labels for dominant habitat outputs - rock/seagrass removed (not modelled)
dom_labels <- c(
  sand = "Sand",
  macro = "Macroalgae",
  inverts = "Sessile invertebrates",
  reef = "Reef"
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

# Create and save prediction layers - a single pooled set of files
for (this_label in pred.labels) {

  print(this_label)

  rr <- rows_for(this_label)

  xy <- rr$habi %>%
    dplyr::transmute(x = longitude_dd, y = latitude_dd)

  preddf_m <- NULL

  for (resp_var in resp.vars) {

    print(resp_var)

    mod <- get(stringr::str_replace(resp_var, "^p_", "m_"))

    temppred <- rr$predhab %>%
      dplyr::select(
        x, y,
        dplyr::all_of(paste0(resp_var, ".fit")),
        dplyr::all_of(paste0(resp_var, ".se.fit"))
      ) %>%
      terra::rast(crs = "epsg:4326")

    geo.vars <- names(mod$model)[startsWith(names(mod$model), "geoscience")]

    dat <- terra::extract(terra::subset(preds, geo.vars), xy) %>%
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

  # ---------------------------
  # Combined standard error - rock/seagrass removed (not modelled)
  # ---------------------------
  se_rasts <- terra::subset(
    preddf_m,
    c("p_macro.se.fit", "p_sand.se.fit", "p_inverts.se.fit", "p_reef.se.fit")
  )

  se_rasts_norm <- terra::rast(
    lapply(1:terra::nlyr(se_rasts), function(i) normalise(se_rasts[[i]]))
  )
  names(se_rasts_norm) <- names(se_rasts)

  mean_se <- terra::mean(se_rasts_norm, na.rm = TRUE)
  names(mean_se) <- "mean_se"

  # Stack fits + se.fits + dominant habitat + combined SE
  preddf_m2 <- c(preddf_m, dom_rast, mean_se)

  # Data frame for ggplot categorical tiles - rock/seagrass removed (not modelled)
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

  # Write original masked prediction rasters
  writeRaster(
    preddf_m,
    paste0(
      "output/model-output/", park, "/habitat/",
      names(preddf_m), "_predicted_", this_label, ".tif"
    ),
    overwrite = TRUE
  )

  # Save normalised SE rasters
  saveRDS(
    se_rasts_norm,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-se-normalised_", this_label, ".rds"
    )
  )

  writeRaster(
    se_rasts_norm,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-se-normalised_", this_label, ".tif"
    ),
    overwrite = TRUE
  )

  # Save combined SE raster
  saveRDS(
    mean_se,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-mean-se_", this_label, ".rds"
    )
  )

  writeRaster(
    mean_se,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-mean-se_", this_label, ".tif"
    ),
    overwrite = TRUE
  )

  # Save stack including dominant habitat layer + combined SE
  saveRDS(
    preddf_m2,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-habitat_", this_label, ".rds"
    )
  )

  # Save dominant habitat dataframe for plotting scripts
  saveRDS(
    pred_dom_df,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-dominant-habitat_", this_label, ".rds"
    )
  )

  # Write full raster stack including dominant habitat + combined SE
  writeRaster(
    preddf_m2,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-habitat-with-dominant_", this_label, ".tif"
    ),
    overwrite = TRUE
  )

  # Write dominant habitat raster only
  writeRaster(
    dom_rast,
    paste0(
      "output/model-output/", park, "/habitat/",
      name, "_predicted-dominant-habitat_", this_label, ".tif"
    ),
    overwrite = TRUE
  )
}
