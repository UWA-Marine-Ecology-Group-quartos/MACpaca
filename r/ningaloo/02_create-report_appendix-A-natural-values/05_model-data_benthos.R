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
years <- config$years
combine_benthos <- config$combine_benthos

# Label used to save/read pooled benthos outputs (single combined file)
# instead of one file per year. Ignored when combine_benthos is FALSE.
benthos_label <- if (combine_benthos) paste(years, collapse = "_") else NA

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
  dplyr::filter(geoscience_roughness < 4) %>% # TODO Filter outliers - check and adjust
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
factor.vars <- c("year") # TODO set factors - if one year of data or combining years set as null

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
# TODO if only one year: remove "year +" and drop ", by = year" from each s() term for every model below

# Sand
m_sand <- gam(cbind(sand, total_pts - sand) ~
                year +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_depth, by = year, k = 5, bs = "cr") +
                s(geoscience_detrended, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)

# Rock - too rare to model
m_rock <- gam(cbind(rock, total_pts - rock) ~
                year +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_detrended, by = year, k = 5, bs = "cr") +
                s(geoscience_roughness, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)

# Macroalgae
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 year +
                 s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                 s(geoscience_depth, by = year, k = 5, bs = "cr") +
                 s(geoscience_detrended, by = year, k = 5, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)

# Seagrass
m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
                    year +
                    s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                    s(geoscience_depth, by = year, k = 5, bs = "cr") +
                    s(geoscience_detrended, by = year, k = 5, bs = "cr"),
                  data = habi, method = "REML", family = binomial("logit"))
summary(m_seagrass)

# Inverts
m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   year +
                   s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                   s(geoscience_depth, by = year, k = 5, bs = "cr") +
                   s(geoscience_roughness, by = year, k = 5, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))
summary(m_inverts)

# Reef
m_reef <- gam(cbind(reef, total_pts - reef) ~
                year +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_detrended, by = year, k = 5, bs = "cr") +
                s(geoscience_roughness, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))
summary(m_reef)

# Read predictor rasters to predict onto
preds <- readRDS(paste0("data/", park, "/spatial/rasters/", name, "_bathymetry-derivatives.rds"))
preddf <- preds %>%
  as.data.frame(xy = T, na.rm = T)

# Extract status to predict onto
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>% # TODO select marine parks in your area
  dplyr::filter(zone_type %in% c("Sanctuary Zone (IUCN VI)",
                                 "National Park Zone (IUCN II)")) %>%
  dplyr::mutate(status = "No-Take") %>%
  vect() %>%
  glimpse()

predv <- vect(preddf, geom = c("x", "y"), crs = "epsg:4326")

preddf_s <- cbind(preddf, terra::extract(marine_parks, predv)) %>%
  dplyr::mutate(status = as.factor(ifelse(is.na(status), "Fished", "No-Take"))) %>%
  glimpse()

if (combine_benthos) {
  preddf_sy <- preddf_s
} else {
  preddf_sy <- purrr::map_dfr(years, \(y) preddf_s %>% dplyr::mutate(year = y)) %>%
    dplyr::mutate(year = factor(year, levels = levels(habi$year)))
}

# predict, rasterise and plot
# TODO comment-out any habitats not modeled above
predhab <- cbind(preddf_sy,
                 "p_macro"    = predict(m_macro, preddf_sy, type = "response", se.fit = T),
                 "p_sand"     = predict(m_sand, preddf_sy, type = "response", se.fit = T),
                 "p_seagrass" = predict(m_seagrass, preddf_sy, type = "response", se.fit = T),
                 "p_inverts"  = predict(m_inverts, preddf_sy, type = "response", se.fit = T),
                 "p_rock"     = predict(m_rock, preddf_sy, type = "response", se.fit = T),
                 "p_reef"     = predict(m_reef, preddf_sy, type = "response", se.fit = T)
                 ) %>%
  glimpse()

# Calculate MESS and mask predictions ----
# TODO remove habitats not predicted
resp.vars <- c("p_sand", "p_macro", "p_seagrass", "p_inverts", "p_rock", "p_reef")

# One output per label: each survey year, or a single combined label when pooling.
pred.labels <- if (combine_benthos) benthos_label else years

# Helper: return the habi + predhab rows that belong to a given output label.
# When pooling there is no year split, so we use every row.
rows_for <- function(label) {
  if (combine_benthos) {
    list(habi = habi, predhab = predhab)
  } else {
    list(
      habi    = dplyr::filter(habi,    as.character(year) == label),
      predhab = dplyr::filter(predhab, as.character(year) == label)
    )
  }
}

# Labels and colours for dominant habitat outputs
dom_labels <- c(
  sand = "Sand",
  macro = "Macroalgae",
  seagrass = "Seagrass",
  inverts = "Sessile invertebrates",
  reef = "Reef",
  rock = "Rock"
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

# Create and save prediction layers
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
  # Combined standard error
  # ---------------------------
  # TODO remove irrelevant habitats
  se_rasts <- terra::subset(
    preddf_m,
    c("p_macro.se.fit", "p_rock.se.fit", "p_sand.se.fit",
      "p_seagrass.se.fit", "p_inverts.se.fit", "p_reef.se.fit")
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
  # TODO remove irrelevant habitats
  pred_dom_df <- as.data.frame(dom_rast, xy = TRUE, na.rm = TRUE) %>%
    dplyr::mutate(
      dom_tag = unname(dom_labels[as.character(dom_tag)]),
      dom_tag = factor(
        dom_tag,
        levels = c("Sand", "Macroalgae", "Seagrass",
                   "Rock", "Sessile invertebrates", "Reef")
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
