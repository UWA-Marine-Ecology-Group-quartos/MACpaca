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
years <- unlist(config$years)  # coerce YAML list -> atomic vector (avoids silent NA bugs downstream)

combine_benthos <- config$combine_benthos
benthos_label <- if (combine_benthos) paste(years, collapse = "_") else NA

# Fish are modelled with YEARS SEPARATE (year as a factor, one predicted surface
# per survey year), even though 05_model-data_benthos.R pools the benthos into a
# single combined habitat product. That means every fish year shares the one
# pooled reef surface - see the reef section further down.

# Factor levels for `year`, set ONCE here and reused for the modelling data and
# the prediction grid. If the two ever drift apart predict.gam() returns NA for
# the whole surface without erroring, which is very hard to spot.
year_levels <- as.character(sort(years))

# Make sure output folders exist before the FSS loops start writing PNGs
for (d in c(paste0("output/model-output/", park, "/fish/maxn/"),
            paste0("output/model-output/", park, "/fish/length/"),
            paste0("output/model-output/", park, "/fish/"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}


library(mgcv)
library(tidyverse)
library(terra)
library(sf)
library(predicts)
library(patchwork)
library(FSSgam)
library(CheckEM)

tidy_maxn <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-count.rds")) %>%
  # TODO roughness outlier filter left OFF to match the successful Eastern
  # Recherche run - switch back on only if the diagnostics below justify it
  # dplyr::filter(geoscience_roughness < 4) %>%
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

# Guard - year must match the config, otherwise the year terms and the
# prediction grid below will not line up
stopifnot(!any(is.na(tidy_maxn$year)))
print(table(tidy_maxn$year, useNA = "ifany"))

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

# Only offer a factor to FSS if it actually has 2+ levels. In the previous
# Eastern Recherche run every sample was Fished (no No-Take zones surveyed), so
# status had to be dropped - this works that out from the data rather than
# needing to be remembered.
pick_factors <- function(dat) {
  keep <- c("status", "year")[
    sapply(c("status", "year"), function(v) {
      v %in% names(dat) && dplyr::n_distinct(dat[[v]], na.rm = TRUE) > 1
    })
  ]
  if (length(keep) == 0) NULL else keep
}

factor.vars <- pick_factors(tidy_maxn)
print(factor.vars)
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
    png(file = paste0(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_")))
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
write.csv(all.mod.fits[ , -2], file = paste0(savedir, paste(name, "all.mod.fits.csv", sep = "_")))
write.csv(all.var.imp, file = paste0(savedir, paste(name, "all.var.imp.csv", sep = "_")))

# Do FSS for B20
tidy_b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-b20.rds")) %>%
  # dplyr::filter(geoscience_roughness < 4) %>% # kept consistent with tidy_maxn above
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

stopifnot(!any(is.na(tidy_b20$year)))
print(table(tidy_b20$year, useNA = "ifany"))

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
factor.vars <- pick_factors(tidy_b20)
print(factor.vars)

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
    png(file = paste0(savedir, paste(name_b20, m, resp.vars[i], "mod_fits.png", sep = "_")))
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
write.csv(all.mod.fits[ , -2], file = paste0(savedir, paste(name_b20, "all.mod.fits.csv", sep = "_")))
write.csv(all.var.imp, file = paste0(savedir, paste(name_b20, "all.var.imp.csv", sep = "_")))

# read in
fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

# Every config year must be present, otherwise the year terms below cannot be
# fitted for that year
stopifnot(all(year_levels %in% as.character(unique(fabund$year))))

## TODO Select best models from above then write them below (check all.mod.fits and all.var.imp)
# For each response, carefully write the selected model choosing model type (family),
# predictor variables, factor variables, k and bs

# Total abundance
m_abundance <- gam(count ~ year +
                     s(geoscience_aspect, k = 3, bs = "cc") +
                     s(geoscience_roughness, k = 3, bs = "cr") +
                     s(reef, k = 3, bs = "cr"),
                   data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                   family = tw())
summary(m_abundance)

# Species richness
m_richness <- gam(count ~ year +
                    s(geoscience_roughness, k = 3, bs = "cr") +
                    s(reef, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = tw())
summary(m_richness)

# CTI
m_cti <- gam(count ~ year +
               s(geoscience_detrended, k = 3, bs = "cr") +
               s(reef, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = tw())
summary(m_cti)

# B20
m_b20 <- gam(count ~ year +
               s(geoscience_detrended, k = 3, bs = "cr") +
               s(reef, k = 3, bs = "cr"),
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
  dplyr::filter(.data$name %in% "Eastern Recherche") %>% # TODO select marine parks in your area - must match 05_model-data_benthos.R
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
## ADD REEF SURFACE FOR FISH MODELLING
## ------------------------------------------------------------
# Benthos is POOLED (combine_benthos = TRUE), so 05_model-data_benthos.R writes
# ONE combined habitat file rather than one per year. Every fish year therefore
# shares the same reef surface - the years-separate part of this script is the
# fish models and their predicted surfaces, not reef.
habitat_dir <- paste0("output/model-output/", park, "/habitat/")

read_reef <- function(f) {
  r <- readRDS(f) %>% terra::subset("p_reef.fit")
  names(r) <- "reef"
  r
}

# Try the labelled pooled file first, then the unlabelled one, then per-year
# files (in case the benthos script was run in per-year mode after all).
pooled_files <- c(
  paste0(habitat_dir, name, "_predicted-habitat_", benthos_label, ".rds"),
  paste0(habitat_dir, name, "_predicted-habitat.rds")
)
peryear_files <- paste0(habitat_dir, name, "_predicted-habitat_", years, ".rds")

if (any(file.exists(pooled_files))) {
  f <- pooled_files[file.exists(pooled_files)][1]
  message("Using pooled reef surface: ", f)
  reef_r <- read_reef(f)
  reef_by_year <- setNames(rep(list(reef_r), length(years)), as.character(years))
} else if (all(file.exists(peryear_files))) {
  message("Pooled reef file not found - falling back to year-specific reef surfaces")
  reef_by_year <- setNames(lapply(peryear_files, read_reef), as.character(years))
} else {
  stop("Cannot find a predicted habitat file. Re-run 05_model-data_benthos.R. Looked for:\n  ",
       paste(c(pooled_files, peryear_files), collapse = "\n  "))
}

plot(reef_by_year[[1]])

# Build one prediction frame per fish year.
# NOTE: the loop variable is named `yr`, NOT `y` - preddf_s already has a column
# literally called `y` (latitude), and inside mutate() a bare `y` would resolve
# to that column instead of the loop argument, silently corrupting `year`.
preddf_sy <- purrr::map_dfr(years, function(yr) {
  cbind(preddf_s,
        terra::extract(reef_by_year[[as.character(yr)]], predv)[, "reef", drop = FALSE]) %>%
    dplyr::mutate(year = as.character(yr))
}) %>%
  dplyr::mutate(year = factor(year, levels = year_levels))

# Sanity check before predicting - should show real counts for each year, no NA
print(table(preddf_sy$year, useNA = "ifany"))
stopifnot(identical(levels(preddf_sy$year), levels(fabund$year)))

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
prasts <- setNames(lapply(year_levels, function(yr) {
  rast(
    predicted_fish %>%
      dplyr::filter(as.character(year) %in% yr) %>%
      dplyr::select(x, y, starts_with("p_")),
    crs = "epsg:4326"
  )
}), year_levels)

for (yr in year_levels) {
  message("Unmasked predictions - ", yr)
  plot(prasts[[yr]])
  print(summary(prasts[[yr]]))
}

# Calculate MESS and mask predictions

resp.vars <- c("p_abundance", "p_richness", "p_cti", "p_b20")
pred.years <- year_levels

for (yi in seq_along(pred.years)) {

  this_year <- pred.years[yi]
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
        message("No complete covariate rows for ", resp.vars[i], " (", this_year, "). Skipping mask.")
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

