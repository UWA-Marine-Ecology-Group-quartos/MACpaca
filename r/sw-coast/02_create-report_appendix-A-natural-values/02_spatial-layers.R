###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Spatial covariates
# Task:    Format spatial covariates, extract covariates for each sampling location
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
###

# Clear the environment
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

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(RNetCDF)
library(rerddap)

# No sample buffer for this report - predictions cover the whole extent of
# the samples, Abrolhos to Eastern Recherche (out of range areas are still
# masked by MESS in 05 and 06)

# Padding (decimal degrees) added around the samples before cropping
e_pad <- 0.02

# TODO Download AusBathyTopo 2024 from https://pid.geoscience.gov.au/dataset/ga/150050
# and save in below folder

# Read in the metadata ----
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# Convert metadata to a spatial file
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Set the extent of the study from the samples
bb <- st_bbox(metadata_sf)

# Predict an extra 2 degrees east and west, 2.5 degrees south and 1 degree north
e <- ext(bb[["xmin"]] - e_pad - 2, bb[["xmax"]] + e_pad + 2,
         bb[["ymin"]] - 2.5, bb[["ymax"]] + 1)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(bathy)

# Create terrain metrics (bathymetry derivatives)
preds <- terrain(bathy, neighbors = 8,
                 v = c("aspect", "roughness"),
                 unit = "degrees")
names(preds) <- c("geoscience_aspect", "geoscience_roughness")

# Create detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "SpatRaster")
names(detre) <- c("geoscience_detrended", "lineartrend")

# Join depth, terrain metrics and detrended bathymetry
preds <- rast(list(bathy, preds, detre[[1]]))
names(preds)[1] <- "geoscience_depth"

# Save the sample points
vector_dir <- paste0("data/", park, "/spatial/shapefiles")
if (!dir.exists(vector_dir)) dir.create(vector_dir, recursive = TRUE)

st_write(metadata_sf,
         file.path(vector_dir, paste0(name, "_samples.gpkg")),
         delete_dsn = TRUE, quiet = TRUE)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(st_geometry(metadata_sf), add = T, pch = 20)

# TODO Copy this into prediction_limits in scripts 07 and 08
message("prediction_limits <- c(",
        paste(round(as.vector(ext(preds)), 4), collapse = ", "), ")")

# Save the bathymetry derivatives ----
# 05, 06, 07 and 08 all read this one file
raster_dir <- paste0("data/", park, "/spatial/rasters")
if (!dir.exists(raster_dir)) dir.create(raster_dir, recursive = TRUE)

saveRDS(preds, file = file.path(raster_dir,
                                paste0(name, "_bathymetry-derivatives.rds")))

# Extract bathymetry derivatives at each of the samples ----
metadata.bathy.derivatives.all <- cbind(metadata,
                                        terra::extract(preds, metadata_sf)) %>%
  dplyr::select(-ID)

# TODO Check this
dropped <- metadata.bathy.derivatives.all %>%
  dplyr::filter(if_any(c(geoscience_depth, geoscience_aspect,
                         geoscience_roughness, geoscience_detrended),
                       ~ is.na(.)))

message("Samples dropped for missing bathymetry derivatives: ", nrow(dropped),
        " of ", nrow(metadata.bathy.derivatives.all))

if (nrow(dropped) > 0) {
  metadata.bathy.derivatives.all %>%
    dplyr::mutate(retained = !sample %in% dropped$sample) %>%
    dplyr::count(campaignid, retained) %>%
    tidyr::pivot_wider(names_from = retained, values_from = n,
                       names_prefix = "retained_", values_fill = 0) %>%
    print(n = Inf)
}

metadata.bathy.derivatives <- metadata.bathy.derivatives.all %>%
  filter(if_all(c(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
                ~!is.na(.))) %>% # TODO Removes samples missing bathymetry derivatives - check these!
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))

