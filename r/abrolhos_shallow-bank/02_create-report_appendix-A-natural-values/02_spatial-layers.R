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

# TODO Set the extent of the study
e <- ext(113, 113.7, -28.3, -27.8)

# TODO Download AusBathyTopo 2024 from https://pid.geoscience.gov.au/dataset/ga/150050
# and save in below folder

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

# Save the bathymetry derivatives
saveRDS(preds, file = paste0("data/", park, "/spatial/rasters/",
                      name, "_bathymetry-derivatives.rds"))

# Read in the metadata
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, method, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# Convert metadata to a spatial file and check alignment with bathymetry
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Check that samples align with bathymetry derivatives
plot(preds[[1]])
plot(metadata_sf, add = T)

# Extract bathymetry derivatives at each of the samples
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds, metadata_sf)) %>%
  filter(if_all(c(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
                ~!is.na(.))) %>% # TODO Removes samples missing bathymetry derivatives - check these!
  dplyr::select(-ID) %>%
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))
