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

# Predictions are limited to within 10 km of any sample. Benthos is scored on
# both BRUV and BOSS, fish on BRUV only, so there are two buffers.
buffer_km <- 10

# TODO Set the projected CRS used to buffer the samples in metres
# 32750 = UTM zone 50S (114-120E), 32751 = UTM zone 51S (120-126E)
buffer_crs <- 32751

# Padding (decimal degrees) added around the buffer before cropping
e_pad <- 0.02

# TODO Download AusBathyTopo 2024 from https://pid.geoscience.gov.au/dataset/ga/150050
# and save in below folder

# Read in the metadata
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, method, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# TODO Check both methods and both years are present before buffering
metadata %>% count(method, year)

# Convert metadata to a spatial file
metadata_sf <- st_as_sf(metadata, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Buffer the samples
make_buffer <- function(x) {
  x %>%
    st_transform(buffer_crs) %>%
    st_buffer(dist = buffer_km * 1000) %>%
    st_union() %>%
    st_transform(4326) %>%
    st_as_sf()
}

# All samples - used for the benthos predictions
sample_buffer <- make_buffer(metadata_sf)

# BRUV only - used for the fish predictions
bruv_buffer <- make_buffer(metadata_sf %>% dplyr::filter(method %in% "BRUV"))

# Set the extent of the study from the larger (all sample) buffer, so both
# masked stacks are cropped from the same bathymetry and stay on the same grid
bb <- st_bbox(sample_buffer)

e <- ext(bb[["xmin"]] - e_pad, bb[["xmax"]] + e_pad,
         bb[["ymin"]] - e_pad, bb[["ymax"]] + e_pad)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(bathy)

# Create terrain metrics (bathymetry derivatives)
preds <- terrain(bathy, neighbors = 8,
                 v = c("roughness"),
                 unit = "degrees")
names(preds) <- "geoscience_roughness"

# Create detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "SpatRaster")
names(detre) <- c("geoscience_detrended", "lineartrend")

# Join depth, terrain metrics and detrended bathymetry
preds <- rast(list(bathy, preds, detre[[1]]))
names(preds)[1] <- "geoscience_depth"

# Mask to each buffer. Derivatives are built before masking so the terrain and
# detrending neighbourhoods are not affected by the buffer edge.
preds_benthos <- preds %>%
  terra::mask(terra::vect(sample_buffer)) %>%
  terra::trim()

preds_fish <- preds %>%
  terra::mask(terra::vect(bruv_buffer)) %>%
  terra::trim()

# Save the buffers and the sample points
vector_dir <- paste0("data/", park, "/spatial/shapefiles")
if (!dir.exists(vector_dir)) dir.create(vector_dir, recursive = TRUE)

st_write(sample_buffer,
         file.path(vector_dir, paste0(name, "_sample-buffer.gpkg")),
         delete_dsn = TRUE, quiet = TRUE)

st_write(bruv_buffer,
         file.path(vector_dir, paste0(name, "_sample-buffer-bruv.gpkg")),
         delete_dsn = TRUE, quiet = TRUE)

st_write(metadata_sf,
         file.path(vector_dir, paste0(name, "_samples.gpkg")),
         delete_dsn = TRUE, quiet = TRUE)

# Check that samples align with bathymetry derivatives
plot(preds_benthos[[1]])
plot(st_geometry(sample_buffer), add = T, border = "red")
plot(st_geometry(metadata_sf), add = T, pch = 20)

plot(preds_fish[[1]])
plot(st_geometry(bruv_buffer), add = T, border = "red")
plot(st_geometry(metadata_sf %>% dplyr::filter(method %in% "BRUV")), add = T, pch = 20)

# TODO Copy these into prediction_limits - benthos into 07, fish into 08
message("07 prediction_limits <- c(",
        paste(round(as.vector(ext(preds_benthos)), 4), collapse = ", "), ")")
message("08 prediction_limits <- c(",
        paste(round(as.vector(ext(preds_fish)), 4), collapse = ", "), ")")

# Save the bathymetry derivatives
saveRDS(preds_benthos, file = paste0("data/", park, "/spatial/rasters/",
                                     name, "_bathymetry-derivatives.rds"))

saveRDS(preds_fish, file = paste0("data/", park, "/spatial/rasters/",
                                  name, "_bathymetry-derivatives-fish.rds"))

# Extract bathymetry derivatives at each of the samples. Taken from the benthos
# stack, which covers every sample - the BRUV samples sit inside both buffers.
metadata.bathy.derivatives   <- cbind(metadata,
                                      terra::extract(preds_benthos, metadata_sf)) %>%
  filter(if_all(c(geoscience_depth, geoscience_roughness, geoscience_detrended),
                ~!is.na(.))) %>% # TODO Removes samples missing bathymetry derivatives - check these!
  dplyr::select(-ID) %>%
  glimpse()

# TODO Compare against the count above - the buffer should not drop any samples
metadata.bathy.derivatives %>% count(method, year)

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))
