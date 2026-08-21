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

# Predictions are limited to within 10 km of any sample
buffer_km <- 10

# TODO Set the projected CRS used to buffer the samples in metres
# 32750 = UTM zone 50S (114-120E), 32751 = UTM zone 51S (120-126E)
# The western arm BRUV footprint is 114.54-114.98E, so zone 50S
buffer_crs <- 32750

# Padding (decimal degrees) added around the buffer before cropping
e_pad <- 0.02

# TODO Download AusBathyTopo 2024 from https://pid.geoscience.gov.au/dataset/ga/150050
# and save in below folder

# Read in the metadata ----
# Two objects are needed because benthos and fish are predicted over different
# areas. Benthos pools BRUV and BOSS, so it can be predicted anywhere within
# 10 km of ANY sample. Fish comes from the BRUVs only, so it must be held to
# within 10 km of a BRUV - synthesis 87 spans the whole south-west and reaches
# well past the BRUV footprint at Augusta and off the west coast.
#
# Note bruv_metadata.RDS is written early in 01, before `year` is derived from
# date_time and before status is converted to a factor, so it does not carry a
# `year` column. Only the coordinates are needed here to build the buffer.
metadata_bruv <- readRDS(paste0("data/", park, "/raw/bruv_metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd) %>%
  glimpse()

metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year) %>%
  glimpse()

# Convert metadata to spatial files
metadata_bruv_sf <- st_as_sf(metadata_bruv, coords = c("longitude_dd", "latitude_dd"), crs = 4326)
metadata_sf      <- st_as_sf(metadata,      coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Buffer the samples ----
# make_buffer() is the same operation both times, so the two extents cannot
# drift apart if buffer_km or buffer_crs is changed later
make_buffer <- function(x) {
  x %>%
    st_transform(buffer_crs) %>%
    st_buffer(dist = buffer_km * 1000) %>%
    st_union() %>%
    st_transform(4326) %>%
    st_as_sf()
}

sample_buffer <- make_buffer(metadata_sf)       # benthos - BRUV + BOSS
bruv_buffer   <- make_buffer(metadata_bruv_sf)  # fish    - BRUV only

# Set the extent of the study from the WIDER of the two buffers, so the full
# 10 km is retained around every sample. bruv_buffer is a subset of this, so
# the bathymetry only has to be processed once.
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

# Mask to each buffer, dropping the pad ----
# Both stacks hold the SAME covariate values - the terrain metrics are computed
# once, above, before either mask is applied. Only the coverage differs.
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
         file.path(vector_dir, paste0(name, "_sample-buffer-fish.gpkg")),
         delete_dsn = TRUE, quiet = TRUE)

st_write(metadata_sf,
         file.path(vector_dir, paste0(name, "_samples.gpkg")),
         delete_dsn = TRUE, quiet = TRUE)

# Check that samples align with bathymetry derivatives
plot(preds_benthos[[1]])
plot(st_geometry(sample_buffer), add = T, border = "red")
plot(st_geometry(bruv_buffer),   add = T, border = "blue")
plot(st_geometry(metadata_sf),   add = T, pch = 20)

# TODO Copy these into prediction_limits in scripts 07 and 08. They will differ -
# the fish limits should be the smaller of the two.
message("07 (benthos) prediction_limits <- c(",
        paste(round(as.vector(ext(preds_benthos)), 4), collapse = ", "), ")")
message("08 (fish)    prediction_limits <- c(",
        paste(round(as.vector(ext(preds_fish)), 4), collapse = ", "), ")")

# Save the bathymetry derivatives ----
# 05 and 07 read the benthos version, 06 and 08 read the fish version
raster_dir <- paste0("data/", park, "/spatial/rasters")
if (!dir.exists(raster_dir)) dir.create(raster_dir, recursive = TRUE)

saveRDS(preds_benthos, file = file.path(raster_dir,
                                        paste0(name, "_bathymetry-derivatives.rds")))
saveRDS(preds_fish,    file = file.path(raster_dir,
                                        paste0(name, "_bathymetry-derivatives-fish.rds")))

# Extract bathymetry derivatives at each of the samples ----
# Extracted from the BENTHOS stack, which is the one that covers every sample.
# The covariate values at a BRUV are identical in either stack; using the fish
# stack here would return NA for any BOSS drop outside the BRUV buffer and
# silently drop it from the benthos models in 05.
metadata.bathy.derivatives.all <- cbind(metadata,
                                        terra::extract(preds_benthos, metadata_sf)) %>%
  dplyr::select(-ID)

# TODO Check this report before moving on. Samples listed here are outside the
# benthos buffer (or on land / deeper than the 250 m clamp) and will not be
# modelled at all. Since the buffer is built from these same samples, this
# should be a small number - anything large means a coordinate problem.
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

# TODO Separately, check how many samples sit inside the FISH buffer. These are
# not dropped here - fish metrics are built from BRUVs in 04 - but this is the
# number that tells you how much smaller the fish prediction surface is.
in_fish_buffer <- lengths(st_intersects(metadata_sf, bruv_buffer)) > 0
message("Samples inside the fish (BRUV-only) buffer: ", sum(in_fish_buffer),
        " of ", nrow(metadata_sf))

metadata.bathy.derivatives <- metadata.bathy.derivatives.all %>%
  filter(if_all(c(geoscience_depth, geoscience_aspect, geoscience_roughness, geoscience_detrended),
                ~!is.na(.))) %>% # TODO Removes samples missing bathymetry derivatives - check these!
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))

