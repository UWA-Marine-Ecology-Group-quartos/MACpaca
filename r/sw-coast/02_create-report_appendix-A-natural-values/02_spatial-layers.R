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

# Oceanography ----
# Long-term mean of each layer, coastal gaps filled, then resampled onto the
# bathymetry grid (250 m) so every layer has exactly the same cells
ocean_dir <- paste0("data/", park, "/spatial/oceanography/")

# Fill NA cells along the coast with the mean of their neighbours
fill_coast <- function(r, n = 10) {
  for (i in 1:n) {
    r <- terra::focal(r, w = 3, fun = "mean", na.policy = "only", na.rm = TRUE)
  }
  r
}

# Only keep years with data in all 12 months (same as the other scripts)
full_years <- function(dates) {
  n_months <- tapply(lubridate::month(dates), lubridate::year(dates),
                     function(m) length(unique(m)))
  keep <- lubridate::year(dates) %in% as.numeric(names(n_months)[n_months == 12])
  message("Years kept: ", paste(unique(lubridate::year(dates[keep])), collapse = ", "))
  keep
}

to_bathy_grid <- function(r) {
  terra::crs(r) <- "EPSG:4326"
  r %>%
    fill_coast() %>%
    terra::resample(bathy, method = "bilinear") %>%
    terra::mask(bathy)
}

# SST - monthly, Kelvin to Celsius
nc_sst  <- open.nc(paste0(ocean_dir, "SST.nc"))
sst_var <- var.get.nc(nc_sst, "sea_surface_temperature")   # [lon, lat, time]
lat     <- var.get.nc(nc_sst, "lat")
lon     <- var.get.nc(nc_sst, "lon")
sst_dates <- as.Date(utcal.nc("seconds since 1981-01-01 00:00:00",
                              var.get.nc(nc_sst, "time"), type = "c"))
close.nc(nc_sst)

sst_var  <- sst_var[, , full_years(sst_dates), drop = FALSE]
sst_mean <- rowMeans(sst_var, dims = 2, na.rm = TRUE) - 273.15
sst <- terra::rast(t(sst_mean),
                   extent = terra::ext(min(lon), max(lon), min(lat), max(lat)))
rm(sst_var)

# SLA and currents (eastward = UCUR, northward = VCUR)
nc_sla    <- open.nc(paste0(ocean_dir, "SLA.nc"))
sla_dates <- as.Date(utcal.nc("days since 1985-01-01 00:00:00",
                              var.get.nc(nc_sla, "TIME"), type = "c"))
close.nc(nc_sla)
sla_keep <- full_years(sla_dates)

sla  <- mean(terra::rast(paste0(ocean_dir, "SLA.nc"), subds = "GSLA")[[sla_keep]], na.rm = TRUE)
ucur <- mean(terra::rast(paste0(ocean_dir, "SLA.nc"), subds = "UCUR")[[sla_keep]], na.rm = TRUE)
vcur <- mean(terra::rast(paste0(ocean_dir, "SLA.nc"), subds = "VCUR")[[sla_keep]], na.rm = TRUE)

# Primary productivity - both models, FSS picks between them in 05 and 06
# WARNING: these are large files and the mean can take a while
prod_dates <- function(file) {
  nc <- open.nc(paste0(ocean_dir, file))
  d  <- as.Date(utcal.nc("days since 1990-01-01 00:00:00",
                         var.get.nc(nc, "time"), type = "c"))
  close.nc(nc)
  d
}

prod_gsm <- mean(terra::rast(paste0(ocean_dir, "PROD_GSM.nc"),
                             subds = "npp_vgpm_eppley_gsm")[[full_years(prod_dates("PROD_GSM.nc"))]],
                 na.rm = TRUE)
prod_oc3 <- mean(terra::rast(paste0(ocean_dir, "PROD_OC3.nc"),
                             subds = "npp_vgpm_eppley_oc3")[[full_years(prod_dates("PROD_OC3.nc"))]],
                 na.rm = TRUE)

# TODO Check these are the right way up (north at the top) before moving on
plot(c(sla, ucur, vcur))
plot(c(prod_gsm, prod_oc3))
plot(sst)

ocean <- c(to_bathy_grid(sst),
           to_bathy_grid(sla),
           to_bathy_grid(ucur),
           to_bathy_grid(vcur),
           to_bathy_grid(prod_gsm),
           to_bathy_grid(prod_oc3))
names(ocean) <- c("ocean_sst", "ocean_sla", "ocean_ucur", "ocean_vcur",
                  "ocean_prod_gsm", "ocean_prod_oc3")
plot(ocean)

preds <- c(preds, ocean)

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
  dplyr::filter(if_any(any_of(names(preds)), ~ is.na(.)))

message("Samples dropped for missing covariates: ", nrow(dropped),
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
  filter(if_all(any_of(names(preds)), ~!is.na(.))) %>% # TODO Removes samples missing any covariate - check these!
  glimpse()

# Save the metadata bathymetry derivatives
saveRDS(metadata.bathy.derivatives, paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds"))

