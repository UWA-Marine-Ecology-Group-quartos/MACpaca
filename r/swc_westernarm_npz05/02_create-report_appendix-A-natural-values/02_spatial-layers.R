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

# National Park Zone ----
# Samples and predictions are both restricted to the Commonwealth NPZ. Held as
# an sfc (not sf) so it can be used directly in st_filter() and
# st_intersection() without colliding with the buffer's own columns.
# TODO Check the shapefile path matches the network you are working in
npz <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(epbc %in% "Commonwealth",
                zone %in% "National Park Zone") %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_union()

if (length(npz) == 0) {
  stop("No National Park Zone polygons returned - check the `zone` labels in ",
       "the marine parks shapefile.")
}

# Keep only samples that fall inside the NPZ
in_npz <- function(df) {
  df %>%
    st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
    st_filter(npz, .predicate = st_within) %>%
    st_drop_geometry()
}

# Read in the metadata ----
metadata_bruv_all <- readRDS(paste0("data/", park, "/raw/bruv_metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd)

metadata_all <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::select(campaignid, sample, longitude_dd, latitude_dd, status, year)

metadata_bruv <- in_npz(metadata_bruv_all) %>%
  glimpse()

metadata <- in_npz(metadata_all) %>%
  glimpse()

# TODO Check how many samples survived the NPZ filter
message("Samples inside the NPZ: ", nrow(metadata), " of ", nrow(metadata_all),
        " | BRUVs inside the NPZ: ", nrow(metadata_bruv), " of ",
        nrow(metadata_bruv_all))

if (nrow(metadata) == 0 || nrow(metadata_bruv) == 0) {
  stop("No samples fall inside the NPZ - check the shapefile and the sample ",
       "coordinates before going any further.")
}

# TODO Every NPZ sample should come back No-take. Any model term or summary that
# contrasts status has only one level to work with from here on.
message("Status levels retained: ",
        paste(sort(unique(as.character(metadata$status))), collapse = ", "))

# Samples with habitat data ----
habitat_samples <- readRDS(paste0("data/", park, "/raw/", name, "_benthos.RDS")) %>%
  dplyr::distinct(campaignid, sample)

metadata_habitat      <- dplyr::semi_join(metadata,      habitat_samples,
                                          by = c("campaignid", "sample"))
metadata_bruv_habitat <- dplyr::semi_join(metadata_bruv, habitat_samples,
                                          by = c("campaignid", "sample"))

# TODO Check these counts
message("Samples with habitat data: ", nrow(metadata_habitat), " of ", nrow(metadata),
        " | BRUVs with habitat data: ", nrow(metadata_bruv_habitat), " of ",
        nrow(metadata_bruv))

if (nrow(metadata_habitat) == 0 || nrow(metadata_bruv_habitat) == 0) {
  stop("No samples matched the habitat data - check campaignid and sample in ",
       name, "_benthos.RDS against metadata.RDS.")
}

# Convert metadata to spatial files
metadata_bruv_habitat_sf <- st_as_sf(metadata_bruv_habitat, coords = c("longitude_dd", "latitude_dd"), crs = 4326)
metadata_habitat_sf      <- st_as_sf(metadata_habitat,      coords = c("longitude_dd", "latitude_dd"), crs = 4326)
metadata_sf              <- st_as_sf(metadata,              coords = c("longitude_dd", "latitude_dd"), crs = 4326)

# Buffer the samples ----
# make_buffer() is the same operation both times, so the two extents cannot
# drift apart if buffer_km or buffer_crs is changed later
make_buffer <- function(x) {
  x %>%
    st_transform(buffer_crs) %>%
    st_buffer(dist = buffer_km * 1000) %>%
    st_union() %>%
    st_transform(4326) %>%
    st_as_sf() %>%
    # Clip to the NPZ - the 10 km buffer only ever removes area from the zone,
    # it never extends the prediction surface beyond it
    st_intersection(npz) %>%
    st_make_valid()
}

sample_buffer <- make_buffer(metadata_habitat_sf)       # benthos - habitat samples
bruv_buffer   <- make_buffer(metadata_bruv_habitat_sf)  # fish    - BRUV habitat samples
extent_buffer <- make_buffer(metadata_sf)               # extent  - every sample

# Set the extent of the study from the WIDER of the two buffers, so the full
# 10 km is retained around every sample where the NPZ boundary allows it.
# bruv_buffer is a subset of this, so the bathymetry only has to be processed
# once.
bb <- st_bbox(extent_buffer)

e <- ext(bb[["xmin"]] - e_pad, bb[["xmax"]] + e_pad,
         bb[["ymin"]] - e_pad, bb[["ymax"]] + e_pad)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = F) %>%
  trim()
plot(bathy)
list.files("data/south-west network/spatial/rasters")
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

