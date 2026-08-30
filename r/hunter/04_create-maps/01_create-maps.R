###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park monitoring data syntheses, oceanographic data, marine park boundary files
# Task:    Create pre-modelling figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

# Table of contents
# 1. Overall location plot (including State and Commonwealth Marine Parks)
# 2. Sampling location plot
# 3. Key Ecological Features
# 4. Historical Sea Levels
# 5. Bathymetry cross section

# Clear your environment
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
library(tidyverse)
library(sf)
library(rnaturalearth)
library(metR)
library(patchwork)
library(terra)
library(tidyterra)
library(ggnewscale)
library(CheckEM)
library(geosphere)

# Load functions
file.sources <- list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(152.2,153.1,-32.7,  -32.31)

# Load necessary spatial files
sf_use_s2(T)
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()
ausc <- st_crop(aus, e)

# Load marine parks
# All australian marine parks - for inset plotting
aus_marine_parks <- st_read("data/amp_shapefile/Australian_Marine_Parks_v2.shp")

marine_parks <- st_read("data/amp_shapefile/Australian_Marine_Parks_v2.shp") %>%
  dplyr::filter(name %in% c("Hunter")|
                  NETNAME == "Port Stephens - Great Lakes") %>% # TODO select relevant parks
  glimpse()

# Australian Marine Parks only (for separate ggplot legends)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# State Marine Parks only (for separate ggplot legends)
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State")

# Terrestrial parks
terrnp <- st_read("data/amp_shapefile/capad.shp") %>%  # Terrestrial reserves
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))
plot(terrnp["TYPE"])

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6", # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                name = "Terrestrial Parks")

# Key Ecological Features
# This shapefile has added columns in QGIS for hex colour code and abbreviated names
kef <- st_read("data/amp_shapefile/Marine_Key_Ecological_Features_v2.shp") %>%
  CheckEM::clean_names() %>%
  st_make_valid() %>%
  st_crop(e) %>%
  arrange(desc(area_km2)) %>%
  glimpse()
unique(kef$abbrv)

# Coastal waters limit
cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Bathymetry data
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = F)
names(bathy) <- "Depth"
plot(bathy)

bathdf <- as.data.frame(bathy, xy = T)

# Create marine park colours and fills (scale_fill_manual)
# amp_fills <- amp_marine_park_fills(marine_parks)
# state_fills <- state_marine_park_fills(marine_parks)
# amp_cols <- amp_marine_park_cols(marine_parks)
# state_cols <- state_marine_park_fills(marine_parks)

# 1. Location overview plot
# Set plot inputs
plot_limits <- c(152.2,153.1,-32.7,  -32.31) # TODO Extent of the main plot
study_limits <- c(152.2,153.1,-32.7,  -32.31) # TODO Extent of sampling
annotation_labels <- data.frame(x = c(152.32044, 152.526341), # TODO Labels for annotation e.g. nearby towns
                                y = c(-32.61745,-32.4391539),
                                label = c("Boughton Island", "Seal Rocks"))
# Create plot
location_plot(plot_limits,
              study_limits,
              annotation_labels)
# Save plot
ggsave(paste(paste0('plots/', park, '/spatial/', name) , 'broad-site-plot.png',
             sep = "-"), dpi = 600, width = 8, height = 5, bg = "white")

# 2. Site level overview - with sampling point locations
metadata <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326) %>%
  glimpse()
# Set plot inputs
site_limits <- c(152.27,153.03,-32.67,-32.34) # TODO Plot limits for subsequent plots - tighter zoom
# Create plot
site_plot(site_limits, annotation_labels)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'sampling-locations.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 4)

# 3. Key Ecological Features
# Create plot
kef_plot(plot_limits, annotation_labels)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'key-ecological-features.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 6)

# 4. Historical sea levels
# Set coastline fills
depth_fills <- scale_fill_manual(values = c("#f9ddb1","#ee9f27", "#dc6601"),
                                 labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                 name = "Coastline age")
# Create plot
transect_line <- sfheaders::sf_linestring(
  obj = data.frame(x = c(152.19926, 153.056), y = c(-32.64421, -32.64421), id = 1),
  x = "x", y = "y", linestring_id = "id"
) %>%
  st_set_crs(4326)
sealevel_plot(plot_limits, annotation_labels)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'old-sea-levels.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 6)

# 5. Bathymetry cross sections
# Create data
bath_df1 <- dem_cross_section(152.19926, 152.91733,-32.64421, -32.64421, maxdist = 10) # TODO set coords
# Set plot inputs
crosssection_labels <- data.frame(x = c(50), # TODO Labels for annotation
                                 y = c(-20),
                                 label = c("Shelf Rocky Reef"))

segment_offset <- 5 # Length of the segment
label_offset <- segment_offset + 2 # Distance from end of segment to label
# Create plot
crosssection_plot(crosssection_labels, label_offset, segment_offset)
# Save plot
ggsave(filename = paste(paste0('plots/', park, '/spatial/', name) , 'bathymetry-cross-section.png',
                        sep = "-"), units = "in", dpi = 600,
       bg = "white",
       width = 8, height = 4)
