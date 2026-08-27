rm(list = ls())

library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
library(patchwork)

# Load functions
file.sources = list.files(pattern = "*.R", path = "functions/", full.names = T)
sapply(file.sources, source, .GlobalEnv)

# # Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# TODO Set the extent of the study
e <- ext(146.41, 147.58, -39.639, -38.98)

# Read in shapefile data for maps
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

marine_parks <- st_read("data/amp_shapefile/Australian_Marine_Parks_v2.shp") %>%
  dplyr::filter(name %in% c("Beagle", "Kent Group Marine Park")) # TODO select relevant parks
marine_parks <- st_crop(marine_parks, e)

# Spatial plots
## SST
sst <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SST_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sst)
sst <- sst[[c("Jan", "Mar", "May", "Jul", "Sep", "Nov")]]
names(sst)

prediction_limits = c(146.4226, 147.6526, -39.6899, -38.9574)

plot_sst(prediction_limits) +
  theme(axis.text = element_text(size = 6))


ggsave(paste0("plots/", park, "/pressures/", name, "_SST.png"),
       height = 4.5, width = 8, dpi = 600, bg = "white", units = "in")

## SLA
sla <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SLA_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sla)

plot_sla(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/", park, "/pressures/", name, "_SLA.png"),
       height = 4.5, width = 8, dpi = 600, bg = "white", units = "in")

## DHW
dhw <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_DHW_raster.rds"))
names(dhw)

plot_dhw(prediction_limits) +
  theme(axis.text = element_text(size = 6))

ggsave(paste0("plots/", park, "/pressures/", name, "_DHW.png"),
       height = 3.5, width = 8, dpi = 600, bg = "white", units = "in")

pressure_data()

maxyear = c(2011, 2025)
pressure_plot(maxyear)

ggsave(filename = paste0('plots/', park, '/pressures/', name, '_oceanography_time-series.png'),
       dpi = 300, units = "in", bg = "white",
       width = 6, height = 6.75)

