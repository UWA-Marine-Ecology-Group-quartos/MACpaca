rm(list = ls())

library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
library(patchwork)

# Load functions
file.sources = list.files(pattern = "*.R", path = paste0("r/", "ningaloo", "/functions/"), full.names = T)
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

# Extent of the study - matches the extent used elsewhere for ningaloo (e.g.
# 09_threatened-species.R in the Appendix A1 folder)
e <- ext(113.2, 114.4, -23.6, -21.4)

# Read in shapefile data for maps
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

marine_parks <- st_read("data/north-west network/spatial/shapefiles/north-west-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% "Ningaloo")
marine_parks <- st_crop(marine_parks, e)

# Spatial plots
## SST
sst <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SST_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sst)
sst <- sst[[c("Jan", "Mar", "May", "Jul", "Sep", "Nov")]]
names(sst)

prediction_limits = c(113.2, 114.4, -23.6, -21.4)

p_sst <- plot_sst(prediction_limits) +
  theme(axis.text = element_text(size = 6))
p_sst

ggsave(paste0("plots/", park, "/pressures/", name, "_SST.png"),
       plot = p_sst,
       height = 4.5, width = 8, dpi = 600, bg = "white", units = "in")

## SLA
sla <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SLA_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sla)

p_sla <- plot_sla(prediction_limits) +
  theme(axis.text = element_text(size = 6))
p_sla

ggsave(paste0("plots/", park, "/pressures/", name, "_SLA.png"),
       plot = p_sla,
       height = 4.5, width = 8, dpi = 600, bg = "white", units = "in")

## DHW
dhw <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_DHW_raster.rds"))
names(dhw)

p_dhw <- plot_dhw(prediction_limits) +
  theme(axis.text = element_text(size = 6))
p_dhw

ggsave(paste0("plots/", park, "/pressures/", name, "_DHW.png"),
       plot = p_dhw,
       height = 3.5, width = 8, dpi = 600, bg = "white", units = "in")

pressure_data()

maxyear = c(2011, 2025) # TODO check against Ningaloo's own highest DHW periods (see the TODO in 01_spatial-layers.R)
p_pressure <- pressure_plot(maxyear)
p_pressure

ggsave(filename = paste0('plots/', park, '/pressures/', name, '_oceanography_time-series.png'),
       plot = p_pressure,
       dpi = 300, units = "in", bg = "white",
       width = 6, height = 6.75)

