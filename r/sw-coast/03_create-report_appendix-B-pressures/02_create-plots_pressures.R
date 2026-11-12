rm(list = ls())

library(terra)
library(tidyterra)
library(tidyverse)
library(sf)
library(patchwork)

# # Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# Load functions
file.sources <- list.files(pattern = "*.R", path = paste0("r/", park, "/functions/"), full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)


# TODO Set the extent of the study
e <- ext(110.5, 127.0, -37.0, -25.5)

# Read in shapefile data for maps
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- st_crop(aus, e)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Jurien", "Jurien Bay", "Lancelin Island Lagoon",
                              "Two Rocks", "Marmion", "Perth Canyon", "Rottnest", "Cottesloe Reef",
                              "Shoalwater Islands", "Geographe", "Ngari Capes", "South-west Corner",
                              "Walpole And Nornalup Inlets", "Bremer", "Eastern Recherche")) # TODO select relevant parks
marine_parks <- st_crop(marine_parks, e)

# Spatial plots
## SST
sst <- rast(paste0("data/", park, "/spatial/oceanography/", name, "_SST_raster.rds")) %>%
  subset(names(.) %in% c("Jan", "Mar", "May", "Jul", "Sep", "Nov"))
names(sst)
sst <- sst[[c("Jan", "Mar", "May", "Jul", "Sep", "Nov")]]
names(sst)

prediction_limits = c(113.0, 124.3, -35.3, -27.0) # TODO check

plot_sst(prediction_limits) +
  theme(axis.text = element_text(size = 6))


ggsave(paste0("plots/", park, "/pressures/", name, "_SST.png"),
       height = 4.5, width = 8, dpi = 600, bg = "white", units = "in")

## SST anomaly
sst_anom <- readRDS(paste0("data/", park, "/spatial/oceanography/", name, "_SST_anomaly.rds"))

ggplot(sst_anom, aes(x = year, y = anomaly, fill = anomaly > 0)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("TRUE" = "#D81B60", "FALSE" = "#1E88E5"), guide = "none") +
  labs(x = "Year", y = "SST anomaly (°C)") +
  theme_classic()

ggsave(paste0("plots/", park, "/pressures/", name, "_SST_anomaly.png"),
       height = 3.5, width = 8, dpi = 600, bg = "white", units = "in")

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

