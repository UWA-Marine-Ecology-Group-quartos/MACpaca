###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Maps of pot locations and bubble plots of catch by year
# Author:  Henry Evans
# Date:    August 2026
###

rm(list = ls())

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
name <- config$name

tidy_dir <- "data/abrolhos/tidy/lobster"
plot_dir <- "plots/abrolhos/lobster"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

zone_colours <- c("National Park Zone"   = "#7bbc63",
                  "Special Purpose Zone" = "#6daff4")

catch_per_pot <- read.csv(file.path(tidy_dir, paste0(name, "_lobster-catch-per-pot.csv")),
                          colClasses = c(pot_number = "character"))

# Spatial layers ---------------------------------------------------------------

parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp",
                 quiet = TRUE) %>%
  dplyr::filter(name %in% "Abrolhos", epbc %in% "Commonwealth") %>%
  st_transform(4326)

# Pad the survey extent so the zone boundaries are visible around the pots
survey_bbox <- catch_per_pot %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_bbox() %>%
  as.list()

pad <- 0.03
map_ext <- c(xmin = survey_bbox$xmin - pad, xmax = survey_bbox$xmax + pad,
             ymin = survey_bbox$ymin - pad, ymax = survey_bbox$ymax + pad)

bathy <- readRDS("data/abrolhos/spatial/rasters/abrolhosAMP_bathymetry-derivatives.rds")
if (inherits(bathy, "PackedSpatRaster")) bathy <- terra::unwrap(bathy)

depth <- bathy[["geoscience_depth"]] %>%
  terra::crop(terra::ext(map_ext[["xmin"]], map_ext[["xmax"]],
                         map_ext[["ymin"]], map_ext[["ymax"]]))

depth_contours <- depth %>%
  terra::as.contour(levels = seq(-70, -30, by = 10)) %>%
  st_as_sf()

parks_cropped <- suppressWarnings(
  st_crop(parks, st_bbox(c(xmin = map_ext[["xmin"]], xmax = map_ext[["xmax"]],
                           ymin = map_ext[["ymin"]], ymax = map_ext[["ymax"]]),
                         crs = st_crs(4326)))
)

base_map <- list(
  geom_sf(data = parks_cropped, aes(fill = zone), colour = NA, alpha = 0.35),
  geom_sf(data = depth_contours, colour = "grey60", linewidth = 0.2),
  scale_fill_manual(values = zone_colours, name = "Zone"),
  coord_sf(xlim = c(map_ext[["xmin"]], map_ext[["xmax"]]),
           ylim = c(map_ext[["ymin"]], map_ext[["ymax"]])),
  labs(x = NULL, y = NULL),
  theme_minimal(),
  theme(panel.grid = element_line(colour = "grey92"))
)

# Pot locations by year --------------------------------------------------------

ggplot() +
  base_map +
  geom_point(data = catch_per_pot, aes(x = longitude, y = latitude),
             shape = 21, fill = "white", colour = "black", size = 1.6, stroke = 0.4) +
  facet_wrap(~ year) +
  labs(title = "Lobster pot locations, Abrolhos Marine Park",
       caption = "Grey lines show 10 m depth contours")

ggsave(file.path(plot_dir, paste0(name, "_lobster-pot-locations_year.png")),
       height = 6, width = 10, dpi = 300, bg = "white")

# Bubble plot of abundance by year ---------------------------------------------

# Pots that caught nothing are drawn as small crosses so that effort with zero
# catch stays visible rather than disappearing from the map
zero_catch <- dplyr::filter(catch_per_pot, n_total %in% 0)
with_catch <- dplyr::filter(catch_per_pot, n_total > 0)

ggplot() +
  base_map +
  geom_point(data = zero_catch, aes(x = longitude, y = latitude),
             shape = 4, colour = "grey40", size = 1.2, stroke = 0.5) +
  geom_point(data = with_catch, aes(x = longitude, y = latitude, size = n_total),
             shape = 21, fill = "#d95f02", colour = "black", alpha = 0.7, stroke = 0.3) +
  scale_size_area(max_size = 9, breaks = c(1, 5, 10, 20, 30),
                  name = "Lobsters\nper pot") +
  facet_wrap(~ year) +
  labs(title = "Western rock lobster abundance per pot",
       caption = "Crosses are pots that caught no lobsters")

ggsave(file.path(plot_dir, paste0(name, "_lobster-abundance-bubble_year.png")),
       height = 6, width = 10, dpi = 300, bg = "white")

# Bubble plot of legal sized lobsters ------------------------------------------

legal_zero <- dplyr::filter(catch_per_pot, n_legal %in% 0)
legal_catch <- dplyr::filter(catch_per_pot, n_legal > 0)

ggplot() +
  base_map +
  geom_point(data = legal_zero, aes(x = longitude, y = latitude),
             shape = 4, colour = "grey40", size = 1.2, stroke = 0.5) +
  geom_point(data = legal_catch, aes(x = longitude, y = latitude, size = n_legal),
             shape = 21, fill = "#7570b3", colour = "black", alpha = 0.7, stroke = 0.3) +
  scale_size_area(max_size = 9, breaks = c(1, 5, 10, 20),
                  name = "Legal sized\nper pot") +
  facet_wrap(~ year) +
  labs(title = "Legal sized western rock lobster per pot",
       caption = "Crosses are pots that caught no legal sized lobsters")

ggsave(file.path(plot_dir, paste0(name, "_lobster-legal-bubble_year.png")),
       height = 6, width = 10, dpi = 300, bg = "white")

message("Spatial plots written to ", plot_dir)
