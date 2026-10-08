###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Western rock lobster pot synthesis, Abrolhos (Yamatji Shallow Bank)
# Task:    Bubble plot, size histogram and metric plots for Appendix A
# Author:
# Date:    October 2026
###
rm(list = ls())

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park
maturity_mm <- config$maturity_mm
large_mm    <- config$large_mm

library(tidyverse)
library(sf)
library(terra)

plot_dir <- paste0("plots/", park)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

status_colours <- c("No-Take" = "#7bbc63",
                    "Fished"  = "#b9e6fb")

zone_colours <- c("National Park Zone"   = "#7bbc63",
                  "Special Purpose Zone" = "#6daff4")

metric_labels <- c(all           = "All lobster",
                   sub_maturity  = paste0("Sub-maturity (<", maturity_mm, " mm CL)"),
                   mature_female = paste0("Female ≥", maturity_mm, " mm CL"),
                   mature_male   = paste0("Male ≥", maturity_mm, " mm CL"),
                   large_female  = paste0("Female ≥", large_mm, " mm CL"),
                   large_male    = paste0("Male ≥", large_mm, " mm CL"))

metrics <- readRDS(paste0("data/", park, "/tidy/", name, "_lobster-metrics.rds")) %>%
  dplyr::mutate(status = factor(status, levels = names(status_colours)))

lobsters <- readRDS(paste0("data/", park, "/tidy/", name, "_lobster-lengths.rds")) %>%
  dplyr::mutate(status = factor(status, levels = names(status_colours)))

# Spatial bubble plot ----
pots <- dplyr::filter(metrics, response %in% "all")

pad <- 0.03
map_ext <- c(min(pots$longitude_dd) - pad, max(pots$longitude_dd) + pad,
             min(pots$latitude_dd)  - pad, max(pots$latitude_dd)  + pad)

parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp",
                 quiet = TRUE) %>%
  dplyr::filter(name %in% "Abrolhos", epbc %in% "Commonwealth") %>%
  st_transform(4326)

parks_cropped <- suppressWarnings(
  st_crop(parks, st_bbox(c(xmin = map_ext[1], xmax = map_ext[2],
                           ymin = map_ext[3], ymax = map_ext[4]),
                         crs = st_crs(4326)))
)

bathy <- readRDS("data/abrolhos/spatial/rasters/abrolhosAMP_bathymetry-derivatives.rds")
if (inherits(bathy, "PackedSpatRaster")) bathy <- terra::unwrap(bathy)

depth_contours <- bathy[["geoscience_depth"]] %>%
  terra::crop(terra::ext(map_ext)) %>%
  terra::as.contour(levels = seq(-70, -30, by = 10)) %>%
  st_as_sf()

p_bubble <- ggplot() +
  geom_sf(data = parks_cropped, aes(fill = zone), colour = NA, alpha = 0.35) +
  geom_sf(data = depth_contours, colour = "grey60", linewidth = 0.2) +
  geom_point(data = dplyr::filter(pots, count == 0),
             aes(x = longitude_dd, y = latitude_dd),
             shape = 4, colour = "grey40", size = 1.2, stroke = 0.5) +
  geom_point(data = dplyr::filter(pots, count > 0),
             aes(x = longitude_dd, y = latitude_dd, size = count),
             shape = 21, fill = "#d95f02", colour = "black", alpha = 0.7, stroke = 0.3) +
  scale_size_area(max_size = 9, name = "Lobster\nper pot") +
  scale_fill_manual(values = zone_colours, name = "Zone", na.value = "grey80") +
  coord_sf(xlim = map_ext[1:2], ylim = map_ext[3:4], crs = 4326, expand = FALSE) +
  facet_wrap(~year, nrow = 1) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text  = element_text(size = 7),
        strip.text = element_text(size = 9, face = "bold"))
p_bubble

saveRDS(p_bubble, paste0(plot_dir, "/", name, "_bubble-plot.rds"))

# Size histogram by status and year ----
panel_n <- dplyr::count(lobsters, year, status)

p_hist <- ggplot(lobsters, aes(x = length_mm, fill = status)) +
  geom_histogram(binwidth = 5, boundary = 0, colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = maturity_mm, colour = "red", linetype = "dashed") +
  geom_text(data = panel_n, aes(x = Inf, y = Inf, label = paste0("n = ", n)),
            hjust = 1.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = status_colours) +
  facet_grid(year ~ status) +
  labs(x = "Carapace length (mm)", y = "Number of lobster") +
  theme_classic() +
  theme(legend.position  = "none",
        strip.background = element_blank(),
        strip.text       = element_text(face = "bold"))
p_hist

saveRDS(p_hist, paste0(plot_dir, "/", name, "_size-histogram.rds"))

# Metric plots by status and year ----
metric_summary <- metrics %>%
  dplyr::group_by(response, year, status) %>%
  dplyr::summarise(mean = mean(count),
                   se   = sd(count) / sqrt(n()),
                   .groups = "drop")

metric_plot <- function(responses) {
  dat <- metric_summary %>%
    dplyr::filter(response %in% responses) %>%
    dplyr::mutate(response = factor(metric_labels[response],
                                    levels = metric_labels[responses]))

  ggplot(dat, aes(x = factor(year), y = mean, fill = status)) +
    geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                  width = 0.3, position = position_dodge(0.5)) +
    geom_point(shape = 21, size = 3, stroke = 0.2, colour = "black", alpha = 0.8,
               position = position_dodge(0.5)) +
    scale_fill_manual(values = status_colours, name = "Status") +
    coord_cartesian(ylim = c(0, NA)) +
    facet_wrap(~response, nrow = 1) +
    labs(x = "Year", y = "Lobster per pot (mean ± SE)") +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text       = element_text(face = "bold"))
}

p_all <- metric_plot("all")
p_all
saveRDS(p_all, paste0(plot_dir, "/", name, "_metric-plot_all.rds"))

p_sub <- metric_plot("sub_maturity")
p_sub
saveRDS(p_sub, paste0(plot_dir, "/", name, "_metric-plot_sub-maturity.rds"))

p_mature <- metric_plot(c("mature_female", "mature_male"))
p_mature
saveRDS(p_mature, paste0(plot_dir, "/", name, "_metric-plot_mature-by-sex.rds"))

p_large <- metric_plot(c("large_female", "large_male"))
p_large
saveRDS(p_large, paste0(plot_dir, "/", name, "_metric-plot_large-by-sex.rds"))
