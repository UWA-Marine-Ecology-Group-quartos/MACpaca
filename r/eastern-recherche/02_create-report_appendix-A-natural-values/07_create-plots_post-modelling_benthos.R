###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Habitat data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling habitat figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
###

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

# Load libraries ----
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)
library(scatterpie)
library(CheckEM)
library(grid)
library(viridis)
library(geos)

# Load functions ----
file.sources <- list.files(pattern = "*.R", paste0("r/", park, "/functions/"), full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(123.1, 124.0, -34.7, -33.9)

# Load necessary spatial files ----
ausc <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

npz    <- marine_parks[marine_parks$zone %in% "National Park Zone", ]
wasanc <- marine_parks[marine_parks$zone %in% "Sanctuary Zone", ]

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

cwatr_offset <- st_as_sf(geos_offset_curve(as_geos_geometry(cwatr), distance = 0.003))
st_crs(cwatr_offset) <- 4326

# Load the bathymetry data (GA 250m resolution) ----
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

# Map pretty habitat names to raster layer prefixes in dat ----
habitat_lookup <- c(
  "Sand"                  = "sand",
  "Macroalgae"            = "macro",
  "Sessile invertebrates" = "inverts"
)

# Habitat colours ----
hab_cols <- c(
  "Sand"                  = "wheat",
  "Macroalgae"            = "darkgoldenrod4",
  "Sessile invertebrates" = "plum"
)

# TODO Plot extent
prediction_limits <- c(123.1, 124.0, -34.7, -33.9)

# Read single pooled prediction raster ----
dat <- readRDS(
  paste0("output/model-output/", park, "/habitat/", name, "_predicted-habitat.rds")
)

# =============================================================================
# PLOTTING FUNCTIONS
# =============================================================================

dominantbenthos_plot <- function(pred_plot, prediction_limits) {

  ggplot() +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
    scale_fill_gradient(
      low = "white", high = "wheat",
      name = "Sand",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
    scale_fill_gradient(
      low = "white", high = "darkorange4",
      name = "Macroalgae",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none") +
    scale_fill_gradient(
      low = "white", high = "deeppink3",
      name = "Sessile\ninvertebrates",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour = "black",
      breaks = c(-30, -70, -200),
      linewidth = 0.1
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2) +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill = NA,
      show.legend = FALSE,
      linewidth = 0.6
    ) +
    geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6) +
    scale_colour_manual(
      name = "Australian Marine Parks",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs = 4326,
      expand = FALSE
    ) +
    labs(x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
      panel.grid.minor = element_blank(),
      legend.title      = element_text(size = 9),
      legend.text       = element_text(size = 8),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width  = unit(0.5, "cm"),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )
}

dominantbenthos_plot_single <- function(pred_plot, prediction_limits) {

  ggplot() +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = p_sand.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
    scale_fill_gradient(
      low = "white", high = "wheat",
      name = "Sand",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macro.alpha, alpha = p_macro.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
    scale_fill_gradient(
      low = "white", high = "darkorange4",
      name = "Macroalgae",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    new_scale_fill() +
    new_scale("alpha") +
    geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = p_inverts.fit)) +
    scale_alpha_continuous(range = c(0, 1), guide = "none") +
    scale_fill_gradient(
      low = "white", high = "deeppink3",
      name = "Sessile\ninvertebrates",
      na.value = "transparent",
      breaks = c(0, 0.5, 1),
      labels = c("0", "0.5", "1")
    ) +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour = "black",
      breaks = c(-30, -70, -200),
      linewidth = 0.1
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2) +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill = NA,
      show.legend = FALSE,
      linewidth = 0.6
    ) +
    geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6) +
    scale_colour_manual(
      name = "Australian Marine Parks",
      values = with(marine_parks_amp, setNames(colour, zone))
    ) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs = 4326,
      expand = FALSE
    ) +
    labs(x = NULL, y = NULL, colour = NULL) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_text(size = 8),
      axis.ticks = element_line(linewidth = 0.2),
      panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
      panel.grid.minor = element_blank(),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      legend.key.height = unit(0.6, "cm"),
      legend.key.width = unit(0.6, "cm"),
      plot.margin = margin(2, 2, 2, 2, unit = "mm")
    )
}

categoricalhabitat_plot <- function(pred_plot, prediction_limits) {

  pred_cat <- pred_plot %>%
    dplyr::mutate(
      dom_tag = as.character(dom_tag),
      dom_tag = dplyr::case_when(
        dom_tag %in% c("sand", "Sand") ~ "Sand",
        dom_tag %in% c("macro", "macroalgae", "Macroalgae") ~ "Macroalgae",
        dom_tag %in% c("sessile invertebrates", "Sessile Invertebrates", "inverts", "Inverts") ~ "Sessile invertebrates",
        TRUE ~ dom_tag
      ),
      dom_tag = factor(
        dom_tag,
        levels = c("Sessile invertebrates", "Macroalgae", "Sand")
      )
    )

  ggplot() +
    geom_tile(data = pred_cat, aes(x = x, y = y, fill = dom_tag)) +
    scale_fill_manual(
      name = "Habitat",
      limits = c("Sessile invertebrates", "Macroalgae", "Sand"),
      values = c(
        "Sessile invertebrates" = "plum",
        "Macroalgae"            = "darkgoldenrod4",
        "Sand"                  = "wheat"
      ),
      na.value = "transparent",
      drop = FALSE
    ) +
    labs(x = NULL, y = NULL, fill = NULL) +
    new_scale_color() +
    geom_contour(
      data = bathy,
      aes(x = x, y = y, z = Depth),
      colour = "black",
      breaks = c(-30, -70, -200),
      linewidth = 0.2
    ) +
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
    geom_sf(
      data = marine_parks_amp,
      aes(colour = zone),
      fill = NA,
      linewidth = 1.2,
      show.legend = FALSE
    ) +
    scale_colour_manual(values = with(marine_parks_amp, setNames(colour, zone))) +
    new_scale_color() +
    geom_sf(
      data = wasanc,
      colour = "#bfd054",
      fill = NA,
      linewidth = 0.7,
      show.legend = FALSE
    ) +
    new_scale_color() +
    geom_sf(data = cwatr, colour = "red", linewidth = 0.9) +
    coord_sf(
      xlim = c(prediction_limits[1], prediction_limits[2]),
      ylim = c(prediction_limits[3], prediction_limits[4]),
      crs = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.text = element_text(size = 6),
      legend.title = element_blank()
    )
}

normalise_se <- function(data) {
  if ("p_sand.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_sand.alpha = 1 - (p_sand.se.fit - min(p_sand.se.fit, na.rm = TRUE)) /
                      (max(p_sand.se.fit, na.rm = TRUE) - min(p_sand.se.fit, na.rm = TRUE)))
  }
  if ("p_macro.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_macro.alpha = 1 - (p_macro.se.fit - min(p_macro.se.fit, na.rm = TRUE)) /
                      (max(p_macro.se.fit, na.rm = TRUE) - min(p_macro.se.fit, na.rm = TRUE)))
  }
  if ("p_inverts.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_inverts.alpha = 1 - (p_inverts.se.fit - min(p_inverts.se.fit, na.rm = TRUE)) /
                      (max(p_inverts.se.fit, na.rm = TRUE) - min(p_inverts.se.fit, na.rm = TRUE)))
  }
  if ("p_black.se.fit" %in% colnames(data)) {
    data <- data %>%
      dplyr::mutate(p_black.alpha = 1 - (p_black.se.fit - min(p_black.se.fit, na.rm = TRUE)) /
                      (max(p_black.se.fit, na.rm = TRUE) - min(p_black.se.fit, na.rm = TRUE)))
  }
  return(data)
}

# NOTE (ERMP): benthos benchmark/control plots are NOT produced for ERMP.
# Habitat is modelled as a single pooled prediction (no year dimension), so the
# controldata_benthos() / controlplot_benthos() functions and Part 4 have been
# removed. A year-comparison benchmark plot built from a pooled prediction would
# show identical values for 2022 and 2025 (flat lines), which is misleading.
# Restore both functions and Part 4 from the template if per-year habitat
# predictions are ever produced.

# Fill scales used inside scatterpie_plot_single ----
hab_fills <- scale_fill_manual(
  name = "Habitat",
  limits = c("Rock", "Sessile invertebrates", "Macroalgae", "Seagrass", "Sand"),
  values = c(
    "Rock"                  = "grey40",
    "Sessile invertebrates" = "plum",
    "Macroalgae"            = "darkgoldenrod4",
    "Seagrass"              = "forestgreen",
    "Sand"                  = "wheat"
  )
)

wampa_fills <- scale_fill_manual(
  values = c(
    "Sanctuary Zone"       = "#bfd054",
    "General Use Zone"     = "#bddde1",
    "Special Purpose Zone" = "#c5bcc9"
  ),
  name = "State Marine Parks"
)

depth_fills <- scale_fill_manual(
  values = c("#a7cfe0", "#9acbec", "#98c4f7", "#a3bbff", "#81a1fc"),
  guide = "none"
)

scatterpie_plot_single <- function(benthos_year, site_limits, pie_radius = 0.004) {

  ggplot() +
    geom_contour_filled(
      data = bathy,
      aes(x, y, z = Depth, fill = after_stat(level)),
      color = "black",
      breaks = c(-30, -70, -200, -700, -2000, -4000),
      linewidth = 0.1
    ) +
    depth_fills +
    new_scale_fill() +
    geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.1) +
    geom_sf(data = wasanc, fill = "#bfd054", alpha = 2/5, colour = NA) +
    wampa_fills +
    labs(fill = "State Marine Parks") +
    new_scale_fill() +
    geom_sf(data = npz, fill = "#7bbc63", alpha = 2/5, colour = NA) +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, linewidth = 0.3) +
    new_scale_fill() +
    geom_scatterpie(
      data = benthos_year,
      aes(x = longitude_dd, y = latitude_dd, r = pie_radius),
      cols = c("Sand", "Sessile invertebrates", "Rock", "Macroalgae", "Seagrass"),
      colour = NA
    ) +
    hab_fills +
    labs(x = "Longitude", y = "Latitude") +
    coord_sf(
      xlim = c(site_limits[1], site_limits[2]),
      ylim = c(site_limits[3], site_limits[4]),
      crs = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background      = element_rect(fill = "#b9d1d6", colour = NA),
      panel.grid.major      = element_blank(),
      panel.grid.minor      = element_blank(),
      legend.position       = "bottom",
      legend.direction      = "horizontal",
      legend.box            = "vertical",
      legend.box.just       = "left",
      legend.title.position = "top",
      legend.text           = element_text(size = 10),
      legend.title          = element_text(size = 11),
      legend.key.size       = unit(0.3, "cm")
    )
}

# =============================================================================
# PART 1: Single pooled categorical + dominant benthos plot
# =============================================================================

pred_class <- as.data.frame(dat, xy = TRUE)
pred_plot  <- normalise_se(data = pred_class)

p_cat <- categoricalhabitat_plot(
  pred_plot         = pred_plot,
  prediction_limits = prediction_limits
)
print(p_cat)

ggsave(
  filename = paste0("plots/", park, "/habitat/", name, "_predicted-habitat-categorical.png"),
  plot = p_cat, height = 6, width = 8, dpi = 600, units = "in", bg = "white"
)
saveRDS(p_cat, paste0("plots/", park, "/habitat/", name, "_predicted-habitat-categorical.rds"))

p_dom <- dominantbenthos_plot(
  pred_plot         = pred_plot,
  prediction_limits = prediction_limits
) +
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.box       = "horizontal",
    legend.box.just  = "left",
    legend.text      = element_text(size = 5),
    legend.title     = element_text(size = 7),
    legend.key.size  = unit(0.5, "cm"),
    legend.margin    = margin(t = -0.1, unit = "cm")
  )
print(p_dom)

ggsave(
  filename = paste0("plots/", park, "/habitat/", name, "_predicted-dominant-habitat.png"),
  plot = p_dom, height = 6, width = 8, dpi = 600, units = "in", bg = "white"
)

saveRDS(p_dom, paste0("plots/", park, "/habitat/", name, "_predicted-dominant-habitat.rds"))

# =============================================================================
# PART 2: Dominant benthos + combined SE side-by-side plot
# =============================================================================

se_rast <- dat[["mean_se"]]

p_dom_se <- (
  p_dom |
    (ggplot() +
       geom_spatraster(data = se_rast, maxcell = Inf) +
       scale_fill_viridis_c(
         option = "A",
         na.value = "transparent",
         name = "Normalised\ncombined SE"
       ) +
       geom_contour(
         data = bathy, aes(x = x, y = y, z = Depth),
         colour = "black", breaks = c(-30, -70, -200), linewidth = 0.1
       ) +
       geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2) +
       geom_sf(data = marine_parks_amp, aes(colour = zone), fill = NA,
               show.legend = FALSE, linewidth = 0.6) +
       geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6) +
       scale_colour_manual(values = with(marine_parks_amp, setNames(colour, zone))) +
       coord_sf(
         xlim = c(prediction_limits[1], prediction_limits[2]),
         ylim = c(prediction_limits[3], prediction_limits[4]),
         crs = 4326, expand = FALSE
       ) +
       labs(x = NULL, y = NULL) +
       theme_minimal() +
       theme(
         axis.text = element_text(size = 8),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
         panel.grid.minor = element_blank(),
         legend.title = element_text(size = 10),
         legend.text = element_text(size = 9)
       ))
) +
  plot_layout(guides = "collect") &
  theme(
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.box       = "horizontal"
  )

print(p_dom_se)

ggsave(
  filename = paste0("plots/", park, "/habitat/", name, "_predicted-dominant-benthos-and-combined-se.png"),
  plot = p_dom_se, height = 6, width = 10, dpi = 900, units = "in", bg = "white"
)
saveRDS(p_dom_se, paste0("plots/", park, "/habitat/", name, "_predicted-dominant-benthos-and-combined-se.rds"))

# =============================================================================
# PART 3: Individual habitat plots (single pooled)
# =============================================================================
individualbenthic_plot <- function(habitat_name,
                                   layer_stub,
                                   dat_list,
                                   prediction_limits,
                                   pred_limits = c(0, 1),
                                   se_limits = NULL) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  # ---- Extract rasters ----
  pred_list <- lapply(dat_list, function(x) x[[paste0("p_", layer_stub, ".fit")]])
  se_list   <- lapply(dat_list, function(x) x[[paste0("p_", layer_stub, ".se.fit")]])

  # ---- Shared limits ----
  if (is.null(pred_limits)) {
    pred_vals   <- unlist(lapply(pred_list, terra::values))
    pred_limits <- range(pred_vals, na.rm = TRUE)
  }

  if (is.null(se_limits)) {
    se_vals   <- unlist(lapply(se_list, terra::values))
    se_limits <- range(se_vals, na.rm = TRUE)
  }

  # ---- Shared theme ----
  theme_map <- theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 8),
    axis.ticks = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width = unit(0.45, "cm"),
    plot.margin = margin(2, 2, 2, 2, unit = "mm")
  )

  # ---- Base map layers ----
  base_layers <- function() {
    list(
      geom_contour(
        data = bathy,
        aes(x = x, y = y, z = Depth),
        colour = "black",
        breaks = c(-30, -70, -200),
        linewidth = 0.1
      ),
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2),
      geom_sf(
        data = marine_parks_amp,
        aes(colour = zone),
        fill = NA,
        show.legend = FALSE,
        linewidth = 0.6
      ),
      geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6),
      scale_colour_manual(
        name = "Australian Marine Parks",
        values = with(marine_parks_amp, setNames(colour, zone))
      ),
      coord_sf(
        xlim = c(prediction_limits[1], prediction_limits[2]),
        ylim = c(prediction_limits[3], prediction_limits[4]),
        crs = 4326,
        expand = FALSE
      ),
      labs(x = NULL, y = NULL, colour = NULL),
      theme_minimal(),
      theme_map
    )
  }

  # ---- Build one prediction + SE pair per element in dat_list ----
  plots <- lapply(seq_along(yrs), function(i) {

    p_pred <- ggplot() +
      geom_spatraster(data = pred_list[[i]]) +
      scale_fill_viridis_c(
        name = "Probability",
        direction = -1,
        na.value = "transparent",
        limits = pred_limits,
        oob = scales::squish
      ) +
      ggtitle("Prediction") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        legend.position = "right"
      ) +
      base_layers()

    p_se <- ggplot() +
      geom_spatraster(data = se_list[[i]]) +
      scale_fill_viridis_c(
        option = "A",
        name = "SE",
        na.value = "transparent",
        limits = se_limits,
        oob = scales::squish
      ) +
      ggtitle("Standard Error") +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right"
      ) +
      base_layers()

    p_pred | p_se
  })

  # ---- Stack groups vertically if more than one element ----
  p_out <- wrap_plots(plots, ncol = 1)

  return(p_out)
}

for (habitat_name in names(habitat_lookup)) {

  message("Building individual habitat plot for: ", habitat_name)

  layer_stub <- habitat_lookup[[habitat_name]]

  p_hab <- individualbenthic_plot(
    habitat_name      = habitat_name,
    layer_stub        = layer_stub,
    dat_list          = list(pooled = dat),
    prediction_limits = prediction_limits,
    pred_limits       = NULL,
    se_limits         = NULL
  )

  print(p_hab)

  out_name <- habitat_name %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "-")

  ggsave(
    filename = paste0("plots/", park, "/habitat/", name, "_predicted-individual-habitat_", out_name, ".png"),
    plot = p_hab, height = 5, width = 8, dpi = 900, units = "in", bg = "white"
  )
  saveRDS(p_hab, paste0("plots/", park, "/habitat/", name, "_predicted-individual-habitat_", out_name, ".rds"))
}

# =============================================================================
# PART 4: (removed for ERMP) Benthos benchmark / control plots
# =============================================================================
# Removed: pooled habitat has no year dimension, so a 2022-vs-2025 benchmark plot
# would duplicate identical values across years (flat lines). The controldata_benthos()
# and controlplot_benthos() definitions and the taxa_lookup driver loop have been
# removed. Restore from the template if per-year habitat predictions are produced.

# =============================================================================
# PART 5: Scatterpie (pooled, single plot)
# =============================================================================

metadata_bathy_derivatives <- readRDS(
  paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")
) %>%
  clean_names()

benthos <- readRDS(
  paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")
) %>%
  dplyr::rename(
    Macroalgae              = macroalgae,
    Seagrass                = seagrasses,
    Sand                    = sand,
    Rock                    = rock,
    "Sessile invertebrates" = sessile_invertebrates
  ) %>%
  left_join(metadata_bathy_derivatives, by = c("campaignid", "sample", "year", "status")) %>%
  arrange(desc(Sand))

site_limits <- c(123.25, 123.8, -34.63, -33.95) # TODO set limits

p_scatterpie <- scatterpie_plot_single(
  benthos_year = benthos,
  site_limits  = site_limits,
  pie_radius   = 0.005
)
print(p_scatterpie)

ggsave(
  filename = paste0("plots/", park, "/habitat/", name, "_scatterpie.png"),
  plot = p_scatterpie, height = 10, width = 6, dpi = 300, bg = "white"
)
saveRDS(p_scatterpie, paste0("plots/", park, "/habitat/", name, "_scatterpie.rds"))

## Predicted reef
p_reef <- predictedreef_plot_multi(
  dat_list          = list(pooled = dat),
  prediction_limits = prediction_limits
)

print(p_reef)

# Unsuffixed filenames, matching the pooled convention used by every other
# output in this script (`years` is not read from the config here).
ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name, "_predicted-reef-and-se.png"
  ),
  plot = p_reef, height = 7, width = 8, dpi = 300, units = "in", bg = "white"
)

saveRDS(p_reef,
        paste0("plots/", park, "/habitat/", name,
               "_predicted-reef-and-se.rds"))
