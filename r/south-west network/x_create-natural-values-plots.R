###
# Project: NESP 5.6 Project - South-west Corner Report
# Data:    Natural values ecosystems (NESP MERI), Commonwealth marine parks,
#          terrestrial parks and aus outline
# Task:    Creating natural values (benthic ecosystem) map — south-west network
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. South-west network natural values map (original source colours,
#             predicted reef layer removed, Commonwealth marine parks only)
#          2. Individual park zoom-in natural values maps (Abrolhos, Bremer Bay,
#             Eastern Recherche [+ full extent], Geographe, Great Australian
#             Bight [+ full extent], Jurien Bay, Kangaroo Island, Murat &
#             Western Eyre [+ full extent, + Murat only], Rottnest Canyon,
#             SWC east, SWC west, Two Rocks, Twilight)
#
# NOTE: adapted from the north-west network version of this script. Two things
# below are best-guess placeholders and should be checked against the actual
# south-west shapefile before running:
#   1. The marine park shapefile filename (section 1)
#   2. The `name %in% c(...)` list used to build marine_parks_amp (section 1) —
#      built from the individual park names used in the south-west AMP bathymetry
#      script, but not verified against the shapefile's actual `name` field.
###

# Table of contents
#     1.  Set up and load data
#     2.  CRS, colours and other housekeeping
#     3.  Network-scale map function
#     4.  FIGURE 1: South-west network natural values map
#     5.  Individual park functions — hillshade past 200m
#     6.  FIGURES 2+: Individual park zoom-ins (assemble and save)


# ==============================================================================
# 1. LOAD DATA AND SETUP
# ==============================================================================

# Clear environment
rm(list = ls())

# Set study name (folder structure)
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(cowplot)
library(ggplot2)
library(dplyr)

# Set cropping extent (matches south-west network AMP bathymetry script's
# bbox_network, with a small buffer)
e <- ext(106, 139, -41, -22)

# Aus outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Marine parks — Commonwealth AMPs (south-west network) + WA/SA state marine
# parks. VERIFY this list against the shapefile's `name` field — built from
# the individual park zoom-ins in the AMP bathymetry script, not confirmed.
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(# Commonwealth AMPs (South-west Network)
    "Abrolhos", "Bremer", "Eastern Recherche", "Geographe",
    "Great Australian Bight", "Jurien", "Kangaroo Island",
    "Murray", "Western Eyre", "Rottnest Canyon", "South-west Corner",
    "Twilight", "Two Rocks", "Murat",
    # State marine parks (WA/SA) — update to match your shapefile's attributes
    "Ngari Capes", "Shoalwater Islands", "Marmion", "Walpole and Nornalup Inlets")) %>%
  glimpse()

# Commonwealth marine parks only, for the natural values map
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# Terrestrial parks for mapping
terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  st_make_valid() %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

# Natural values ecosystem — NESP MERI raster (has its own embedded colour table)
naturalvalues <- rast("data/south-west network/spatial/rasters/NESP_MERI_Natural_Values_Ecosystems.tif") %>%
  crop(e)

# Bathymetry — used for hillshade background on individual park figures only
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)


# ==============================================================================
# 2. CRS, COLOURS, AND OTHER HOUSEKEEPING
# ==============================================================================

target_crs <- "EPSG:4326"

if (!same.crs(naturalvalues, target_crs)) naturalvalues <- project(naturalvalues, target_crs, method = "near")
if (!same.crs(bathy, target_crs))         bathy         <- project(bathy,         target_crs, method = "bilinear")
if (st_crs(aus)          != st_crs(4326)) aus          <- st_transform(aus,          4326)
if (st_crs(terrnp)       != st_crs(4326)) terrnp       <- st_transform(terrnp,       4326)
if (st_crs(marine_parks_amp) != st_crs(4326)) marine_parks_amp <- st_transform(marine_parks_amp, 4326)

# Clip natural values to the 200m shelf — past 200m the hillshade base is
# shown alone with no ecosystem colour on top (same pattern as the old
# south-west script's mask_250, but at the 200m break requested here)
mask_200 <- ifel(bathy >= -200, 1, NA)
mask_200_resamp <- resample(mask_200, naturalvalues, method = "near")
naturalvalues_clipped <- mask(naturalvalues, mask_200_resamp)

# Class names (1:18, same lookup as the north-west/south-west scripts)
nv_lookup <- c(
  "1"  = "Shelf unvegetated sediments",
  "2"  = "Upper slope sediments",
  "3"  = "Mid slope sediments",
  "4"  = "Lower slope reef and sediments",
  "5"  = "Abyssal reef and sediments",
  "6"  = "Seamount sediments",
  "7"  = "Shelf incising canyons",
  "8"  = "Oceanic shallow coral reefs",
  "9"  = "Shelf vegetated sediments",
  "10" = "Shallow coral reefs",
  "11" = "Shallow rocky reefs",
  "12" = "Mesophotic coral reefs",
  "13" = "Mesophotic rocky reefs",
  "14" = "Oceanic mesophotic coral reefs",
  "15" = "Rariphotic shelf reefs",
  "16" = "Upper slope reefs",
  "17" = "Mid slope reefs",
  "18" = "Seamount reefs"
)

# Original colours — pulled directly from the raster's own embedded colour
# table (NESP_MERI_Natural_Values_Ecosystems.tif), NOT the approximate
# R-named colours (hab_colours) used in earlier scripts.
hab_colours_original <- c(
  "Shelf unvegetated sediments"      = "#A2D9FF",
  "Upper slope sediments"            = "#5171E2",
  "Mid slope sediments"              = "#B13DFF",
  "Lower slope reef and sediments"   = "#4098C4",
  "Abyssal reef and sediments"       = "#0012D9",
  "Seamount sediments"               = "#42ECD0",
  "Shelf incising canyons"           = "#848484",
  "Oceanic shallow coral reefs"      = "#EEA6F1",
  "Shelf vegetated sediments"        = "#29D000",
  "Shallow coral reefs"              = "#A17456",
  "Shallow rocky reefs"              = "#C15E7D",
  "Mesophotic coral reefs"           = "#E0A800",
  "Mesophotic rocky reefs"           = "#F427E3",
  "Oceanic mesophotic coral reefs"   = "#E7689F",
  "Rariphotic shelf reefs"           = "#DF0003",
  "Upper slope reefs"                = "#FFE400",
  "Mid slope reefs"                  = "#B1C706",
  "Seamount reefs"                   = "#9EED7C"
)


# ==============================================================================
# 3. NETWORK-SCALE MAP FUNCTION — natural values only, no predicted reef
# ==============================================================================

naturalvalues_map_southwest <- function(plot_limits,
                                        ocean_colour = "#e8e8e8",
                                        show_legend  = TRUE,
                                        title        = NULL,
                                        break_step   = 5.0) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale); require(cowplot)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])
  nv_crop  <- crop(naturalvalues, ext_plot)

  nv_df <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours_original[names(hab_colours_original) %in% present_classes]

  # Keep legend in the same class order as nv_lookup
  level_order <- names(nv_lookup)[names(nv_lookup) %in% as.character(nv_df$value)]
  level_order <- unname(nv_lookup[level_order])
  nv_df$classname <- factor(nv_df$classname, levels = level_order)

  x_breaks <- seq(floor(plot_limits[1] / break_step) * break_step,
                  ceiling(plot_limits[2] / break_step) * break_step,
                  by = break_step)
  y_breaks <- seq(floor(plot_limits[3] / break_step) * break_step,
                  ceiling(plot_limits[4] / break_step) * break_step,
                  by = break_step)

  # ── Main map — legends suppressed here, built separately below ──────────
  p_map <- ggplot() +

    # Natural values ecosystem layer — original source colours
    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(values = present_colours[level_order], breaks = level_order, guide = "none") +

    # Land
    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      guide  = "none"
    ) +

    # Commonwealth marine park boundaries
    geom_sf(data = marine_parks_amp, fill = alpha("grey70", 0.3),
            colour = alpha("white", 0.7), linewidth = 0.35) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.position  = "none",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white",      colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0)
      else                 element_blank(),
      plot.margin      = margin(t = 0, r = 0, b = 0, l = 0)
    )

  if (!show_legend) return(p_map)

  # ── Benthic ecosystem legend — ncol = 3, same as set in this script ──────
  dummy_df <- data.frame(x = 1, y = 1, classname = factor(level_order, levels = level_order))

  legend_benthic <- ggplot(dummy_df, aes(x = x, y = y, fill = classname)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours[level_order],
      breaks = level_order,
      guide  = guide_legend(ncol = 4, direction = "horizontal",
                            title.position = "top", title.hjust = 0, byrow = TRUE)
    ) +
    theme_void() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 11),
      legend.position  = "bottom"
    )

  # ── Terrestrial parks legend — ncol = 1, same as set in this script ──────
  tp_df <- data.frame(x = 1, y = 1,
                      tp = factor(c("National Park", "Nature Reserve"),
                                  levels = c("National Park", "Nature Reserve")))

  legend_tp <- ggplot(tp_df, aes(x = x, y = y, fill = tp)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Terrestrial Parks",
      values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
      guide  = guide_legend(ncol = 1, title.position = "top")
    ) +
    theme_void() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 11),
      legend.position  = "top"
    )

  # Benthic ecosystem legend sits next to Terrestrial Parks legend, not stacked
  legend_row <- cowplot::plot_grid(
    cowplot::get_legend(legend_benthic),
    cowplot::get_legend(legend_tp),
    nrow       = 1,
    rel_widths = c(4, 1.1)
  )

  p <- cowplot::plot_grid(
    p_map,
    legend_row,
    ncol        = 1,
    rel_heights = c(1, 0.25)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA))

  return(p)
}


# ==============================================================================
# 4. FIGURE 1: South-west network natural values map
# ==============================================================================

network_limits <- c(109, 139, -41, -24.05)

figure_southwest_nv <- naturalvalues_map_southwest(
  plot_limits  = network_limits,
  ocean_colour = "#2b3a4a",
  show_legend  = TRUE,
  break_step   = 5.0
)

ggsave(paste(paste0("plots/", park, "/spatial/benthic_habitat/", name),
             "network-natural-values.png", sep = "-"),
       plot   = figure_southwest_nv,
       dpi    = 600,
       width  = 12,
       height = 9,
       bg     = "white")


# ==============================================================================
# 5. INDIVIDUAL PARK FUNCTIONS — hillshade past 200m
# ==============================================================================
# --- Helper: sanity-check the plotted aspect ratio for a given extent ---
check_ratio <- function(l) {
  mean_lat <- (l[3] + l[4]) / 2
  cos_lat  <- cos(mean_lat * pi / 180)
  rendered <- (l[2] - l[1]) / (l[4] - l[3]) * cos_lat
  cat(sprintf("w: %.4f  h: %.4f  raw_ratio: %.3f  rendered_ratio: %.3f\n",
              l[2]-l[1], l[4]-l[3], (l[2]-l[1])/(l[4]-l[3]), rendered))
}

shelf_classes <- c(
  "Shelf unvegetated sediments",
  "Shelf vegetated sediments",
  "Oceanic shallow coral reefs",
  "Shallow coral reefs",
  "Shallow rocky reefs",
  "Mesophotic coral reefs",
  "Mesophotic rocky reefs",
  "Oceanic mesophotic coral reefs",
  "Rariphotic shelf reefs",
  "Upper slope reefs",
  "Upper slope sediments",
  "Shelf incising canyons"
)

# --- FUNCTION: full natural values layer, ocean-colour background, no hillshade ---
naturalvalues_map_hillshade_southwest <- function(plot_limits,
                                                  ocean_colour = "#e8e8e8",
                                                  show_legend  = TRUE,
                                                  title        = NULL,
                                                  break_step   = 0.2) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])

  # --- Natural values — full layer, all classes, no 200m clip ---
  nv_crop <- crop(naturalvalues, ext_plot)
  nv_df   <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  present_classes <- unique(nv_df$classname)
  present_colours <- hab_colours_original[names(hab_colours_original) %in% present_classes]

  level_order <- names(nv_lookup)[names(nv_lookup) %in% as.character(nv_df$value)]
  level_order <- unname(nv_lookup[level_order])
  nv_df$classname <- factor(nv_df$classname, levels = level_order)

  x_breaks <- seq(floor(plot_limits[1] / break_step) * break_step,
                  ceiling(plot_limits[2] / break_step) * break_step,
                  by = break_step)
  y_breaks <- seq(floor(plot_limits[3] / break_step) * break_step,
                  ceiling(plot_limits[4] / break_step) * break_step,
                  by = break_step)

  nv_guide <- if (show_legend) guide_legend(order = 1, ncol = 1, title.position = "top") else "none"

  p <- ggplot() +

    # Natural values layer — full raster, no hillshade underneath
    geom_tile(data = nv_df, aes(x = x, y = y, fill = classname)) +
    scale_fill_manual(
      name   = "Benthic ecosystem",
      values = present_colours[level_order],
      breaks = level_order,
      guide  = nv_guide
    ) +

    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(order = 2, ncol = 1, title.position = "top") else "none"
    ) +

    geom_sf(data = marine_parks_amp, fill = NA, colour = alpha("grey40", 0.6), linewidth = 0.3) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4], crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 11),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = ocean_colour, colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.text        = element_text(size = 10, colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      plot.title       = if (!is.null(title)) element_text(face = "bold", size = 14, hjust = 0.5)
      else                 element_blank(),
      plot.margin      = margin(t = 0, r = 0, b = 0, l = 0)
    )

  return(p)
}

# ── Reusable natural values / benthic habitat plot function ──────────────────
make_natural_values_plot <- function(plot_limits, break_step, save_name,
                                     width, height, park, name,
                                     legend_ncol = 4) {

  check_ratio(plot_limits)

  # Map itself already draws aus + terrnp (see naturalvalues_map_hillshade_southwest)
  hs <- naturalvalues_map_hillshade_southwest(
    plot_limits = plot_limits,
    show_legend = FALSE,
    break_step  = break_step
  )

  ext_crop <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])

  # ── Benthic habitat legend — ALL classes present in this park's extent ────
  nv_crop <- crop(naturalvalues, ext_crop)
  nv_df   <- as.data.frame(nv_crop, xy = TRUE, na.rm = TRUE)
  colnames(nv_df)[3] <- "value"
  nv_df$classname <- nv_lookup[as.character(nv_df$value)]
  nv_df <- dplyr::filter(nv_df, !is.na(classname))

  level_order <- names(nv_lookup)[names(nv_lookup) %in% as.character(nv_df$value)]
  level_order <- unname(nv_lookup[level_order])

  legend_benthic <- ggplot(data.frame(x = 1, y = 1,
                                      classname = factor(level_order, levels = level_order)),
                           aes(x = x, y = y, fill = classname)) +
    geom_tile() +
    scale_fill_manual(
      name   = "Benthic habitat",
      values = hab_colours_original[level_order],
      breaks = level_order,
      guide  = guide_legend(ncol = legend_ncol, direction = "horizontal",
                            title.position = "top", title.hjust = 0, byrow = TRUE)
    ) +
    theme_void() +
    theme(
      legend.key.size = unit(0.45, "cm"),
      legend.text     = element_text(size = 9),
      legend.title    = element_text(size = 11),
      legend.position = "bottom"
    )

  # ── Terrestrial parks legend — only include types actually present ────────
  bbox_sf <- st_as_sfc(st_bbox(c(xmin = plot_limits[1], xmax = plot_limits[2],
                                 ymin = plot_limits[3], ymax = plot_limits[4]),
                               crs = st_crs(terrnp)))
  terrnp_crop  <- suppressWarnings(st_intersection(terrnp, bbox_sf))
  present_types <- intersect(c("National Park", "Nature Reserve"), unique(terrnp_crop$TYPE))

  if (length(present_types) > 0) {

    legend_tp <- ggplot(data.frame(x = 1, y = 1,
                                   tp = factor(present_types, levels = present_types)),
                        aes(x = x, y = y, fill = tp)) +
      geom_tile() +
      scale_fill_manual(
        name   = "Terrestrial Parks",
        values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb")[present_types],
        guide  = guide_legend(ncol = 1, title.position = "top")
      ) +
      theme_void() +
      theme(
        legend.key.size = unit(0.45, "cm"),
        legend.text     = element_text(size = 9),
        legend.title    = element_text(size = 11),
        legend.position = "top"
      )

    legend_row <- cowplot::plot_grid(
      cowplot::get_legend(legend_benthic),
      cowplot::get_legend(legend_tp),
      nrow       = 1,
      rel_widths = c(4, 1.1)
    )

  } else {
    legend_row <- cowplot::get_legend(legend_benthic)
  }

  figure <- cowplot::plot_grid(
    hs,
    legend_row,
    ncol        = 1,
    rel_heights = c(1, 0.2),
    align       = "v",
    axis        = "t"
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin     = margin(t = 2, r = 15, b = 15, l = 5))

  ggsave(paste(paste0("plots/", park, "/spatial/benthic_habitat/", name),
               paste0(save_name, "-natural-values.png"), sep = "-"),
         plot   = figure,
         dpi    = 600,
         width  = width,
         height = height,
         bg     = "white")

  invisible(figure)
}

# ==============================================================================
# 6. INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================
# All extents below are taken directly from the south-west network AMP
# bathymetry script's network_map_wms_zoomed() calls. break_step is set from
# that script's `break_step` argument where supplied (used there for thinned
# longitude breaks); otherwise a sensible default is used since this script's
# breaks aren't thinned the same way.

# ── Abrolhos ──────────────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(108.5, 116.1, -30.0, -24.2),
  break_step  = 1.0,
  save_name   = "abrolhos",
  width       = 8,
  height      = 8.5,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ── Bremer Bay ────────────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(119.3, 120.3, -35.3, -33.9),
  break_step  = 0.2,
  save_name   = "bremer",
  width       = 5,
  height      = 11.2,
  park        = park,
  name        = name,
  legend_ncol = 2
)

# ── Eastern Recherche ─────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(123.2, 124.4, -34.9, -33.5),
  break_step  = 0.2,
  save_name   = "eastern-recherche",
  width       = 7,
  height      = 9.5,
  park        = park,
  name        = name,
  legend_ncol = 2
)

make_natural_values_plot(
  plot_limits = c(122.2, 125.6, -37.8, -33.4),
  break_step  = 0.5,
  save_name   = "eastern-recherche-full-extent",
  width       = 6.5,
  height      = 11,
  park        = park,
  name        = name,
  legend_ncol = 2
)

# ── Geographe ─────────────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(114.8, 115.7, -33.7, -33.2),
  break_step  = 0.2,
  save_name   = "geographe",
  width       = 8,
  height      = 6.5,
  park        = park,
  name        = name,
  legend_ncol = 2
)

# ── Great Australian Bight ────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(128.7, 132.5, -33.6, -31.3),
  break_step  = 0.5,
  save_name   = "great-aus-bight",
  width       = 7,
  height      = 6,
  park        = park,
  name        = name,
  legend_ncol = 2
)

make_natural_values_plot(
  plot_limits = c(128.7, 132.5, -37.8, -31.3),
  break_step  = 1,
  save_name   = "great-aus-bight_full-extent",
  width       = 7,
  height      = 9,
  park        = park,
  name        = name,
  legend_ncol = 2
)

# ── Jurien Bay ────────────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(114.2, 115.5, -31.0, -30.0),
  break_step  = 0.2,
  save_name   = "jurien",
  width       = 8,
  height      = 9,
  park        = park,
  name        = name,
  legend_ncol = 2
)

# ── Kangaroo Island ───────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(136.0, 137.85, -36.5, -35.5),
  break_step  = 0.5,
  save_name   = "kangaroo-island",
  width       = 9,
  height      = 7.5,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ── Murat and Western Eyre ────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(132.45, 135.5, -35.4, -31.9),
  break_step  = 0.5,
  save_name   = "murat-western-eyre",
  width       = 8,
  height      = 9,
  park        = park,
  name        = name,
  legend_ncol = 2
)

make_natural_values_plot(
  plot_limits = c(131, 137, -39.4, -31.9),
  break_step  = 1,
  save_name   = "murat-western-eyre_full-extent",
  width       = 6,
  height      = 11,
  park        = park,
  name        = name,
  legend_ncol = 2
)

make_natural_values_plot(
  plot_limits = c(132.3, 133, -33.2, -32.2),
  break_step  = 0.5,
  save_name   = "murat",
  width       = 5,
  height      = 7,
  park        = park,
  name        = name,
  legend_ncol = 1
)

# ── Rottnest Island Canyon ────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(113.8, 115.8, -32.8, -31.3),
  break_step  = 0.5,
  save_name   = "rottnest-canyon",
  width       = 10,
  height      = 6,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ── SWC Eastern arm ───────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(120.35, 122.2, -35.5, -33.7),
  break_step  = 0.5,
  save_name   = "swc-east",
  width       = 8,
  height      = 6,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ── SWC Western arm ───────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(113.5, 116.4, -34.7857, -33.2643),
  break_step  = 0.5,
  save_name   = "swc-west",
  width       = 9,
  height      = 4.5,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ── Two Rocks ─────────────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(114.7, 116.0, -32.0, -31.3),
  break_step  = 0.2,
  save_name   = "two-rocks",
  width       = 11.5,
  height      = 5,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ── Twilight MP ───────────────────────────────────────────────────────────────
make_natural_values_plot(
  plot_limits = c(125.2, 127.15, -33.3, -32.1),
  break_step  = 0.5,
  save_name   = "twilight",
  width       = 9,
  height      = 5,
  park        = park,
  name        = name,
  legend_ncol = 3
)

# ==============================================================================
# End of script
# ==============================================================================
