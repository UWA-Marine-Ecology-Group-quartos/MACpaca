###
# Project: NESP 4.20 / 5.6 - South-west Network Report
# Data:    AusBathyTopo bathymetry (2024), CAPAD terrestrial parks, aus
#          outline, south-west network marine park shapefile
# Task:    Create network-scale and individual park bathymetry maps
# Author:  Annika Leunig
# Date:    September 2026
# Outputs: 1. South-west network bathymetry map (fixed 0-7000 m scale)
#          2. Individual park bathymetry maps for every zoom-in extent used
#             in 10_create-AMP-bathy-maps.R (Abrolhos, Bremer Bay, Eastern
#             Recherche [+ full extent], Geographe, Great Australian Bight
#             [+ full extent], Jurien Bay, Kangaroo Island, Murat & Western
#             Eyre [+ full extent, + Murat], Rottnest Canyon, SWC East
#             [+ full extent], SWC West, Two Rocks, Twilight, and the SW
#             full-extent overview) — each with a colour scale fitted to its
#             own depth range, with manually overridable breaks to avoid
#             overlapping colourbar labels
#
###

# Table of contents
#     1.  Set up and load libraries
#     2.  Load spatial files
#     3.  Hillshade and axis break helper
#     4.  Define colour ramp (network-scale, fixed)
#     5.  FIGURE 1: South-west Network
#     6.  Dynamic colour scale helpers (per-park depth range)
#     7.  Individual park bathymetry panel function
#     8.  Individual park bathymetry panels (assemble and save) — every
#         zoom-in extent from 10_create-AMP-bathy-maps.R

# ==============================================================================
# 1. SET UP AND LOAD
# ==============================================================================

# Clear the environment
rm(list = ls())

# Set the study name
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(RColorBrewer)

# Set cropping extent (covers the network extent plus every park zoom-in
# extent used in 10_create-AMP-bathy-maps.R, with a margin for hillshading)
e <- ext(107, 140, -42, -23)

# Progress bar for raster operations
terraOptions(progress = 3)
sf_use_s2(TRUE)

# ==============================================================================
# 2. LOAD SPATIAL FILES
# ==============================================================================
# Terrestrial parks using CAPAD (same source/column as the north-west network
# bathy-network-plot script)
terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

# Aus Outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Marine parks — south-west network AMPs + WA/SA state marine parks (same
# filter list used in the south-west network's 04_create-bathymetry-maps.R)
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
    "Ngari Capes", "Geographe", "South-west Corner", "Great Australian Bight",
    "Jurien", "Murat", "Jurien Bay", "Perth Canyon", "Southern Kangaroo Island",
    "Twilight", "Two Rocks", "Western Eyre", "Western Kangaroo Island",
    "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group", "Investigator",
    "West coast Bays", "Southern Spencer Gulf", "Upper Spencer Gulf",
    "Cottesloe Reef", "Rottnest", "Shoalwater Islands"))

# Bathymetry layer
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e)

# ==============================================================================
# 3. HILLSHADE AND AXIS BREAK HELPER
# ==============================================================================

make_hillshade <- function(bathy_rast) {
  slope  <- terrain(bathy_rast, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_rast, v = "aspect", unit = "radians")
  shade(slope, aspect, angle = 40, direction = 270)
}

hill <- make_hillshade(bathy)

# to manually set the tick marks for the plots (same helper as the north-west
# network zone-map / bathy-network-plot scripts)
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# ==============================================================================
# 4. DEFINE COLOUR RAMP (NETWORK-SCALE, FIXED)
# ==============================================================================
# Same fixed 0-7000 m stretch as the north-west network standardised script,
# so the two networks read on the same visual scale. If south-west network
# depths exceed 7000 m (e.g. abyssal areas offshore), widen `limits`/`breaks`
# below and in bathy_trans accordingly.
bathy_cols <- RColorBrewer::brewer.pal(11, "Spectral")
bathy_cols <- c(bathy_cols[1:9], bathy_cols[10], bathy_cols[10], bathy_cols[11], bathy_cols[11])

bathy_trans <- scales::trans_new(
  name      = "bathy_stretch",
  transform = function(x) {
    ifelse(x <= 200,
           x / 200 * 0.3,
           0.3 + (x - 200) / (7000 - 200) * 0.7)
  },
  inverse = function(p) {
    ifelse(p <= 0.3,
           p / 0.3 * 200,
           200 + (p - 0.3) / 0.7 * 6800)
  }
)

hill_scale <- scale_fill_gradient(
  low      = "#1a1a2e",
  high     = "#a0a0a0",
  na.value = NA,
  guide    = "none"
)

# ==============================================================================
# 5. FIGURE 1: South-west Network
# ==============================================================================
# ── Set up ────────────────────────────────────────────────────────────────────
names(bathy) <- "depth"
names(hill)  <- "hillshade"

bathy_abs <- bathy * -1
names(bathy_abs) <- "depth_abs"

# Same network extent as bbox_network in 10_create-AMP-bathy-maps.R
xlim_shared <- c(109, 139)
ylim_shared <- c(-41, -24.05)

# ── Bathymetry panel ───────────────────────────────────────────────────────────
p_bathy <- ggplot() +
  # Hillshade first (bottom layer)
  geom_spatraster(data = hill, aes(fill = hillshade),
                  alpha = 0.40, show.legend = FALSE) +
  hill_scale +
  new_scale_fill() +
  geom_spatraster(data = bathy_abs, aes(fill = depth_abs),
                  alpha = 0.65) +
  scale_fill_gradientn(
    colours  = bathy_cols,
    trans    = bathy_trans,
    limits   = c(0, 7000),
    breaks   = c(0, 200, 2500, 4500, 7000),
    labels   = c("0", "-200", "-2500","-4500", "-7,000"),
    na.value = NA,
    name     = "Depth (m)",
    guide    = guide_colorbar(
      barwidth       = 14,
      barheight      = 0.5,
      title.position = "top",
      title.hjust    = 0.5,
      title.theme    = element_text(size = 9, face = "plain"),
      label.theme    = element_text(size = 8, face = "plain")
    )
  ) +
  # Australia landmass
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
  new_scale_fill() +
  # Terrestrial parks
  geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
  scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    guide  = "none"
  ) +
  # Marine parks on top — no fill, white boundary
  geom_sf(data = marine_parks,
          fill      = NA,
          colour    = alpha("white", 0.3),
          linewidth = 0.7) +
  coord_sf(xlim = xlim_shared, ylim = ylim_shared, expand = FALSE) +
  scale_x_continuous(breaks = thin_breaks(xlim_shared, step = 2)) +
  scale_y_continuous(breaks = thin_breaks(ylim_shared, step = 2)) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.margin = margin(10, 0, 0, 0),
    legend.title      = element_text(size = 9,  face = "plain"),
    legend.text       = element_text(size = 8,  face = "plain"),
    panel.grid        = element_blank(),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.background   = element_rect(fill = "white", colour = NA),
    panel.border      = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
    axis.ticks        = element_line(colour = "grey80", linewidth = 0.3),
    axis.text         = element_text(size = 8, colour = "grey40"),
    plot.margin       = margin(2, 2, 2, 2)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
dir.create(paste0("plots/", park, "/spatial/bathymetry/SW/"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = paste0("plots/", park, "/spatial/bathymetry/SW/", name, "-network-bathy-panel.png"),
  plot     = p_bathy,
  dpi      = 800,
  width    = 7,
  height   = 6,
  bg       = "white"
)

# ==============================================================================
# 6. DYNAMIC COLOUR SCALE HELPERS (PER-PARK DEPTH RANGE)
# ==============================================================================
get_max_depth <- function(bathy_abs_rast) {
  v <- terra::values(bathy_abs_rast, mat = FALSE)
  v <- v[is.finite(v) & v > 0]
  if (length(v) == 0) return(200)
  ceiling(max(v, na.rm = TRUE) / 100) * 100
}

make_bathy_trans_dynamic <- function(max_depth, stretch_depth = 200, stretch_frac = 0.3) {

  if (max_depth <= stretch_depth) {
    return(scales::trans_new(
      name      = "bathy_stretch_dynamic",
      transform = function(x) x / max_depth,
      inverse   = function(p) p * max_depth
    ))
  }

  scales::trans_new(
    name      = "bathy_stretch_dynamic",
    transform = function(x) {
      ifelse(x <= stretch_depth,
             x / stretch_depth * stretch_frac,
             stretch_frac + (x - stretch_depth) / (max_depth - stretch_depth) * (1 - stretch_frac))
    },
    inverse = function(p) {
      ifelse(p <= stretch_frac,
             p / stretch_frac * stretch_depth,
             stretch_depth + (p - stretch_frac) / (1 - stretch_frac) * (max_depth - stretch_depth))
    }
  )
}


make_bathy_breaks <- function(max_depth, stretch_depth = 200) {
  if (max_depth <= stretch_depth) {
    sort(unique(c(0, max_depth)))
  } else {
    sort(unique(c(0, stretch_depth, max_depth)))
  }
}

# "-200", "-2,000" etc, but "0" stays unsigned
make_bathy_labels <- function(breaks) {
  ifelse(breaks == 0, "0", paste0("-", scales::comma(breaks)))
}

# ==============================================================================
# 7. INDIVIDUAL PARK BATHYMETRY PANEL FUNCTION
# ==============================================================================
make_bathy_plot <- function(longitude, latitude, save_name, width, height,
                            break_step = 0.2, depth_breaks = NULL) {

  bathy_park <- bathy %>%
    crop(ext(longitude[1], longitude[2], latitude[1], latitude[2]))

  hill_park <- make_hillshade(bathy_park)
  names(hill_park) <- "hillshade"

  bathy_abs_park <- bathy_park * -1
  names(bathy_abs_park) <- "depth_abs"

  # Dynamic depth scale for this park's actual range
  max_depth   <- get_max_depth(bathy_abs_park)
  trans_park  <- make_bathy_trans_dynamic(max_depth)
  breaks_park <- if (is.null(depth_breaks)) make_bathy_breaks(max_depth) else depth_breaks
  labels_park <- make_bathy_labels(breaks_park)

  p <- ggplot() +
    # Hillshade first (bottom layer)
    geom_spatraster(data = hill_park, aes(fill = hillshade),
                    alpha = 0.40, show.legend = FALSE) +
    hill_scale +
    new_scale_fill() +
    # Bathymetry second — scale fitted to this park's own depth range
    geom_spatraster(data = bathy_abs_park, aes(fill = depth_abs),
                    alpha = 0.65) +
    scale_fill_gradientn(
      colours  = bathy_cols,
      trans    = trans_park,
      limits   = c(0, max_depth),
      breaks   = breaks_park,
      labels   = labels_park,
      na.value = NA,
      name     = "Depth (m)",
      guide    = guide_colorbar(
        barwidth       = 14,
        barheight      = 0.5,
        title.position = "top",
        title.hjust    = 0.5,
        title.theme    = element_text(size = 9, face = "plain"),
        label.theme    = element_text(size = 8, face = "plain")
      )
    ) +
    # Australia landmass
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    new_scale_fill() +
    # Terrestrial parks
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
      guide  = "none"
    ) +
    # Marine parks on top — no fill, white boundary
    geom_sf(data = marine_parks,
            fill      = NA,
            colour    = alpha("white", 0.3),
            linewidth = 0.7) +
    coord_sf(xlim = longitude, ylim = latitude, expand = FALSE) +
    scale_x_continuous(breaks = thin_breaks(longitude, step = break_step)) +
    scale_y_continuous(breaks = thin_breaks(latitude,  step = break_step)) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      legend.margin     = margin(0, 0, 0, 0),
      legend.box.margin = margin(10, 0, 0, 0),
      legend.title      = element_text(size = 9,  face = "plain"),
      legend.text       = element_text(size = 8,  face = "plain"),
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
      axis.ticks        = element_line(colour = "grey80", linewidth = 0.3),
      axis.text         = element_text(size = 8, colour = "grey40"),
      plot.margin       = margin(2, 2, 2, 2)
    )

  dir.create(paste0("plots/", park, "/spatial/bathymetry/SW/"), recursive = TRUE, showWarnings = FALSE)

  ggsave(
    filename = paste0("plots/", park, "/spatial/bathymetry/SW/", name, "-", save_name, "-bathy-panel.png"),
    plot     = p,
    dpi      = 800,
    width    = width,
    height   = height,
    bg       = "white"
  )

  message(save_name, ": max depth = ", max_depth, " m, breaks = ", paste(breaks_park, collapse = ", "))

  return(invisible(p))
}

# ==============================================================================
# 8. INDIVIDUAL PARK BATHYMETRY PANELS (assemble and save)
# ==============================================================================
# Every zoom-in extent below is carried over directly from the
# `plot_limits`/width/height values used for each `network_map_wms_zoomed()`
# call in 10_create-AMP-bathy-maps.R, so the two scripts frame the same parks
# the same way. Run each call, check the console message() for max
# depth/breaks and the saved PNG for overlapping colourbar labels or a
# width/height that no longer suits the bottom (rather than left) legend
# layout, then add an explicit depth_breaks = c(...) / adjust width & height
# for any park where the default overlaps or looks cramped.

# ── Abrolhos ──────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(108.5, 116.1),
  latitude   = c(-30.0, -24.2),
  save_name  = "abrolhos",
  width      = 7,
  height     = 5.5,
  break_step = 1
)

# ── Bremer Bay ────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(119.3, 120.3),
  latitude   = c(-35.3, -33.9),
  save_name  = "bremer-bay",
  width      = 5.7,
  height     = 7,
  break_step = 0.2
)

# ── Eastern Recherche ─────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(123.2, 124.4),
  latitude   = c(-34.9, -33.5),
  save_name  = "eastern-recherche",
  width      = 5.5,
  height     = 6.5,
  break_step = 0.4
)

make_bathy_plot(
  longitude  = c(122.2, 125.3),
  latitude   = c(-37.8, -33.5),
  save_name  = "eastern-recherche-full-extent",
  width      = 5.25,
  height     = 5.5,
  break_step = 1
)

# ── Geographe ─────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(114.8, 115.7),
  latitude   = c(-33.7, -33.2),
  save_name  = "geographe",
  width      = 7,
  height     = 5,
  break_step = 0.1
)

# ── Great Australian Bight ────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(128.7, 132.5),
  latitude   = c(-33.6, -31.3),
  save_name  = "great-australian-bight",
  width      = 7,
  height     = 4.75,
  break_step = 0.5
)

make_bathy_plot(
  longitude  = c(128.7, 132.45),
  latitude   = c(-37.1, -31.3),
  save_name  = "great-australian-bight-full-extent",
  width      = 4.5,
  height     = 5,
  break_step = 1
)

# ── Jurien Bay ────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(114.2, 115.5),
  latitude   = c(-31.0, -30.0),
  save_name  = "jurien-bay",
  width      = 6,
  height     = 5,
  break_step = 0.2
)

# ── Kangaroo Island ───────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(136.0, 137.85),
  latitude   = c(-36.5, -35.5),
  save_name  = "kangaroo-island",
  width      = 6,
  height     = 4,
  break_step = 0.2
)

# ── Murat and Western Eyre ────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(132.45, 135.5),
  latitude   = c(-35.4, -31.9),
  save_name  = "murat-western-eyre",
  width      = 5,
  height     = 5.75,
  break_step = 0.5
)

make_bathy_plot(
  longitude  = c(132.2, 136),
  latitude   = c(-39.3, -31.9),
  save_name  = "murat-western-eyre-full-extent",
  width      = 6,
  height     = 9,
  break_step = 1
)

make_bathy_plot(
  longitude  = c(132.3, 133),
  latitude   = c(-32.9, -32.1),
  save_name  = "murat",
  width      = 6,
  height     = 5.5,
  break_step = 0.2
)

# ── Rottnest Canyon ────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(113.8, 115.8),
  latitude   = c(-32.8, -31.3),
  save_name  = "rottnest-canyon",
  width      = 7,
  height     = 5.75,
  break_step = 0.3
)

# ── SWC East ──────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(120.35, 122.2),
  latitude   = c(-35.5, -33.7),
  save_name  = "swc-east",
  width      = 5.5,
  height     = 6,
  break_step = 0.4
)

make_bathy_plot(
  longitude  = c(120.2, 122.4),
  latitude   = c(-38, -33.7),
  save_name  = "swc-east-full-extent",
  width      = 7,
  height     = 9,
  break_step = 1
)

# ── SWC West ──────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(113.5, 116.4),
  latitude   = c(-34.7857, -33.2643),
  save_name  = "swc-west",
  width      = 6,
  height     = 4.25,
  break_step = 0.3
)

# ── Two Rocks ─────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(114.7, 116.0),
  latitude   = c(-32.0, -31.3),
  save_name  = "two-rocks",
  width      = 9,
  height     = 5,
  break_step = 0.2
)

# ── Twilight MP ───────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(125.2, 127.15),
  latitude   = c(-33.3, -32.1),
  save_name  = "twilight",
  width      = 7,
  height     = 4.75,
  break_step = 0.2
)

# ── SW Network full-extent overview ───────────────────────────────────────────
make_bathy_plot(
  longitude  = c(110, 123),
  latitude   = c(-39, -33),
  save_name  = "sw-full-extent",
  width      = 9,
  height     = 4,
  break_step = 2
)

# ── SWC (South-west Corner) full-extent overview — same extent as the "SWC
#    Full Extent" zoom in 02_create-south-west-network-map-faceted.R ─────────
make_bathy_plot(
  longitude  = c(110, 123),
  latitude   = c(-39, -33),
  save_name  = "swc-full-extent",
  width      = 9,
  height     = 6.5,
  break_step = 1
)
# ==============================================================================
# End of script
# ==============================================================================
