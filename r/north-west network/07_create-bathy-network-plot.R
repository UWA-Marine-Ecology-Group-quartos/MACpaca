###
# Project: NESP 5.6 Project - North-west Network Report
# Data:    New bathymetry data (2024), marine park shapefiles, terrestrial
#          parks and aus outline
# Task:    Create network-scale and individual park bathymetry maps
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. North-west network bathymetry map (fixed 0-7000 m scale)
#          2. Individual park bathymetry maps (Argo-Rowley Terrace, Ashmore
#             Reef, Carnarvon Canyon, Cartier Island, Dampier, Eighty Mile
#             Beach, Gascoyne, Kimberley, Mermaid Reef, Montebello, Ningaloo,
#             Roebuck, Shark Bay) — each with a colour scale fitted to its
#             own depth range, with manually overridable breaks to avoid
#             overlapping colourbar labels
###

# Table of contents
#     1.  Set up and load libraries
#     2.  Load spatial files
#     3.  Hillshade and axis break helper
#     4.  Define colour ramp (network-scale, fixed)
#     5.  FIGURE 1: North-west Network
#     6.  Dynamic colour scale helpers (per-park depth range)
#     7.  Individual park bathymetry panel function
#     8.  Individual park bathymetry panels (assemble and save)

# ==============================================================================
# 1. SET UP AND LOAD
# ==============================================================================

# Clear the environment
rm(list = ls())

# Set the study name
name <- "north-west"
park <- "network"

# Load libraries
library(sf)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggnewscale)
library(RColorBrewer)

# Set cropping extent (matches the north-west network KEF/SST/natural-values scripts)
e <- ext(106, 133, -28, -11)

# Progress bar for raster operations
terraOptions(progress = 3)
sf_use_s2(TRUE)

# ==============================================================================
# 2. LOAD SPATIAL FILES
# ==============================================================================
# Terrestrial parks using CAPAD
terrnp <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

# Aus Outline
aus <- st_read("data/north-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# Marine parks — Commonwealth AMPs + WA state marine parks (same list as the
# north-west network KEF script's network_map() filter)
marine_parks <- st_read("data/north-west network/spatial/shapefiles/nw-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(# Commonwealth AMPs (North-west Network)
    "Argo-Rowley Terrace", "Ashmore Reef", "Carnarvon Canyon", "Cartier Island",
    "Dampier", "Eighty Mile Beach", "Gascoyne", "Kimberley", "Mermaid Reef",
    "Montebello", "Ningaloo", "Roebuck", "Shark Bay",
    # WA state marine parks (Gascoyne-Pilbara-Kimberley)
    "Hamelin Pool", "Muiron Islands", "Barrow Island", "Thevenard Island",
    "Montebello Islands", "Yawuru Nagulagun / Roebuck Bay", "Yawuru", # IPA
    "Nyangumarta Warrarn", # IPA
    "Bardi Jawi Gaarra", "North Kimberley", "Mayala",
    "Lalang-gaddam", "Rowley Shoals", "Scott Reef"))

# Bathymetry layer
bathy <- rast("data/north-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
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

# to manually set the tick marks for the plots (same helper as the zone-map script)
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# ==============================================================================
# 4. DEFINE COLOUR RAMP (NETWORK-SCALE, FIXED)
# ==============================================================================
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
# 5. FIGURE 1: North-west Network
# ==============================================================================
# ── Set up ────────────────────────────────────────────────────────────────────
names(bathy) <- "depth"
names(hill)  <- "hillshade"

bathy_abs <- bathy * -1
names(bathy_abs) <- "depth_abs"

xlim_shared <- c(109, 130)
ylim_shared <- c(-26.5, -12.0)

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
# Only unhash below if folder doesn't exist yet
# dir.create(paste0("plots/", park, "/spatial/bathymetry/"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = paste0("plots/", park, "/spatial/bathymetry/", name, "-network-bathy-panel.png"),
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

  dir.create(paste0("plots/", park, "/spatial/bathymetry/"), recursive = TRUE, showWarnings = FALSE)

  ggsave(
    filename = paste0("plots/", park, "/spatial/bathymetry/", name, "-", save_name, "-bathy-panel.png"),
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
# Run each call, check the console message() for max depth/breaks and the
# saved PNG for overlapping colourbar labels, then add an explicit
# depth_breaks = c(...) argument to any park where the default overlaps.

# ── Argo-Rowley Terrace ───────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(115.5, 121.0),
  latitude   = c(-18.0, -13.0),
  save_name  = "argo-rowley-terrace",
  width      = 5,
  height     = 6,
  break_step = 0.5,
  depth_breaks = c(0, 200, 2000, 4000, 6000)
)

# ── Ashmore Reef  ─────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(122.5, 124.0),
  latitude   = c(-13.0, -11.5),
  save_name  = "ashmore-reef",
  width      = 5,
  height     = 6,
  break_step = 0.2,
  depth_breaks = c(0, 200, 1000,2000)
)

# ── Carnarvon Canyon ──────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(110.0, 112.1),
  latitude   = c(-24.5, -23.0),
  save_name  = "carnarvon-canyon",
  width      = 8,
  height     = 7,
  break_step = 0.2,
  depth_breaks = c(0, 200, 2500, 5000)
)

# ── Cartier Island ────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(123.3, 123.8),
  latitude   = c(-12.7, -12.3),
  save_name  = "cartier-island",
  width      = 6,
  height     = 6.5,
  break_step = 0.1,
  depth_breaks = c(0, 200, 400, 600)
)

# ── Dampier ───────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(116.6, 117.8),
  latitude   = c(-21.0, -20.0),
  save_name  = "dampier",
  width      = 6,
  height     = 6.5,
  break_step = 0.2,
  depth_breaks = c(0, 25, 50, 75, 100)
)

# ── Eighty Mile Beach ─────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(118.2, 122.0),
  latitude   = c(-20.5, -18.0),
  save_name  = "eighty-mile-beach",
  width      = 7,
  height     = 6,
  break_step = 0.4,
  depth_breaks = c(0, 200, 400, 600)
)

# ── Gascoyne ──────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(109.5, 114.6),
  latitude   = c(-24.2, -20.5),
  save_name  = "gascoyne",
  width      = 7.5,
  height     = 7,
  break_step = 0.5,
  depth_breaks = c(0, 200, 2000, 4000, 5700)
)

# ── Kimberley ─────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(120.5, 127.3),
  latitude   = c(-17.5, -13.0),
  save_name  = "kimberley",
  width      = 8,
  height     = 6,
  break_step = 0.4,
  depth_breaks = c(0, 200, 1500, 2700)
)

# ── Mermaid Reef  ─────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(118.7, 119.8),
  latitude   = c(-17.8, -16.7),
  save_name  = "mermaid-reef",
  width      = 5,
  height     = 6,
  break_step = 0.2,
  depth_breaks = c(0, 200, 800, 1600)
)

# ── Montebello ────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(114.6, 116.6),
  latitude   = c(-21.6, -19.4),
  save_name  = "montebello",
  width      = 5,
  height     = 7,
  break_step = 0.2,
  depth_breaks = c(0, 200, 800, 1500)
)

# ── Ningaloo ──────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(113.0, 114.6),
  latitude   = c(-23.7, -21.4),
  save_name  = "ningaloo",
  width      = 5,
  height     = 8.3,
  break_step = 0.2,
  depth_breaks = c(0, 200, 1500, 2700)
)

# ── Roebuck ───────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(121.6, 122.8),
  latitude   = c(-18.7, -17.2),
  save_name  = "roebuck",
  width      = 5,
  height     = 7,
  break_step = 0.2,
  depth_breaks = c(0, 50, 100, 150, 200)
)

# ── Shark Bay ─────────────────────────────────────────────────────────────────
make_bathy_plot(
  longitude  = c(111.5, 114.5),
  latitude   = c(-26.1, -24.1),
  save_name  = "shark-bay",
  width      = 7,
  height     = 6,
  break_step = 0.2,
  depth_breaks = c(0, 200, 1000, 1900)
)

# ==============================================================================
# End of script
# ==============================================================================
