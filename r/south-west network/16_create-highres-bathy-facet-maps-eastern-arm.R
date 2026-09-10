###
# Project: NESP 5.6 Project - South-west Corner Report
# Data:    MERI greyscale bathymetry & AMP composite bathymetry (WMS), AMP zone
#          boundaries, coastal waters limit, terrestrial parks, aus outline,
#          Commonwealth marine parks (locator inset), plus high-resolution
#          survey rasters (lidar/multibeam) and their hillshades
# Task:    Combining the wide-area AMP bathymetry context map with high-
#          resolution survey patches (lidar/multibeam) shown inset at their
#          true geographic location, each outlined with a footprint box and
#          labelled (e.g. "a.", "b.")
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. network_map_with_survey_inset() — reusable function, one call
#             per park/region, producing a context map with any number of
#             high-res survey overlays
#          2. survey_spec() — helper to define a single survey overlay
#             (raster, hillshade, palette, extent, label)
#
# ── 2026-09 revision A (per report review comments) ─────────────────────────
#   1. Survey raster layers (hillshade + depth) now draw BEFORE the AMP zone
#      boundaries and coastal waters line, so those boundaries render on top
#      and stay visible over the high-res inset patches, instead of being
#      covered by them.
#   2. Depth legend is now forced to an explicit continuous guide_colorbar()
#      (with defined breaks) rather than relying on an implicit/default guide,
#      which was rendering as discrete categorised swatches instead of a
#      smooth depth scale.
#   3. Footprint box default colour changed from black to purple, so it no
#      longer reads as an overlapping AMP boundary (which is also black).
#      Still fully overridable via the footprint_colour argument.
#
# ── 2026-09 revision B (standalone rewrite) ─────────────────────────────────
# This script previously required the AMP/WMS script and the LiDAR/multibeam
# script to be run first, in the same R session, in the right order — and the
# LiDAR/multibeam script's own rm(list = ls()) plus its own terrnp (DBCA
# version, column `leg_catego`) would silently clobber the aus/terrnp/cwatr/
# amp objects this script actually needs (CAPAD version, column `TYPE`).
#
# Rather than depend on those two large scripts (which each build several
# other figures, palettes, and rasters this script never touches), section 0
# below pulls in ONLY the handful of objects and functions this script
# actually needs, loaded directly from the source shapefiles/rasters:
#   - aus, terrnp, cwatr, capad, amp        <- lifted from the AMP/WMS script's
#     get_meri_grey(), get_amp_bathy()         section 1 & 2, verbatim
#     thin_breaks(), swc_inset_xlim/ylim
#   - make_hillshade()                      <- lifted from the LiDAR/multibeam
#     lidar_swc_east_crop, swc_east_bathy_crop   script's sections 1-4,
#     hill_swc_east, bathy_palette_swc_east      SWC-east lines only
#     swc_east_xlim, swc_east_ylim
# Everything else those two scripts define (marine_parks, aus_hr, the DBCA
# terrnp, the background hillshades, the other regions' rasters/palettes, the
# faceted comparison figures, the AMP mosaic figures, etc.) is NOT needed by
# network_map_with_survey_inset() and has been left out, which is also what
# removes the terrnp/sf_use_s2 clash — we simply never load the other script's
# conflicting versions of those objects.
#
# This file now runs top-to-bottom on its own (given the data files exist at
# the paths below and you have internet access for the WMS/WFS calls). If you
# want to reuse the section-0 setup for other regions/scripts too, it's a
# clean lift-and-shift into its own "00_setup.R" that gets source()'d instead
# — nothing below depends on it being inlined here specifically.
###
# Table of contents
#     0.  Setup — spatial layers, WMS/WFS functions, SWC-east survey rasters
#     1.  survey_spec() — single survey overlay helper
#     2.  network_map_with_survey_inset() — main map function
#     3.  Example usage
# ==============================================================================
# 0. SETUP — SPATIAL LAYERS, WMS/WFS FUNCTIONS, SWC-EAST SURVEY RASTERS
# ==============================================================================
rm(list = ls())
name <- "south-west"
park <- "network"
library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(cowplot)
library(patchwork)
library(scales)
library(png)
library(grid)
sf::sf_use_s2(FALSE)
options(timeout = 1200)
# ── 0a. Context spatial layers (lifted from the AMP/WMS script, section 1) ──
shp_dir <- "data/south-west network/spatial/shapefiles/"
# Cropping box for coastal waters
crop_box <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = 106, ymin = -45, xmax = 145, ymax = -22), crs = 4326
))
# Australia outline
aus <- sf::st_read(paste0(shp_dir, "STE_2021_AUST_GDA2020.shp"), quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)
# CAPAD commonwealth marine parks - for the locator inset only
capad <- sf::st_read(
  paste0(shp_dir, "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp"),
  quiet = TRUE
) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)
# Terrestrial parks (CAPAD version — column `TYPE`, what this script expects)
terrnp <- sf::st_read(
  paste0(shp_dir,
         "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp"),
  quiet = TRUE
) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) %>%
  dplyr::filter(TYPE %in% c("National Park", "Nature Reserve"))
# Coastal waters limit
cwatr <- sf::st_read(paste0(shp_dir, "amb_coastal_waters_limit.shp"), quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)
cwatr <- suppressWarnings(sf::st_intersection(cwatr, crop_box))
# AMP zone boundaries (WFS)
wfs_url <- paste0(
  "https://geoserver.imas.utas.edu.au/geoserver/seamap/ows?",
  "service=WFS",
  "&version=1.0.0",
  "&request=GetFeature",
  "&typeName=SeamapAus_BOUNDARIES_AMP_ZONE",
  "&outputFormat=application/json"
)
amp <- try(sf::st_read(wfs_url, quiet = TRUE), silent = TRUE)
if (inherits(amp, "sf")) amp <- sf::st_transform(amp, 4326)
# ── 0b. WMS download functions + axis-break helper (AMP/WMS script, section 2) ──
get_meri_grey <- function(bbox) {
  meri_url <- paste0(
    "https://geoserver.imas.utas.edu.au/geoserver/wms?",
    "SERVICE=WMS",
    "&VERSION=1.1.1",
    "&REQUEST=GetMap",
    "&LAYERS=seamap:Aus_bathy_grid_MERI",
    "&STYLES=Aus_bathy_grid_MERI_greyscale",
    "&SRS=EPSG:4326",
    "&BBOX=", paste(bbox, collapse = ","),
    "&WIDTH=3000",
    "&HEIGHT=2000",
    "&FORMAT=image/png",
    "&TRANSPARENT=TRUE"
  )
  meri_tmp <- tempfile(fileext = ".png")
  download.file(meri_url, meri_tmp, mode = "wb")
  png::readPNG(meri_tmp)
}
get_amp_bathy <- function(bbox) {
  bathy_url <- paste0(
    "https://geoserver.imas.utas.edu.au/geoserver/wms?",
    "?SERVICE=WMS",        # For some reason this only works for me when WMS? is doubled up -AL
    "&VERSION=1.1.1",
    "&REQUEST=GetMap",
    "&LAYERS=seamap:bathymetry_AMP_grp",
    "&STYLES=",
    "&SRS=EPSG:4326",
    "&BBOX=", paste(bbox, collapse = ","),
    "&WIDTH=3000",
    "&HEIGHT=1800",
    "&FORMAT=image/png",
    "&TRANSPARENT=TRUE"
  )
  bth_tmp <- tempfile(fileext = ".png")
  download.file(bathy_url, bth_tmp, mode = "wb")
  png::readPNG(bth_tmp)
}
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}
# ── 0c. Locator-inset extent (AMP/WMS script's bbox_network) ────────────────
bbox_network   <- c(xmin = 109, ymin = -41, xmax = 139, ymax = -24.05)
swc_inset_xlim <- c(unname(bbox_network["xmin"]), unname(bbox_network["xmax"]))
swc_inset_ylim <- c(unname(bbox_network["ymin"]), unname(bbox_network["ymax"]))
# ── 0d. Hillshade helper (LiDAR/multibeam script, section 3) ────────────────
make_hillshade <- function(bathy_rast, altitude = 40, azimuth = 270) {
  slope  <- terrain(bathy_rast, v = "slope",  unit = "radians")
  aspect <- terrain(bathy_rast, v = "aspect", unit = "radians")
  hill   <- shade(slope, aspect, angle = altitude, direction = azimuth, normalize = TRUE)
  names(hill) <- "hillshade"
  hill
}
# ── 0e. SWC-east survey rasters only (LiDAR/multibeam script, sections 1-4) ──
# NOTE: only the two rasters, extent, and palette this script's example call
# actually uses are loaded here. For a different region/survey, load and crop
# your own raster(s) the same way and build a fresh survey_spec() (see
# section 3) — you don't need to touch section 0 to do that.
swc_east_xlim <- c(120.6, 121.4)
swc_east_ylim <- c(-34.15, -33.75)
e_swc_east <- ext(swc_east_xlim[1], swc_east_xlim[2], swc_east_ylim[1], swc_east_ylim[2])
# SWC LiDAR (2024) - DoT south coastal LiDAR, cropped to the eastern arm
lidar_swc_raw <- rast("data/south-west network/spatial/rasters/DoT_south-coastal-lidar.tif")
lidar_swc     <- -lidar_swc_raw
lidar_swc     <- clamp(lidar_swc, upper = 0, values = FALSE)
names(lidar_swc) <- "depth"
lidar_swc_east_crop <- crop(lidar_swc, e_swc_east)
# SWC east multibeam (2024, Wudjari survey)
swc_east_bathy <- rast("data/south-west network/spatial/rasters/UWA-EGS_AU038423-Wudjari-r50-SB-FP_rev1.tiff")
swc_east_bathy <- project(swc_east_bathy, "EPSG:7844", method = "bilinear")
swc_east_bathy <- -swc_east_bathy
swc_east_bathy <- clamp(swc_east_bathy, upper = 0, values = FALSE)
names(swc_east_bathy) <- "depth"
swc_east_bathy_crop <- crop(swc_east_bathy, e_swc_east)
# Hillshade for the LiDAR crop
hill_swc_east <- make_hillshade(lidar_swc_east_crop)
# Colour palette (LiDAR/multibeam script, section 4)
v <- scales::viridis_pal(option = "viridis")(100)
bathy_palette_swc_east <- colorRampPalette(c(
  v[1], v[16], v[30], v[44], v[58], v[72], v[83], v[92], v[100]
))(500)
# ==============================================================================
# network_map_with_survey_inset()
# ------------------------------------------------------------------------------
# Drops one or more high-resolution survey rasters (lidar/multibeam + hillshade)
# into their true geographic position on top of the wide-area MERI/AMP-bathy
# context map, with an outlined footprint box and a letter label for each
# survey (e.g. "a.").
#
# Because everything is already in EPSG:4326, no manual inset positioning is
# needed — the high-res raster is just an additional annotation_raster/
# geom_spatraster layer at its own xlim/ylim, and it appears "inset" simply
# because its extent is smaller than the surrounding context map.
#
# Requires (all loaded in section 0 above):
#   aus, terrnp, cwatr, amp, capad
#   get_meri_grey(), get_amp_bathy(), thin_breaks()
# ==============================================================================
# ── Helper: one survey overlay spec ───────────────────────────────────────────
# Build one of these per high-res patch you want to drop into the map.
#   depth_rast    : cropped depth SpatRaster (layer named "depth")
#   hill_rast     : hillshade SpatRaster for depth_rast (from make_hillshade())
#   palette       : colour ramp (e.g. bathy_palette_swc_east)
#   depth_limits  : c(min, max) for scale_fill_gradientn
#   xlim, ylim    : the survey's own extent (used for the footprint box + label)
#   label         : text to draw at the top-right corner of the footprint, e.g. "a."
#   depth_rast2   : OPTIONAL second depth raster layered on top of depth_rast,
#                   within the SAME box/label (e.g. a multibeam survey that
#                   covers more of the same extent than a LiDAR survey does —
#                   mirrors make_panel()'s depth_rast2 argument in the
#                   LiDAR/multibeam script). Use this instead of a second
#                   survey_spec() when two rasters share the same footprint.
survey_spec <- function(depth_rast, hill_rast, palette, depth_limits,
                        xlim, ylim, label, depth_rast2 = NULL) {
  list(
    depth_rast   = depth_rast,
    depth_rast2  = depth_rast2,
    hill_rast    = hill_rast,
    palette      = palette,
    depth_limits = depth_limits,
    xlim         = xlim,
    ylim         = ylim,
    label        = label
  )
}
# ── Main function ──────────────────────────────────────────────────────────
network_map_with_survey_inset <- function(
    plot_limits,
    surveys         = list(),   # list of survey_spec() objects
    inset_xlim      = c(108, 138),
    inset_ylim      = c(-40, -24),
    show_inset      = TRUE,
    thin_lon_breaks = FALSE,
    break_step      = 0.2,
    show_footprint_box   = TRUE,
    show_footprint_label = TRUE,
    label_size      = 4,
    label_fontface  = "bold",
    label_colour    = "black",
    footprint_colour = "purple",
    footprint_linewidth = 0.4,
    save_name       = NULL,
    width           = 10,
    height          = 6
) {
  bbox <- c(
    xmin = plot_limits[1], ymin = plot_limits[3],
    xmax = plot_limits[2], ymax = plot_limits[4]
  )
  xmin <- bbox["xmin"]; xmax <- bbox["xmax"]
  ymin <- bbox["ymin"]; ymax <- bbox["ymax"]
  x_breaks <- if (thin_lon_breaks) {
    thin_breaks(c(unname(xmin), unname(xmax)), step = break_step)
  } else {
    pretty(c(unname(xmin), unname(xmax)), n = 5)
  }
  # ── Base context layer (MERI greyscale + AMP composite bathy) ───────────────
  meri_img <- get_meri_grey(bbox)
  bath_img <- get_amp_bathy(bbox)
  terr_fills_ordered <- scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 2, ncol = 1)
  )
  p_map <- ggplot() +
    annotation_raster(meri_img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +
    annotation_raster(bath_img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered
  # ── Layer in each high-res survey raster ─────────────────────────────────────
  # NOTE: these are added BEFORE the AMP zone boundaries and coastal waters
  # line (below) so that those boundaries draw on top of, and stay visible
  # over, the high-res inset patches rather than being hidden underneath them.
  for (s in surveys) {
    # A single shared colourbar is used when a survey has two depth rasters
    # (depth_rast + depth_rast2) covering the same footprint/palette/limits —
    # the first raster's scale is hidden (guide = "none") and the second
    # carries the (now always-continuous) legend, so it isn't drawn twice.
    depth_guide <- guide_colorbar(
      barheight    = unit(4, "cm"),
      barwidth     = unit(0.4, "cm"),
      ticks.colour = "grey20",
      frame.colour = "grey20",
      order        = 1
    )
    depth_breaks <- scales::breaks_pretty(n = 5)(s$depth_limits)
    p_map <- p_map +
      new_scale_fill() +
      geom_spatraster(data = s$hill_rast, aes(fill = hillshade),
                      alpha = 0.45, show.legend = FALSE) +
      scale_fill_gradient(low = "#1a1a2e", high = "#ffffff",
                          na.value = NA, guide = "none") +
      new_scale_fill() +
      geom_spatraster(data = s$depth_rast, aes(fill = depth), alpha = 1) +
      scale_fill_gradientn(colours = s$palette, limits = s$depth_limits,
                           oob = scales::squish, na.value = NA,
                           breaks = depth_breaks,
                           # Continuous colourbar, explicitly — only suppressed
                           # here when a second depth raster below will carry
                           # the (identical) legend instead.
                           guide = if (is.null(s$depth_rast2)) depth_guide else "none",
                           name  = "Depth (m)")
    # Optional second depth raster — same footprint/box, layered on top
    # (e.g. a multibeam survey covering more of the same extent than the
    # LiDAR does within it). Mirrors make_panel()'s depth_rast2 in the
    # LiDAR/multibeam script.
    if (!is.null(s$depth_rast2)) {
      p_map <- p_map +
        new_scale_fill() +
        geom_spatraster(data = s$depth_rast2, aes(fill = depth), alpha = 1) +
        scale_fill_gradientn(colours = s$palette, limits = s$depth_limits,
                             oob = scales::squish, na.value = NA,
                             breaks = depth_breaks,
                             guide  = depth_guide,
                             name   = "Depth (m)")
    }
  }
  # ── AMP zone boundaries + coastal waters line ────────────────────────────────
  # Drawn AFTER the survey rasters above so they sit on top of, and remain
  # visible over, the high-res inset patches.
  p_map <- p_map +
    geom_sf(data = cwatr, fill = NA, colour = "firebrick",
            linewidth = 0.15, lineend = "round") +
    { if (exists("amp") && inherits(amp, "sf"))
      geom_sf(data = amp, fill = NA, colour = "black", linewidth = 0.15) }
  # ── Footprint box + corner label for each survey ─────────────────────────────
  # Drawn last so the footprint outline is always crisp and legible on top of
  # everything else, including the AMP/coastal-waters lines above.
  for (s in surveys) {
    if (show_footprint_box) {
      p_map <- p_map +
        annotate("rect",
                 xmin = s$xlim[1], xmax = s$xlim[2],
                 ymin = s$ylim[1], ymax = s$ylim[2],
                 colour = footprint_colour, fill = NA,
                 linewidth = footprint_linewidth)
    }
    if (show_footprint_label) {
      p_map <- p_map +
        annotate("text",
                 x = s$xlim[2] - 0.03 * diff(s$xlim),
                 y = s$ylim[2] - 0.06 * diff(s$ylim),
                 label = s$label, fontface = label_fontface,
                 size = label_size, colour = label_colour, hjust = 1)
    }
  }
  # ── Finish styling on the combined plot ──────────────────────────────────────
  p_map <- p_map +
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size   = unit(0.5, "cm"),
      legend.text       = element_text(size = 9),
      legend.title      = element_text(size = 11),
      legend.position   = "left",
      legend.box        = "vertical",
      legend.direction  = "vertical",
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
      axis.ticks        = element_line(colour = "grey80", linewidth = 0.3)
    )
  # ── Legend (terrestrial parks) ────────────────────────────────────────────
  legend_single <- cowplot::get_legend(
    p_map + theme(
      legend.position   = "left",
      legend.box        = "vertical",
      legend.direction  = "vertical",
      legend.key.size   = unit(0.5, "cm"),
      legend.spacing.y  = unit(0.2, "cm")
    )
  )
  # ── Locator inset (unchanged from network_map_wms_zoomed) ───────────────────
  if (show_inset) {
    p_inset <- ggplot(data = aus) +
      geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 0.8) +
      geom_sf(data = capad, colour = "grey85", linewidth = 0.02, alpha = 0.8) +
      annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
               colour = "grey25", fill = "white", alpha = 0.2, linewidth = 0.3) +
      coord_sf(xlim = inset_xlim, ylim = inset_ylim) +
      theme_bw() +
      theme(axis.text = element_blank(), axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.border = element_rect(colour = "grey70"))
    left_col <- cowplot::plot_grid(
      legend_single, NULL, p_inset,
      ncol = 1, rel_heights = c(1, 0.1, 0.45)
    )
  } else {
    left_col <- cowplot::plot_grid(legend_single, ncol = 1)
  }
  # ── Assembly ──────────────────────────────────────────────────────────────
  p_map_nl <- p_map + theme(legend.position = "none", plot.margin = margin(0, 0, 0, 15))
  fig <- cowplot::plot_grid(
    left_col, p_map_nl,
    nrow = 1, rel_widths = c(0.32, 1)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin     = margin(5, 5, 5, 5))
  if (!is.null(save_name)) {
    out_dir <- paste0("plots/", park, "/spatial/AMP_bathy/")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(paste0(out_dir, name, "-", save_name, ".png"),
           plot = fig, dpi = 600, width = width, height = height, bg = "white")
  }
  invisible(fig)
}
# ==============================================================================
# EXAMPLE USAGE — SWC eastern arm, using the rasters loaded in section 0
# ==============================================================================
swc_east_survey <- survey_spec(
  depth_rast   = lidar_swc_east_crop,
  depth_rast2  = swc_east_bathy_crop,
  hill_rast    = hill_swc_east,
  palette      = bathy_palette_swc_east,
  depth_limits = c(-78, 0),
  xlim         = swc_east_xlim,
  ylim         = swc_east_ylim,
  label        = "a."
)
network_map_with_survey_inset(
  plot_limits = c(120.2, 122.4, -35.5, -33.6),
  surveys     = list(swc_east_survey),
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim,
  break_step  = 0.2,
  save_name   = "swc-east_survey-inset",
  width       = 9,
  height      = 7
)
# ── Same map, no footprint box (letter label kept) — for comparison ─────────
network_map_with_survey_inset(
  plot_limits = c(120.2, 122.4, -35.5, -33.6),
  surveys     = list(swc_east_survey),
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim,
  break_step  = 0.2,
  show_footprint_box = FALSE,
  save_name   = "swc-east_survey-inset_no-box",
  width       = 9,
  height      = 7
)
# ==============================================================================
# End of script
# ==============================================================================
