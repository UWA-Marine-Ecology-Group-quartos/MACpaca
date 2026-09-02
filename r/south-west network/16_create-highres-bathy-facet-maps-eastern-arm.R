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
# NOTE: this script depends on objects already loaded by two other scripts —
# run those first, in this session, before sourcing this one:
#   1. The AMP bathymetry/WMS script (defines aus, terrnp, cwatr, amp, capad,
#      get_meri_grey(), get_amp_bathy(), thin_breaks())
#   2. The lidar/multibeam bathymetry script (defines the cropped depth
#      rasters, hillshades, and colour palettes used to build survey_spec()
#      objects, e.g. lidar_swc_east_crop, hill_swc_east, bathy_palette_swc_east)
# The second script currently starts with rm(list = ls()) and also defines its
# own terrnp (DBCA version, column `leg_catego`) which will silently overwrite
# the first script's terrnp (CAPAD version, column `TYPE`) that this function
# expects — remove the rm(list = ls()) call and rename that script's terrnp to
# avoid the clash before running all three in sequence.
###
# Table of contents
#     1.  survey_spec() — single survey overlay helper
#     2.  network_map_with_survey_inset() — main map function
#     3.  Example usage

# ==============================================================================
# network_map_with_survey_inset()
# ------------------------------------------------------------------------------
# Extends network_map_wms_zoomed() (script 3, section 5) to drop one or more
# high-resolution survey rasters (lidar/multibeam + hillshade) into their true
# geographic position on top of the wide-area MERI/AMP-bathy context map, with
# an outlined footprint box and a letter label for each survey (e.g. "a.").
#
# Because everything is already in EPSG:4326, no manual inset positioning is
# needed — the high-res raster is just an additional annotation_raster/
# geom_spatraster layer at its own xlim/ylim, and it appears "inset" simply
# because its extent is smaller than the surrounding context map.
#
# Requires objects already loaded in your session from scripts 3 and 4:
#   aus, terrnp, cwatr, amp, capad          (script 3)
#   get_meri_grey(), get_amp_bathy()        (script 3)
#   thin_breaks()                           (script 3)
#   bathy palettes / hillshade helper       (script 4, make_hillshade())
# ==============================================================================

library(sf)
library(terra)
library(tidyterra)
library(ggplot2)
library(ggnewscale)
library(cowplot)
library(patchwork)

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
#                   mirrors make_panel()'s depth_rast2 argument in script 4).
#                   Use this instead of a second survey_spec() when two
#                   rasters share the same footprint/extent.
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
    label_size      = 4,
    label_fontface  = "bold",
    label_colour    = "black",
    footprint_colour = "black",
    footprint_linewidth = 0.3,
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
    terr_fills_ordered +

    geom_sf(data = cwatr, fill = NA, colour = "firebrick",
            linewidth = 0.15, lineend = "round") +

    { if (exists("amp") && inherits(amp, "sf"))
      geom_sf(data = amp, fill = NA, colour = "black", linewidth = 0.15) }

  # ── Layer in each high-res survey raster at its true extent ─────────────────
  for (s in surveys) {

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
                           # Only show a legend here if there's no second
                           # raster to carry it instead — avoids duplicating
                           # the colourbar when both share the same palette.
                           guide = if (is.null(s$depth_rast2)) guide_colorbar() else "none",
                           name  = "Depth (m)")

    # Optional second depth raster — same footprint/box, layered on top
    # (e.g. a multibeam survey covering more of the same extent than the
    # LiDAR does within it). Mirrors make_panel()'s depth_rast2 in script 4.
    if (!is.null(s$depth_rast2)) {
      p_map <- p_map +
        new_scale_fill() +
        geom_spatraster(data = s$depth_rast2, aes(fill = depth), alpha = 1) +
        scale_fill_gradientn(colours = s$palette, limits = s$depth_limits,
                             oob = scales::squish, na.value = NA,
                             name = "Depth (m)")
    }

    p_map <- p_map +
      # Footprint outline
      annotate("rect",
               xmin = s$xlim[1], xmax = s$xlim[2],
               ymin = s$ylim[1], ymax = s$ylim[2],
               colour = footprint_colour, fill = NA,
               linewidth = footprint_linewidth) +

      # Corner label (e.g. "a.")
      annotate("text",
               x = s$xlim[2] - 0.03 * diff(s$xlim),
               y = s$ylim[2] - 0.06 * diff(s$ylim),
               label = s$label, fontface = label_fontface,
               size = label_size, colour = label_colour, hjust = 1)
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
    ) +
    guides(fill = guide_legend(ncol = 1))

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
    ggsave(paste(paste0("plots/", park, "/spatial/AMP_bathy/", name),
                 paste0(save_name, ".png"), sep = "-"),
           plot = fig, dpi = 600, width = width, height = height, bg = "white")
  }

  invisible(fig)
}

# ==============================================================================
# EXAMPLE USAGE — SWC eastern arm, reusing rasters already built in script 4
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
# ==============================================================================
