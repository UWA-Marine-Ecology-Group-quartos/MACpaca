###
# Project: NESP 5.6 Project - north-west Network Report
# Data:    Marine Parks, Indigenous Protected Areas, bathymetry, terrestrial
#          parks, aus outline
# Task:    North-west network park zone maps — individual zoom-ins
# Author:  Annika Leunig and Abbey Gibbons
# Date:    May 2026
# Outputs: Individual park zoom-in zone maps for the north-west network (Argo-
#          Rowley Terrace, Ashmore Reef, Carnarvon Canyon, Cartier Island,
#          Dampier, Eighty Mile Beach, Gascoyne, Kimberley, Mermaid Reef)
#          Montebello, Ningaloo, Roebuck, Shark Bay,)
###

# Table of contents
#     1.  Set up and load data
#     2.  Helper functions
#     3.  Panel function
#     4.  Zoom-in map function (legend on left)
#     5.  Individual park zoom-ins (assemble and save)


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
# Clear environment
rm(list = ls())

# Set study name
name <- "north-west"
park <- "network"

# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ggnewscale)
library(cowplot)

# Set cropping and plot extents
e                <- ext(108.5, 130, -28, -10)

# Standardised inset extent — used for ALL inset overview plots across every
# individual park zoom-in map, so the inset always shows the same footprint
inset_extent_std <- ext(108.5, 130, -28, -10)

# ── Load spatial files  ───────────────────────────────────────────────────────
sf_use_s2(TRUE)
# Aus outline, terrestrial parks and coastal waters outline
aus <- st_read("data/north-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# CAPAD Marine 2024 — used for the inset overview maps AND as the source of
# Commonwealth zone RES_NUMBER labels (see below)
capad <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")

# Commonwealth AMP zone labels: last 5 characters of RES_NUMBER (e.g. "npz03"),
# plotted at each zone's CAPAD-supplied LONGITUDE/LATITUDE point
capad_amp_labels <- capad %>%
  st_drop_geometry() %>%
  dplyr::filter(EPBC == "Commonwealth",
                TYPE == "Australian Marine Park",
                RES_NUMBER != "") %>%
  dplyr::mutate(label = stringr::str_sub(RES_NUMBER, -5, -1)) %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

terrnp <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

cwatr <- st_read("data/north-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Marine parks — north-west network parks, including Indigenous Protected Areas
marine_parks <- st_read("data/north-west network/spatial/shapefiles/north-west-network-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(
    # Commonwealth AMPs (North-west Network)
    "Argo-Rowley Terrace", "Ashmore Reef", "Carnarvon Canyon", "Cartier Island",
    "Dampier", "Eighty Mile Beach", "Gascoyne", "Kimberley", "Mermaid Reef",
    "Montebello", "Ningaloo", "Roebuck", "Shark Bay",
    # WA state marine parks (Gascoyne–Pilbara–Kimberley)
    "Hamelin Pool", "Muiron Islands", "Barrow Island", "Thevenard Island",
    "Montebello Islands", "Yawuru Nagulagun / Roebuck Bay", "Yawuru",
    "Nyangumarta Warrarn",
    "Lalang-garram / Camden Sound", "Lalang-garram / Horizontal Falls", "North Lalang-garram",
    "North Kimberley",
    "Rowley Shoals", "Scott Reef" ))
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# Indigenous Protected Areas
marine_parks_ipa <- marine_parks %>%
  dplyr::filter(name %in% c("Dhimurru", "Thuwathu/Bujimulla", "Anindilyakwa", "Djelk - Stage 2", "Crocodile Islands Maringa"))

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(
    zone = dplyr::case_when(
      zone == "National Park Zone" & stringr::str_detect(zone_type, "General Use") ~ "General Use Zone",
      zone == "National Park Zone" & stringr::str_detect(zone_type, "Recreation")   ~ "Recreational Use Zone",
      zone == "National Park Zone" & stringr::str_detect(zone_type, "Unassigned")   ~ "Special Purpose Zone",  # or whatever fits your scheme — check what "Unassigned" should map to
      zone == "Reef Observation Area"   ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE                              ~ zone
    ),
    colour = case_when(                                    # REPLACE THIS BLOCK
      zone == "Other State Marine Park Zone" ~ "#FFB6C1",
      zone == "Sanctuary Zone"                ~ "#bfd4a5",
      zone == "General Use Zone"              ~ "#bddde1",
      zone == "Recreational Use Zone"         ~ "#f4e952",
      zone == "Special Purpose Zone"          ~ "#c5bcc9",
      TRUE                                    ~ colour
    )
  )
marine_parks %>%
  st_drop_geometry() %>%
  dplyr::filter(zone == "Special Purpose Zone") %>%
  dplyr::distinct(zone, colour)

# Bathymetry data
bathy <- rast("data/north-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = FALSE)
names(bathy) <- "Depth"

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
filter_to_extent <- function(layer, limits) {
  if (nrow(layer) == 0) return(layer)   # nothing to intersect — return as-is

  box <- st_as_sfc(st_bbox(
    c(xmin = limits[1], xmax = limits[2], ymin = limits[3], ymax = limits[4]),
    crs = st_crs(4326)
  ))

  if (is.na(st_crs(layer))) {
    stop("filter_to_extent(): input layer has no CRS set — assign one with st_set_crs() before filtering.")
  }
  if (st_crs(layer) != st_crs(4326)) {
    layer <- st_transform(layer, 4326)
  }

  layer <- st_make_valid(layer)

  keep <- lengths(st_intersects(layer, box)) > 0   # sparse form — more robust than dense [,1]
  layer[keep, ]
}

# to manually set the tick marks for the plots
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# ==============================================================================
# 3. PANEL FUNCTION
# ==============================================================================

make_zone_panel <- function(plot_limits, mp_amp, mp_state, mp_ipa = NULL,
                            mp_terrnp = NULL, label_data = NULL, break_step = 0.1,
                            state_legend_title = "State Marine Parks") {

  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  if (is.null(label_data)) {
    label_data <- capad_amp_labels[0, ]
  }

  if (is.null(mp_ipa)) {
    mp_ipa <- marine_parks_ipa[0, ]
  }

  if (is.null(mp_terrnp)) {
    mp_terrnp <- terrnp[0, ]
  }

  mp_ipa_recoded <- mp_ipa %>%
    dplyr::mutate(zone = "Indigenous Protected Area", colour = "#FFD8A8")

  mp_state <- dplyr::bind_rows(mp_state, mp_ipa_recoded)

  state_breaks <- c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone",
                    "Special Purpose Zone", "Other State Marine Park Zone")
  if ("Indigenous Protected Area" %in% unique(mp_state$zone)) {
    state_breaks <- c(state_breaks, "Indigenous Protected Area")
  }

  # NEW — restrict to zones actually present in this panel
  state_breaks <- intersect(state_breaks, unique(mp_state$zone))
  p <- ggplot() +

    # Bathymetry filled contours
    geom_spatraster_contour_filled(data   = bathy,
                                   breaks = c(0, -30, -70, -200, -700, -2000, -4000, -6000),
                                   colour = NA, show.legend = FALSE, maxcell = 5e6) +
    scale_fill_manual(values = c("#FFFFFF", "#EFEFEF", "#DEDEDE", "#CCCCCC",
                                 "#B6B6B6", "#9E9E9E", "#808080")) +
    new_scale_fill() +

    # Bathymetry contour lines
    geom_spatraster_contour(data        = bathy,
                            breaks      = c(-30, -70, -200, -700, -2000, -4000, -6000),
                            colour      = "white", alpha = 3/5, linewidth = 0.1,
                            show.legend = FALSE, maxcell = 5e6) +

    # Landmass
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Australian Marine Parks (Commonwealth) — legend sits first (order = 1)
    geom_sf(data = mp_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name   = "Australian Marine Parks",
                      guide  = guide_legend(order = 1, ncol = 1,
                                            title.position = "top"),
                      values = with(mp_amp, setNames(colour, zone)),
                      breaks = sort(unique(mp_amp$zone))) +
    new_scale_fill() +

    # Commonwealth AMP zone labels (last 5 characters of CAPAD RES_NUMBER,
    # e.g. "npz03"), placed at each zone's CAPAD reference point
    geom_sf_text(data          = label_data,
                 aes(label     = label),
                 size          = 2.2,
                 colour        = "grey15",
                 fontface      = "bold",
                 check_overlap = TRUE)

  # Terrestrial parks
  if (nrow(mp_terrnp) > 0) {
    terrnp_types <- intersect(c("National Park", "Nature Reserve"), unique(mp_terrnp$TYPE))

    p <- p +
      geom_sf(data = mp_terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
      scale_fill_manual(name   = "Terrestrial Parks",
                        guide  = guide_legend(order = 3, ncol = 1,
                                              title.position = "top"),
                        values = c("National Park"  = "#c4cea6",   # CHANGED
                                   "Nature Reserve" = "#e4d0bb"),
                        breaks = terrnp_types) +
      new_scale_fill()
  }

  p <- p +

    # State/Territory Marine Parks (incl. Indigenous Protected Areas) —
    geom_sf(data = mp_state, aes(fill = zone), colour = NA, alpha = 0.4) +
    scale_fill_manual(name   = state_legend_title,
                      guide  = guide_legend(order = 4, ncol = 1,
                                            title.position = "top"),
                      values = with(mp_state, setNames(colour, zone)),
                      breaks = state_breaks) +
    new_scale_fill() +

    # Coastal waters limit — mapped to colour for legend entry
    geom_sf(data  = cwatr, aes(colour = "Coastal Waters Limit"),
            linewidth = 0.1, lineend = "round") +
    scale_colour_manual(name   = NULL,
                        values = c("Coastal Waters Limit" = "firebrick"),
                        guide  = guide_legend(order = 5,
                                              override.aes = list(linewidth = 0.8))) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.45, "cm"),
      legend.text      = element_text(size = 8),
      legend.title     = element_text(size = 9),
      legend.position  = "left",
      legend.box       = "vertical",
      legend.direction = "vertical",
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(colour = "grey80", fill = NA, linewidth = 0.5),
      axis.ticks       = element_line(colour = "grey80", linewidth = 0.3),
      axis.text        = element_text(size = 8, colour = "grey40"),
      plot.margin      = margin(2, 2, 2, 2)
    )

  return(p)
}

# ==============================================================================
# 4. ZOOM-IN MAP FUNCTION (LEGEND ON LEFT)
# ==============================================================================
# Function
make_zone_plot_left_legend <- function(plot_limits,
                                       inset_xlim         = c(108.5, 130),
                                       inset_ylim         = c(-28, -10),
                                       break_step         = 0.2,
                                       show_inset         = TRUE,
                                       state_legend_title = "State Marine Parks",
                                       save_name          = NULL,
                                       width              = 10,
                                       height             = 6) {

  mp_amp    <- filter_to_extent(marine_parks_amp,   plot_limits)
  mp_state  <- filter_to_extent(marine_parks_state, plot_limits)
  mp_ipa    <- filter_to_extent(marine_parks_ipa,   plot_limits)
  mp_terrnp <- filter_to_extent(terrnp,             plot_limits)
  mp_labels <- filter_to_extent(capad_amp_labels,   plot_limits)

  p_map <- make_zone_panel(plot_limits, mp_amp, mp_state, mp_ipa = mp_ipa,
                           mp_terrnp = mp_terrnp, label_data = mp_labels,
                           break_step = break_step,
                           state_legend_title = state_legend_title)

  # Legend
  legend_single <- cowplot::get_legend(p_map + theme(
    legend.position  = "left",
    legend.box       = "vertical",
    legend.direction = "vertical",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 11),
    legend.key.size  = unit(0.5, "cm"),
    legend.spacing.y = unit(0.2, "cm")
  ))

  # Inset
  if (show_inset) {

    p_inset_single <- ggplot(data = aus) +
      geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
      geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
      annotate("rect",
               xmin = plot_limits[1], xmax = plot_limits[2],
               ymin = plot_limits[3], ymax = plot_limits[4],
               colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
      coord_sf(xlim = inset_xlim, ylim = inset_ylim) +
      theme_bw() +
      theme(axis.text        = element_blank(),
            axis.ticks       = element_blank(),
            panel.grid.major = element_blank(),
            panel.border     = element_rect(colour = "grey70"))

    left_col_single <- cowplot::plot_grid(
      legend_single,
      NULL,
      p_inset_single,
      ncol        = 1,
      rel_heights = c(1, 0.1, 0.45)
    )

  } else {

    left_col_single <- cowplot::plot_grid(
      legend_single,
      ncol = 1
    )

  }

  # Assemble full figure
  p_map_nl <- p_map + theme(legend.position = "none",
                            plot.margin    = margin(0, 0, 0, 15))

  fig <- cowplot::plot_grid(
    left_col_single,
    p_map_nl,
    nrow       = 1,
    rel_widths = c(0.32, 1)
  ) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin     = margin(5, 5, 5, 5))


  if (!is.null(save_name)) {
    dir.create(paste0("plots/", park, "/spatial/MPA_zoom-ins/"), recursive = TRUE, showWarnings = FALSE)
    ggsave(paste(paste0("plots/", park, "/spatial/MPA_zoom-ins/", name), paste0(save_name, ".png"), sep = "-"),
           plot   = fig,
           dpi    = 600,
           width  = width,
           height = height,
           bg     = "white")
  }

  return(invisible(fig))
}

# ==============================================================================
# 5. INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================
# ── Argo-Rowley Terrace ───────────────────────────────────────────────────────

make_zone_plot_left_legend(
  plot_limits         = c(115.5, 121.0, -18.0, -13.0),
  break_step          = 0.5,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "argo-rowley-terrace-MPs",
  width               = 7.75,
  height              = 4.75
)

# ── Ashmore Reef  ─────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(122.5, 124.0, -13.0, -11.5),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "ashmore-reef-MPs",
  width               = 6.25,
  height              = 4
)

# ── Carnarvon Canyon ──────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(110.0, 112.1, -24.5, -23.0),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "carnarvon-canyon-MPs",
  width               = 8.4,
  height              = 5
)

# ── Cartier Island ────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(123.3, 123.8, -12.7, -12.3),
  break_step          = 0.1,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "cartier-island-MPs",
  width               = 7,
  height              = 4
)

# ── Dampier ───────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(116.6, 117.8, -21.0, -20.0),
  break_step          = 0.1,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "dampier-MPs",
  width               = 7.4,
  height              = 4.25
)

# ── Eighty Mile Beach ─────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(118.2, 122.0, -20.5, -18.0),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "eighty-mile-beach-MPs",
  width               = 9.5,
  height              = 4.5
)

# ── Gascoyne ──────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(109.5, 114.6, -24.2, -20.5),
  break_step          = 0.5,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "gascoyne-MPs",
  width               = 8.5,
  height              = 5.5
)

# ── Kimberley ─────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(120.5, 127.3, -17.5, -13.0),
  break_step          = 0.5,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "kimberley-MPs",
  width               = 8,
  height              = 5.5
)

# ── Mermaid Reef  ─────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(118.7, 119.8, -17.8, -16.7),
  break_step          = 0.1,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "mermaid-reef-MPs",
  width               = 8,
  height              = 5
)

# ── Montebello ────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(114.6, 116.6, -21.6, -19.4),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "montebello-MPs",
  width               = 7.75,
  height              = 5.5
)

# ── Ningaloo ──────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(113, 114.6, -23.7, -21.4),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "ningaloo-MPs",
  width               = 7.75,
  height              = 5.75
)

# ── Roebuck ───────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(121.6, 122.8, -18.7, -17.2),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "roebuck-MPs",
  width               = 7.75,
  height              = 5.5
)

# ── Shark Bay ─────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits         = c(111.5, 114.5, -26.1, -24.1),
  break_step          = 0.2,
  show_inset          = TRUE,
  state_legend_title  = "State Marine Parks",
  save_name           = "shark-bay-MPs",
  width               = 8,
  height              = 4.5
)

# ==============================================================================
# End of script
# ==============================================================================
