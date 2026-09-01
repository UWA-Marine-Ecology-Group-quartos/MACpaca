###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    Marine Parks, bathymetry, terrestrial parks, aus outline
# Task:    Two Rocks & Geographe zone maps — network style, faceted
# Author:  Annika Leunig
# Date:    May 2026
# Outputs: 1. Two Rocks & Geographe faceted zone map
#          2. Individual park zoom-in zone maps (Abrolhos, Jurien Bay, Two Rocks,
#             Rottnest Island Canyon, Geographe, Bremer Bay, SWC east, Eastern
#             Recherche, Great Australian Bight, Murat & Western Eyre,
#             Kangaroo Island)
###

# Table of contents
#     1.  Set up and load data
#     2.  Helper functions
#     3.  Panel function
#     4.  Build Two Rocks & Geographe panels
#     5.  FIGURE 1: TWO ROCKS & GEOGRAPHE FACETED MAP (assemble and save)
#     6.  Zoom-in map function (legend on left)
#     7.  FIGURES 2-12: Individual park zoom-ins (assemble and save)


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
# Clear environment
rm(list = ls())

# Set study name
name <- "south-west"
park <- "network"

# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(ggnewscale)
library(cowplot)
library(metR)
library(ggpattern)

# Set cropping and plot extents
e                <- ext(106.0, 145.0, -45.0, -22.0)
tworocks_limits  <- c(114.7, 116.0, -32.0, -31.3)
geographe_limits <- c(114.4, 115.9, -33.9, -33.1)

# ── Load spatial files  ───────────────────────────────────────────────────────
sf_use_s2(TRUE)
# Aus outline, terrestrial parks and coastal waters outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid()

# CAPAD Marine 2024 — used for the inset overview maps AND as the source of
# Commonwealth zone RES_NUMBER labels (see below)
capad <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")

# Commonwealth AMP zone labels: last 5 characters of RES_NUMBER (e.g. "npz03"),
# plotted at each zone's CAPAD-supplied LONGITUDE/LATITUDE point
capad_amp_labels <- capad %>%
  st_drop_geometry() %>%
  dplyr::filter(EPBC == "Commonwealth",
                TYPE == "Australian Marine Park",
                RES_NUMBER != "") %>%
  dplyr::mutate(label = stringr::str_sub(RES_NUMBER, -5, -1)) %>%
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

terrnp <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park"))

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e)

# Marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
                            "Ngari Capes", "Geographe", "South-west Corner",
                            "Great Australian Bight", "Jurien", "Murat", "Jurien Bay",
                            "Perth Canyon", "Southern Kangaroo Island", "Twilight",
                            "Two Rocks", "Western Eyre", "Western Kangaroo Island",
                            "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
                            "Investigator", "West coast Bays", "Southern Spencer Gulf",
                            "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest",
                            "Shoalwater Islands", "Shark Bay"))

marine_parks <- marine_parks %>%
  dplyr::mutate(
    zone = dplyr::if_else(
      zone == "Special Purpose Zone" & stringr::str_detect(zone_type, "Mining Exclusion"),
      "Special Purpose Zone (Mining Exclusion)",
      zone
    )
  )

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

amp_zone_levels <- c("National Park Zone",
                     "Habitat Protection Zone",
                     "Multiple Use Zone",
                     "Special Purpose Zone",
                     "Special Purpose Zone (Mining Exclusion)")

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  dplyr::mutate(zone = factor(zone, levels = amp_zone_levels))

amp_zone_colours <- marine_parks_amp %>%
  st_drop_geometry() %>%
  dplyr::distinct(zone, colour) %>%
  dplyr::filter(!is.na(zone)) %>%
  tibble::deframe()
# guarantee a colour exists for every levelamp_zone_colours <- amp_zone_colours[amp_zone_levels]names(amp_zone_colours) <- amp_zone_levelsif (is.na(amp_zone_colours[["Special Purpose Zone (Mining Exclusion)"]])) {
amp_zone_colours[["Special Purpose Zone (Mining Exclusion)"]] <-
  amp_zone_colours[["Special Purpose Zone"]]


amp_pattern_values <- setNames(
  ifelse(amp_zone_levels == "Special Purpose Zone (Mining Exclusion)", "stripe", "none"),
  amp_zone_levels
)

amp_zone_colours <- marine_parks_amp %>%
  st_drop_geometry() %>%
  dplyr::distinct(zone, colour) %>%
  tibble::deframe()

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(
    zone = case_when(
      zone == "Reef Observation Area"   ~ "Sanctuary Zone",
      zone == "National Park Zone"      ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE                              ~ zone
    ),
    colour = case_when(
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      zone == "Sanctuary Zone"               ~ "#bfd4a5",
      TRUE                                   ~ colour
    )
  )

# Bathymetry data
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, values = FALSE)
names(bathy) <- "Depth"

# ==============================================================================
# 2. HELPER FUNCTIONS
# ==============================================================================
filter_to_extent <- function(layer, limits) {
  box <- st_as_sfc(st_bbox(
    c(xmin = limits[1], xmax = limits[2], ymin = limits[3], ymax = limits[4]),
    crs = st_crs(4326)
  ))
  dplyr::filter(layer, st_intersects(geometry, box, sparse = FALSE)[, 1])
}

# to manually set the tick marks for the plots
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# Places each Commonwealth AMP zone code (e.g. "npz04") at the geometric
# centre of ONLY the portion of that zone visible within this panel's
# extent. This guarantees every zone actually drawn on the panel gets a
# label, even when the zone extends beyond plot_limits and its CAPAD-supplied
# LONGITUDE/LATITUDE reference point falls outside the crop (which is what
# was causing labels to silently disappear for zones cut off at the edge).
get_panel_labels <- function(mp_amp, limits) {

  if (nrow(mp_amp) == 0) return(capad_amp_labels[0, ])

  box <- st_as_sfc(st_bbox(
    c(xmin = limits[1], xmax = limits[2], ymin = limits[3], ymax = limits[4]),
    crs = st_crs(4326)
  ))

  # Match each AMP zone to its correct CAPAD zone code using the FULL,
  # uncropped zone geometry. Doing this on cropped fragments instead risks
  # pulling in a NEIGHBOURING zone's label when several zones sit close
  # together (e.g. the hpz03 / muz02 / npz04 cluster near Geographe), since
  # a small cropped sliver of one zone can end up geometrically nearer to
  # a different zone's CAPAD reference point than to its own.
  mp_amp_labelled <- st_join(mp_amp, capad_amp_labels["label"], join = st_nearest_feature)

  # Only now crop to the panel extent, so the label is placed at the centre
  # of whatever portion of that (correctly-identified) zone is visible here
  mp_amp_cropped <- suppressWarnings(st_intersection(mp_amp_labelled, box))

  if (nrow(mp_amp_cropped) == 0) return(capad_amp_labels[0, ])

  mp_amp_cropped %>%
    dplyr::mutate(geometry = st_point_on_surface(geometry)) %>%
    dplyr::select(label)
}

# ==============================================================================
# 3. PANEL FUNCTION
# ==============================================================================
# NOTE: `label_buffer` pads the *visible* map extent slightly beyond
# `plot_limits` so labels sitting near the panel edge aren't sliced off by
# coord_sf(expand = FALSE). Tick breaks are still calculated from the
# original plot_limits, so axis labelling is unaffected. We deliberately do
# NOT use clip = "off" here — with cowplot::plot_grid() assembling panels
# side by side, unclipped content bleeds outside its own panel into
# neighbouring grid cells rather than just showing at the panel edge.

make_zone_panel <- function(plot_limits, mp_amp, mp_state, label_data = NULL,
                            break_step = 0.1, label_buffer = 0.03,
                            legend_amp_zones = amp_zone_levels,
                            legend_state_zones = NULL) {
  x_breaks <- thin_breaks(plot_limits[1:2], step = break_step)
  y_breaks <- thin_breaks(abs(plot_limits[3:4]), step = break_step) * -1

  # Coastal waters limit dummy data for legend
  cwatr_legend <- data.frame(x = 1, y = 1, label = "Coastal Waters Limit")

  if (is.null(label_data)) {
    label_data <- capad_amp_labels[0, ]
  }

  ggplot() +

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

    # Overlay stripes ONLY on Mining Exclusion polygons
    # Australian Marine Parks
    ggpattern::geom_sf_pattern(
      data            = mp_amp,
      aes(fill = zone, pattern = zone),
      colour          = NA, alpha = 0.8,
      pattern_colour  = "white",
      pattern_fill    = "white",
      pattern_density = 0.15,
      pattern_spacing = 0.02,
      pattern_angle   = 45,
      pattern_size    = 0.2,
      key_glyph       = ggpattern::draw_key_polygon_pattern
    ) +
    scale_fill_manual(
      name   = "Australian Marine Parks",
      values = amp_zone_colours,
      limits = legend_amp_zones,
      drop   = TRUE,
      guide  = guide_legend(order = 1, ncol = 1, title.position = "top",
                            override.aes = list(pattern_spacing = 0.01,
                                                pattern_density = 0.15,
                                                pattern_size    = 0.1))
    ) +
    ggpattern::scale_pattern_manual(
      name   = "Australian Marine Parks",
      values = amp_pattern_values,
      limits = legend_amp_zones,
      drop   = TRUE,
      guide  = guide_legend(order = 1, ncol = 1, title.position = "top",
                            override.aes = list(pattern_spacing = 0.01,
                                                pattern_density = 0.15,
                                                pattern_size    = 0.1))
    ) +
    new_scale_fill() +
    # Commonwealth AMP zone labels (last 5 characters of CAPAD RES_NUMBER,
    # e.g. "npz03"), placed at each zone's CAPAD reference point
    geom_sf_text(data          = label_data,
                 aes(label     = label),
                 size          = 2.2,
                 colour        = "grey15",
                 fontface      = "bold",
                 check_overlap = TRUE) +

    # Terrestrial parks
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    scale_fill_manual(name   = "Terrestrial Parks",
                      guide  = guide_legend(order = 2, ncol = 1,
                                            title.position = "top"),
                      values = c("National Park"  = "#c4cea6",
                                 "Nature Reserve" = "#e4d0bb")) +
    new_scale_fill() +

    # State Marine Parks
    geom_sf(data = mp_state, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name   = "State Marine Parks",
                      guide  = guide_legend(order = 2, ncol = 1,
                                            title.position = "top"),
                      values = with(mp_state, setNames(colour, zone)),
                      breaks = if (is.null(legend_state_zones)) waiver() else legend_state_zones) +

    # Coastal waters limit — mapped to colour for legend entry
    geom_sf(data  = cwatr, aes(colour = "Coastal Waters Limit"),
            linewidth = 0.1, lineend = "round") +
    scale_colour_manual(name   = NULL,
                        values = c("Coastal Waters Limit" = "firebrick"),
                        guide  = guide_legend(order = 4,
                                              override.aes = list(linewidth = 0.8))) +

    coord_sf(xlim   = c(plot_limits[1] - label_buffer, plot_limits[2] + label_buffer),
             ylim   = c(plot_limits[3] - label_buffer, plot_limits[4] + label_buffer),
             crs    = 4326, expand = FALSE) +
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
      plot.margin      = margin(4, 4, 4, 4)
    )
}

# ==============================================================================
# 4. BUILD TWO ROCKS & GEOGRAPHE PANELS
# ==============================================================================
# Call functions
tr_amp   <- filter_to_extent(marine_parks_amp,   tworocks_limits)
tr_state <- filter_to_extent(marine_parks_state, tworocks_limits)
tr_labels <- get_panel_labels(tr_amp, tworocks_limits)

geo_amp   <- filter_to_extent(marine_parks_amp,   geographe_limits)
geo_state <- filter_to_extent(marine_parks_state, geographe_limits)
geo_labels <- get_panel_labels(geo_amp, geographe_limits)

# Zones actually drawn somewhere in the figure (both panels combined)
used_amp_zones <- amp_zone_levels[
  amp_zone_levels %in% union(as.character(tr_amp$zone), as.character(geo_amp$zone))
]

state_zone_order <- c("Sanctuary Zone", "General Use Zone",
                      "Recreational Use Zone", "Special Purpose Zone",
                      "Other State Marine Park Zone")

used_state_zones <- state_zone_order[
  state_zone_order %in% union(as.character(tr_state$zone), as.character(geo_state$zone))
]

p_tr  <- make_zone_panel(tworocks_limits,  tr_amp,  tr_state,  label_data = tr_labels,
                         break_step = 0.1, label_buffer = 0.03,
                         legend_amp_zones = used_amp_zones,
                         legend_state_zones = used_state_zones)
p_geo <- make_zone_panel(geographe_limits, geo_amp, geo_state, label_data = geo_labels,
                         break_step = 0.1, label_buffer = 0.03,
                         legend_amp_zones = used_amp_zones,
                         legend_state_zones = used_state_zones)

# Build the legend
legend <- cowplot::get_legend(p_geo + theme(
  legend.position  = "left",
  legend.box       = "vertical",
  legend.direction = "vertical",
  legend.text      = element_text(size = 9),
  legend.title     = element_text(size = 11),
  legend.key.size  = unit(0.5, "cm"),
  legend.spacing.y = unit(0.2, "cm")
))

# Build the inset map
p_inset <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
  geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
  annotate("rect",
           xmin = tworocks_limits[1],  xmax = tworocks_limits[2],
           ymin = tworocks_limits[3],  ymax = tworocks_limits[4],
           colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
  annotate("rect",
           xmin = geographe_limits[1], xmax = geographe_limits[2],
           ymin = geographe_limits[3], ymax = geographe_limits[4],
           colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.3) +
  annotate("text",
           x     = mean(tworocks_limits[1:2]),
           y     = tworocks_limits[4],
           label = "Two Rocks",
           size  = 2.5, colour = "grey20", hjust = 0.5, vjust = -0.5) +
  annotate("text",
           x     = mean(geographe_limits[1:2]),
           y     = geographe_limits[3],
           label = "Geographe",
           size  = 2.5, colour = "grey20", hjust = 0.5, vjust = 1.5) +
  coord_sf(xlim = c(112, 122), ylim = c(-36, -28)) +
  theme_bw() +
  theme(axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.grid.major = element_blank(),
        panel.border     = element_rect(colour = "grey70"))

# ==============================================================================
# 5. FIGURE 1: TWO ROCKS & GEOGRAPHE FACETED MAP (assemble and save)
# ==============================================================================
# ── Assemble ──────────────────────────────────────────────────────────────────
label_tr  <- ggdraw() + draw_label("Two Rocks",  size = 14, angle = 90)
label_geo <- ggdraw() + draw_label("Geographe",  size = 14, angle = 90)

p_tr_nl  <- p_tr  + theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))
p_geo_nl <- p_geo + theme(legend.position = "none", plot.margin = margin(0, 0, 0, 0))

row_tr <- cowplot::plot_grid(
  label_tr, p_tr_nl,
  nrow = 1, rel_widths = c(0.06, 1)
)

row_geo <- cowplot::plot_grid(
  label_geo, p_geo_nl,
  nrow = 1, rel_widths = c(0.06, 1)
)

maps_grid <- cowplot::plot_grid(
  row_tr,
  row_geo,
  ncol        = 1,
  rel_heights = c(1, 1)
)

left_col <- cowplot::plot_grid(
  legend,
  NULL,
  p_inset,
  ncol        = 1,
  rel_heights = c(0.45, 0.15, 0.45)
)

figure <- cowplot::plot_grid(
  left_col,
  maps_grid,
  nrow       = 1,
  rel_widths = c(0.32, 1)
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(5, 5, 5, 5))

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(paste(paste0("plots/", park, "/spatial/", name),
             "tworocks-geographe-MPs.png", sep = "-"),
       plot   = figure,
       dpi    = 600,
       width  = 10.5,
       height = 9,
       bg     = "white")


# ==============================================================================
# 6. ZOOM-IN MAP FUNCTION (LEGEND ON LEFT)
# ==============================================================================
make_zone_plot_left_legend <- function(plot_limits,
                                       inset_xlim   = c(108, 138),
                                       inset_ylim   = c(-40, -24),
                                       break_step   = 0.2,
                                       label_buffer = 0.03,
                                       show_inset   = TRUE,
                                       save_name    = NULL,
                                       width        = 10,
                                       height       = 6) {

  mp_amp    <- filter_to_extent(marine_parks_amp,   plot_limits)
  mp_state  <- filter_to_extent(marine_parks_state, plot_limits)
  mp_labels <- get_panel_labels(mp_amp, plot_limits)

  # Zones actually drawn in this panel only
  used_amp_zones <- amp_zone_levels[
    amp_zone_levels %in% as.character(mp_amp$zone)
  ]

  state_zone_order <- c("Sanctuary Zone", "General Use Zone",
                        "Recreational Use Zone", "Special Purpose Zone",
                        "Other State Marine Park Zone")

  used_state_zones <- state_zone_order[
    state_zone_order %in% as.character(mp_state$zone)
  ]

  p_map <- make_zone_panel(plot_limits, mp_amp, mp_state, label_data = mp_labels,
                           break_step = break_step, label_buffer = label_buffer,
                           legend_amp_zones   = used_amp_zones,
                           legend_state_zones = used_state_zones)

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

 # print(fig)

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
# 7. FIGURES 2-12: INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================
# ── Abrolhos ──────────────────────────────────────────────────────────────────

make_zone_plot_left_legend(
  plot_limits = c(108.5, 116.1, -30, -24.2),
  inset_xlim  = c(108.0, 138.0),
  inset_ylim  = c(-40.0, -24.0),
  break_step  = 0.5,
  show_inset = TRUE,
  save_name   = "abrolhos-MPs",
  width       = 10,
  height      = 6
)

# ── Jurien Bay ────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(114.2, 115.5, -31.0, -30),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "jurien-MPs",
  width       = 8,
  height      = 5
)

# ── Two Rocks ─────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(114.7, 116.0, -32.0, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "tworocks-MPs",
  width       = 9,
  height      = 4.5
)

# ── Rottnest Island Canyon ────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(113.8, 115.8, -32.8, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "rottnest-canyon-MPs",
  width       = 10,
  height      = 6
)

# ── Geographe ─────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(114.8, 115.7, -33.7, -33.2),
  inset_xlim  = c(108.0, 138.0),
  inset_ylim  = c(-40.0, -24.0),
  break_step  = 0.1,
  show_inset = TRUE,
  save_name   = "Geographe-MPs",
  width       = 11,
  height      = 5.5
)

# ── Bremer Bay ────────────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(119.3, 120.3, -35.3, -34),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset  = TRUE,
  save_name   = "bremer-MPs",
  width       = 10.25,
  height      = 9.5
)

# ── SWC Eastern arm ───────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(120.35, 122.2, -35.5, -33.7),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "swc-east-MPs",
  width       = 8,
  height      = 6
)

make_zone_plot_left_legend(
  plot_limits = c(120.2, 122.4, -38, -33.7),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "swc-east-full-extent-MPs",
  width       = 8.5,
  height      = 9
)

# ── Eastern Recherche ─────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(123.2, 124.4, -34.9, -33.5),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "eastern-recherche-MPs",
  width       = 8.5,
  height      = 8
)

make_zone_plot_left_legend(
  plot_limits = c(122.2, 125.3, -37.8, -33.5),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "eastern-recherche_full-extent-MPs",
  width       = 7,
  height      = 8
)


# ── Great Aus Bight ───────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(128.7, 132.4, -33.6, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "great-aus-bight-MPs",
  width       = 10.5,
  height      = 5.5
)

make_zone_plot_left_legend(
  plot_limits = c(128.7, 132.45, -37.1, -31.3),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.5,
  show_inset = TRUE,
  save_name   = "great-aus-bight_full-extent-MPs",
  width       = 10.5,
  height      = 9.5
)
# ── Murat and Western Eyre ────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(132.45, 135.5, -35.4, -31.9),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "murat-western-eyre-MPs",
  width       = 8,
  height      = 7
)

make_zone_plot_left_legend(
  plot_limits = c(132.2, 136, -39.3, -31.9),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.4,
  show_inset = TRUE,
  save_name   = "murat-western-eyre_full-extent_MPs",
  width       = 8,
  height      = 9
)

make_zone_plot_left_legend(
  plot_limits = c(132.3, 132.95, -32.9, -32.1),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.1,
  show_inset = TRUE,
  save_name   = "murat-MPs",
  width       = 7,
  height      = 7
)

# ── Kangaroo Island ───────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(136, 137.85, -36.5, -35.5),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "kangaroo-island-MPs",
  width       = 10.75,
  height      = 5.5
)

# ── Twilight Marine Park ──────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(125.2, 127, -33.3, -32.1),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.2,
  show_inset = TRUE,
  save_name   = "twilight-MPs",
  width       = 10.25,
  height      = 5.5
)

# ── SWC Full Extent ───────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(110, 123, -39, -33),
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 1,
  show_inset = TRUE,
  save_name   = "swc-full-extent-MPs",
  width       = 14,
  height      = 6
)

# ── SWC Western arm ───────────────────────────────────────────────────────────
make_zone_plot_left_legend(
  plot_limits = c(113.5, 116.4, -34.7857, -33.2),
  save_name   = "swc-west_MPs",
  inset_xlim  = c(108, 138),
  inset_ylim  = c(-40, -24),
  break_step  = 0.5,
  show_inset = TRUE,
  width       = 11,
  height      = 5.5
)
# ==============================================================================
# End of script
# ==============================================================================
