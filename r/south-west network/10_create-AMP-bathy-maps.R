###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    IMAS WMS (MERI greyscale + AMP composite bathymetry), aus outline,
#          CAPAD marine (inset), terrestrial parks (CAPAD), coastal waters
#          limit, AMP zone boundaries (WFS), saved depth-legend image.
# Task:    Plot AMP mosaics on network and individual MP extents
# Author:  Annika Leunig
# Date:    June 2026
# Outputs: 1. South-west network AMP bathymetry map
#          2. Individual park zoom-in AMP bathymetry maps (Abrolhos, Bremer Bay,
#             Eastern Recherche, Geographe, Great Australian Bight, Jurien Bay,
#             Kangaroo Island, Murat & Western Eyre, Rottnest Canyon, SWC east,
#             SWC west, Two Rocks, Twilight)
###

# Table of contents
#     1.  Set up and load data
#     2.  WMS download functions
#     3.  Network figure builder (fixed Aus-extent inset)
#     4.  FIGURE 1: SOUTH-WEST NETWORK MAP (assemble and save)
#     5.  Zoom-in map function (legend on left)
#     6.  FIGURES 2-14: Individual park zoom-ins (assemble and save)


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
# Clear environment
rm(list = ls())

# Set study name
name <- "south-west"
park <- "network"

# Load libraries
library(sf)
library(ggplot2)
library(dplyr)
library(png)
library(grid)
library(gridExtra)
library(patchwork)
library(cowplot)

sf::sf_use_s2(FALSE)
options(timeout = 1200)

# ── Paths (adjust if your tree differs) ───────────────────────────────────────
shp_dir          <- "data/south-west network/spatial/shapefiles/"
depth_legend_png <- "data/south-west network/spatial/rasters/depth-legend.png"

# ── Map extents (xmin, ymin, xmax, ymax) ──────────────────────────────────────
bbox_network <- c(xmin = 109,    ymin = -41,   xmax = 139,   ymax = -24.05)

swc_inset_xlim <- c(unname(bbox_network["xmin"]), unname(bbox_network["xmax"]))
swc_inset_ylim <- c(unname(bbox_network["ymin"]), unname(bbox_network["ymax"]))

# Cropping box for all layers
crop_box <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = 106, ymin = -45, xmax = 145, ymax = -22), crs = 4326
))

# ── Load spatial files ────────────────────────────────────────────────────────
# Australia outline
aus <- sf::st_read(paste0(shp_dir, "STE_2021_AUST_GDA2020.shp"), quiet = TRUE) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# CAPAD commonwealth marine parks - for the inset only
capad <- sf::st_read(
  paste0(shp_dir, "Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp"),
  quiet = TRUE
) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326)

# Terrestrial parks
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

# ==============================================================================
# 2. WMS DOWNLOAD FUNCTIONS
# ==============================================================================
# ── MERI greyscale base ───────────────────────────────────────────────────────
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

# ── AMP composite bathymetry ──────────────────────────────────────────────────
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

# ── Helper: thinned axis breaks (keeps every 2nd break at a fixed step) ───────
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}


# ==============================================================================
# 3. NETWORK FIGURE BUILDER (FIXED AUS-EXTENT INSET)
# ==============================================================================
amp_bathy_map <- function(bbox, meri_img, bath_img, x_breaks = NULL, y_breaks = NULL) {

  xmin <- unname(bbox["xmin"]); xmax <- unname(bbox["xmax"])
  ymin <- unname(bbox["ymin"]); ymax <- unname(bbox["ymax"])

  if (is.null(x_breaks)) x_breaks <- pretty(c(xmin, xmax), n = 4)
  if (is.null(y_breaks)) y_breaks <- pretty(c(ymin, ymax), n = 4)

  terr_fills_ordered <- scale_fill_manual(
    values = c("National Park" = "#c4cea6", "Nature Reserve" = "#e4d0bb"),
    name   = "Terrestrial Parks",
    guide  = guide_legend(order = 2, ncol = 1)
  )

  # Dpeth legend
  legend_img  <- png::readPNG(depth_legend_png)
  img_h_px    <- nrow(legend_img)
  img_w_px    <- ncol(legend_img)
  plot_h_in   <- 6 * (4 / 5) * 0.70
  legend_w_in <- plot_h_in * (img_w_px / img_h_px)

  legend_grob <- grid::rasterGrob(
    legend_img, interpolate = TRUE,
    x      = unit(0.5, "npc"),
    y      = unit(1,   "npc"),
    width  = unit(legend_w_in, "inches"),
    height = unit(plot_h_in,   "inches"),
    just   = c("centre", "top")
  )

  title_grob <- grid::textGrob(
    "Composite bathymetry \nmosaics per Australian \nMarine Park (AMP)\n",
    x    = unit(0, "npc"),
    y    = unit(0.5, "npc"),
    just = c("left", "centre"),
    gp   = grid::gpar(fontsize = 8, fontface = "plain")
  )

  right_grob <- gridExtra::arrangeGrob(
    title_grob,
    legend_grob,
    nrow    = 2,
    heights = grid::unit(c(0.18, 0.82), "npc")
  )

  # Main map MERI bathy and AMP mosaics
  p1 <- ggplot() +

    # MERI greyscale base
    annotation_raster(meri_img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +

    # AMP composite bathymetry
    annotation_raster(bath_img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) +

    # Land
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +

    # Terrestrial parks
    geom_sf(data = terrnp, aes(fill = TYPE), colour = NA, alpha = 0.8) +
    terr_fills_ordered +

    # Coastal waters limit
    geom_sf(data = cwatr, fill = NA, colour = "firebrick",
            linewidth = 0.2, lineend = "round") +

    # AMP zone boundaries
    {if (inherits(amp, "sf"))
      geom_sf(data = amp, fill = NA, colour = "black", linewidth = 0.15)} +

    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), crs = 4326, expand = FALSE) +
    scale_x_continuous(
      name = NULL, breaks = x_breaks,
      labels = function(x) paste0(x, "°E")
    ) +
    scale_y_continuous(
      name = NULL, breaks = y_breaks,
      labels = function(y) paste0(abs(y), "°S")
    ) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white"),
      panel.border     = element_rect(colour = "grey70", fill = NA, linewidth = 0.3),
      panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
      axis.text        = element_text(size = 10),
      legend.key.size  = unit(0.4, "cm"),
      legend.text      = element_text(size = 7),
      legend.title     = element_text(size = 8),
      legend.position  = "bottom",
      legend.box       = "horizontal",
      legend.direction = "vertical"
    )

  # TPs legend
  p1_other_legend <- cowplot::get_legend(p1 + theme(
    legend.text      = element_text(size = 7),
    legend.title     = element_text(size = 8),
    legend.key.size  = unit(0.3, "cm"),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    legend.direction = "vertical"
  ))

  # Inset
  p1.1 <- ggplot(data = aus) +
    geom_sf(fill = "seashell1", colour = "grey90", linewidth = 0.05, alpha = 4/5) +
    geom_sf(data = capad, alpha = 5/6, colour = "grey85", linewidth = 0.02) +
    coord_sf(xlim = c(105, 160), ylim = c(-48, -8)) +
    annotate("rect",
             xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
             colour = "grey25", fill = "white", alpha = 1/5, linewidth = 0.2) +
    theme_bw() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid.major = element_blank(),
          panel.border     = element_rect(colour = "grey70"))

  # Assembly
  p1_no_legend <- p1 + theme(legend.position = "none",
                             plot.margin     = margin(0, 0, 15, 0))

  right_panel      <- wrap_elements(right_grob)
  right_panel_w_in <- 1.8

  map_row <- p1_no_legend + right_panel +
    plot_layout(widths = c(4, right_panel_w_in / (11 - right_panel_w_in) * 4))

  bottom_row <- p1.1 + wrap_elements(p1_other_legend) + plot_spacer() +
    plot_layout(widths = c(0.28, 1, 0.5))

  map_row / bottom_row +
    plot_layout(heights = c(4, 1))
}

# ==============================================================================
# 4. FIGURE 1: SOUTH-WEST NETWORK MAP (assemble and save)
# ==============================================================================
# SAve function
build_and_save <- function(bbox, save_name, width, height,
                           x_breaks = NULL, y_breaks = NULL) {
  meri_img <- get_meri_grey(bbox)
  bath_img <- get_amp_bathy(bbox)
  fig <- amp_bathy_map(bbox, meri_img, bath_img, x_breaks, y_breaks)
  ggsave(paste(paste0("plots/", park, "/spatial/AMP_bathy/", name),
               paste0(save_name, ".png"), sep = "-"),
         plot = fig, dpi = 600, width = width, height = height, bg = "white")
  invisible(fig)
}

# Save
build_and_save(bbox_network,
               "network_AMP-bathy-plot",  width = 8.5, height = 6,
               x_breaks = seq(110, 140, by = 5),
               y_breaks = seq(-40, -25, by = 5))


# ==============================================================================
# 5. ZOOM-IN MAP FUNCTION (LEGEND ON LEFT)
# ==============================================================================
network_map_wms_zoomed <- function(
    plot_limits,
    inset_xlim = c(108, 138),
    inset_ylim = c(-40, -24),
    show_inset = TRUE,
    thin_lon_breaks = FALSE,   # thin the longitude axis breaks (useful for wide/short extents)
    break_step      = 0.2,     # spacing used by thin_breaks() before thinning; only used when thin_lon_breaks = TRUE
    save_name  = NULL,
    width      = 10,
    height     = 6,
    depth_bar_height_in = 3.2   # fixed physical height of the depth legend bar,
    # same for every park unless overridden per call
) {

  bbox <- c(
    xmin = plot_limits[1],
    ymin = plot_limits[3],
    xmax = plot_limits[2],
    ymax = plot_limits[4]
  )

  xmin <- bbox["xmin"]
  xmax <- bbox["xmax"]
  ymin <- bbox["ymin"]
  ymax <- bbox["ymax"]

  # Thinned lon breaks for wide/short extents, ggplot default breaks otherwise
  x_breaks <- if (thin_lon_breaks) {
    thin_breaks(c(unname(xmin), unname(xmax)), step = break_step)
  } else {
    pretty(c(unname(xmin), unname(xmax)), n = 5)
  }

  # Download rasters
  meri_img <- get_meri_grey(bbox)
  bath_img <- get_amp_bathy(bbox)

  # TPs
  terr_fills_ordered <- scale_fill_manual(
    values = c(
      "National Park" = "#c4cea6",
      "Nature Reserve" = "#e4d0bb"
    ),
    name = "Terrestrial Parks",
    guide = guide_legend(order = 2, ncol = 1)
  )

  # main map
  p_map <- ggplot() +

    # MERI bathy greyscale
    annotation_raster(
      meri_img,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ) +

    # AMP mosaics
    annotation_raster(
      bath_img,
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    ) +

    # Land
    geom_sf(
      data = aus,
      fill = "seashell2",
      colour = "grey80",
      linewidth = 0.1
    ) +

    # TPs
    geom_sf(
      data = terrnp,
      aes(fill = TYPE),
      colour = NA,
      alpha = 0.8
    ) +

    terr_fills_ordered +

    # Coastal waters
    geom_sf(
      data = cwatr,
      fill = NA,
      colour = "firebrick",
      linewidth = 0.15,
      lineend = "round"
    ) +

    # AMP zone boundaries
    {
      if (exists("amp") && inherits(amp, "sf"))
        geom_sf(
          data = amp,
          fill = NA,
          colour = "black",
          linewidth = 0.15
        )
    } +

    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      crs = 4326,
      expand = FALSE
    ) +

    scale_x_continuous(breaks = x_breaks) +   # thinned or default lon breaks

    labs(x = NULL, y = NULL) +

    theme_minimal() +

    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 11),
      legend.position = "left",
      legend.box = "vertical",
      legend.direction = "vertical",
      panel.grid = element_blank(),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.border = element_rect(
        colour = "grey80",
        fill = NA,
        linewidth = 0.5
      ),
      axis.ticks = element_line(
        colour = "grey80",
        linewidth = 0.3
      )
    ) +

    guides(fill = guide_legend(ncol = 1))

  # Depth legend (same image as Figure 1)
  legend_img  <- png::readPNG(depth_legend_png)
  img_h_px    <- nrow(legend_img)
  img_w_px    <- ncol(legend_img)
  img_ratio   <- img_w_px / img_h_px   # width:height

  # Build as a tiny ggplot in relative (0-1) coordinates so cowplot can
  # scale it via rel_heights, instead of a fixed-inch grob that can get
  # clipped/misplaced when the allocated cell size differs (as it did here).
  depth_legend_panel <- ggplot() +
    annotation_raster(
      legend_img,
      xmin = 0, xmax = img_ratio,
      ymin = 0, ymax = 1,
      interpolate = TRUE
    ) +
    coord_cartesian(xlim = c(0, img_ratio), ylim = c(0, 1), expand = FALSE) +
    theme_void()

  # Title text: "Composite bathymetry mosaics per Australian Marine Park
  # (AMP)" — same as Figure 1. Drawn directly onto the canvas below,
  # right-anchored (hjust = 1) so it grows leftward toward the map instead
  # of overflowing off the right edge of the figure.

  # Column widths used in the final row assembly further down — defined
  # here too so the depth-bar sizing below can compute the legend column's
  # actual width in inches and stay in sync with the real layout.
  col_rel_widths <- c(left = 0.24, map = 0.9, spacer = 0.1, legend = 0.12)

  # Fixed physical size for the bar (same across every park, regardless of
  # that park's map height) — this is the key change from before, where the
  # bar stretched to fill the full column height and ended up wildly
  # different sizes across parks with very different `height` values.
  #
  # cowplot::draw_plot() only accepts numeric (0-1) fractions of its canvas
  # for width/height, not grid::unit() objects — so the desired absolute
  # inch size has to be converted into a fraction of the actual column
  # dimensions (in inches) here.
  col_width_in  <- width  * (col_rel_widths["legend"] / sum(col_rel_widths))
  col_height_in <- height   # this column spans the full row height

  bar_height_in <- depth_bar_height_in
  bar_width_in  <- bar_height_in * img_ratio   # preserves the image's aspect ratio

  bar_height_frac <- min(1, bar_height_in / col_height_in)
  bar_width_frac  <- min(1, bar_width_in  / col_width_in)
  # if bar_width_frac hit the cap of 1, the bar is wider than its column
  # allows at this height — either shrink depth_bar_height_in for this
  # call, or widen col_rel_widths["legend"] below

  # Small fixed gap between the title and the top of the bar, and a rough
  # estimate of the title's own height (3 lines at size 8) — both in
  # inches, used below to centre the whole title+bar block in the column.
  title_gap_in    <- 0.15
  title_height_in <- 0.65

  block_height_in <- min(col_height_in, bar_height_in + title_gap_in + title_height_in)
  y_offset_in     <- max(0, (col_height_in - block_height_in) / 2)
  y_offset_frac   <- y_offset_in / col_height_in

  title_y_frac <- min(0.99, (bar_height_in + title_gap_in + y_offset_in) / height)

  # Combine title + bar as a single ggdraw canvas rather than a nested
  # plot_grid — this gives direct control over exact placement. Both are
  # shifted up by y_offset_in so the whole block sits centred in the
  # column instead of pinned to the bottom.
  depth_legend_with_title <- cowplot::ggdraw() +
    cowplot::draw_plot(
      depth_legend_panel,
      x = 0, y = y_offset_frac,
      width  = bar_width_frac,
      height = bar_height_frac
    ) +
    cowplot::draw_label(
      "Composite bathymetry\nmosaics per Australian\nMarine Park (AMP)",
      x = 0.95, y = title_y_frac,
      hjust = 1, vjust = 0,
      size = 8,
      fontface = "plain"
    )

  # Extract TP legend
  legend_single <- cowplot::get_legend(
    p_map +
      theme(
        legend.position = "left",
        legend.box = "vertical",
        legend.direction = "vertical",
        legend.key.size = unit(0.5, "cm"),
        legend.spacing.y = unit(0.2, "cm")
      )
  )

  # Inset map
  if (show_inset) {

    p_inset <- ggplot(data = aus) +

      geom_sf(
        fill = "seashell1",
        colour = "grey90",
        linewidth = 0.05,
        alpha = 0.8
      ) +

      geom_sf(
        data = capad,
        colour = "grey85",
        linewidth = 0.02,
        alpha = 0.8
      ) +

      annotate(
        "rect",
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        colour = "grey25",
        fill = "white",
        alpha = 0.2,
        linewidth = 0.3
      ) +

      coord_sf(
        xlim = inset_xlim,
        ylim = inset_ylim
      ) +

      theme_bw() +

      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70")
      )

  }

  # --- Assembly: Option D -----------------------------------------------
  # Left: TP legend + inset stacked. Middle: map. Right: depth legend
  # (full map height), on the opposite side from the other two.
  p_map_nl <- p_map +
    theme(
      legend.position = "none",
      plot.margin = margin(0, 0, 0, 15)
    )

  if (show_inset) {

    left_col <- cowplot::plot_grid(
      legend_single,
      p_inset,
      ncol = 1,
      rel_heights = c(1, 0.7)   # TP legend vs inset height balance
    )

  } else {

    left_col <- cowplot::plot_grid(
      legend_single,
      ncol = 1
    )

  }

  fig <- cowplot::plot_grid(
    left_col,
    p_map_nl,
    NULL,
    depth_legend_with_title,
    nrow = 1,
    rel_widths = unname(col_rel_widths)   # left column, map, spacer, depth legend
  )

  fig <- fig +
    theme(
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      plot.margin = margin(5, 5, 5, 5)
    )

  # Save
  if (!is.null(save_name)) {

    ggsave(paste(paste0("plots/",park,"/spatial/AMP_bathy/", name),
                 paste0(save_name, ".png"),sep = "-"),
           plot = fig, dpi = 600, width = width, height = height, bg = "white")
  }

  invisible(fig)
}
# ==============================================================================
# 6. FIGURES 2-14: INDIVIDUAL PARK ZOOM-INS (assemble and save)
# ==============================================================================
# ── Abrolhos ──────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(108.5, 116.1, -30.0, -24.2),
  save_name   = "abrolhos_AMP-bathy-plot",
  width       = 9,
  height      = 5.5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Bremer Bay ────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(119.3, 120.3, -35.3, -33.9),
  save_name   = "bremer_AMP-bathy-plot",
  width       = 8,
  height      = 8,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Eastern Recherche ─────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(123.2, 124.4, -34.9, -33.5),
  save_name   = "eastern-recherche_AMP-bathy-plot",
  width       = 7,
  height      = 6.5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

network_map_wms_zoomed(
  plot_limits = c(123, 124.6, -37.8, -33.5),
  save_name   = "eastern-recherche_full-extent_AMP-bathy-plot",
  thin_lon_breaks = TRUE,
  break_step      = 0.4,
  width       = 4.5,
  height      = 6.5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)
# ── Geographe ─────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(114.8, 115.7, -33.7, -33.2),
  save_name   = "geographe_AMP-bathy-plot",
  width       = 10,
  height      = 5.5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Great Australian Bight ────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(128.7, 132.5, -33.6, -31.3),
  save_name   = "great-aus-bight_AMP-bathy-plot",
  width       = 9,
  height      = 5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

network_map_wms_zoomed(
  plot_limits = c(128.7, 132.5, -37.8, -31.3),
  save_name   = "great-aus-bight_full-extent_AMP-bathy-plot",
  thin_lon_breaks = TRUE,
  break_step      = 1,
  width       = 7,
  height      = 5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Jurien Bay ────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(114.2, 115.5, -31.0, -30.0),
  save_name   = "jurien_AMP-bathy-plot",
  width       = 8,
  height      = 5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Kangaroo Island ───────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(136.0, 137.85, -36.5, -35.5),
  save_name   = "kangaroo-island_AMP-bathy-plot",
  width       = 9,
  height      = 6,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Murat and Western Eyre ────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(132.45, 135.5, -35.4, -31.9),
  save_name   = "murat-western-eyre_AMP-bathy-plot",
  width       = 8,
  height      = 7,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

network_map_wms_zoomed(
  plot_limits = c(132.45, 135.5, -39.4, -31.9),
  save_name   = "murat-western-eyre_full-extent_AMP-bathy-plot",
  thin_lon_breaks = TRUE,
  break_step      = 0.5,
  width       = 7,
  height      = 9,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

network_map_wms_zoomed(
  plot_limits = c(132.3, 133, -33.2, -32.2),
  save_name   = "murat_AMP-bathy-plot",
  thin_lon_breaks = TRUE,
  break_step      = 0.1,
  width       = 8,
  height      = 7,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Rottnest Island Canyon ────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(113.8, 115.8, -32.8, -31.3),
  save_name   = "rottnest-canyon_AMP-bathy-plot",
  width       = 10,
  height      = 6,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── SWC Eastern arm ───────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(120.35, 122.2, -35.5, -33.7),
  save_name   = "swc-east_AMP-bathy-plot",
  width       = 8,
  height      = 6,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── SWC Western arm ───────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(113.5, 116.4, -34.7857, -33.2643),
  save_name   = "swc-west_AMP-bathy-plot",
  width       = 9,
  height      = 4.5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Two Rocks ─────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(114.7, 116.0, -32.0, -31.3),
  save_name   = "two-rocks_AMP-bathy-plot",
  width       = 11.5,
  height      = 5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ── Twilight MP ─────────────────────────────────────────────────────────────────
network_map_wms_zoomed(
  plot_limits = c(125.2, 127.15, -33.3, -32.1),
  save_name   = "twilight_AMP-bathy-plot",
  width       = 9,
  height      = 5,
  inset_xlim  = swc_inset_xlim,
  inset_ylim  = swc_inset_ylim
)

# ==============================================================================
# End of script
# ==============================================================================
