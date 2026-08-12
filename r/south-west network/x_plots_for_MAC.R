###
# Project: Plots for MAC
# Data:    Natural values ecosystems, predicted reef, marine park shapefiles,
#          terrestrial parks and aus outline
# Task:    Bathymetry and benthic habitat maps with sanctuary zone overlays
# Author:  Annika Leunig
# Date:    May 2026
# Outputs: 1.  Benthic habitat with closures — WA south extent
#          2.  Benthic habitat with closures — WA north extent
#          3.  Benthic habitat with closures — combined extent
#          4.  Bathymetry with closures — WA south extent
#          5.  Bathymetry with closures — WA north extent
#          6.  Bathymetry with closures — faceted park panels
#          7.  WA overview — sidebar legend
#          8.  WA overview — inside legend
#          9.  South-west Corner zoom
#          10. Jurien Bay zoom
#          11. Rockingham zoom
#          12. Abrolhos zoom
#          13. Gascoyne overview
#          14. Gascoyne zoomed
###

# Table of contents
#     1.  Set up and load data
#     2.  Calculate hillshade
#     3.  Lookup tables and colour palettes
#     4.  Functions (benthic closures maps)
#     5.  FIGURES 1-3:   Benthic habitat with closures overlayed maps
#     6.  FIGURES 4-5:   Bathymetry with closures maps
#     7.  FIGURE 6:      Bathymetry with closures — faceted park panels
#     8.  Load Gascoyne spatial data
#     9.  Additional map functions (bathy overview and zoom-in maps)
#     10. FIGURES 7-8:   WA overview maps
#     11. FIGURES 9-12:  Individual park zoom-ins
#     12. FIGURES 13-14: Gascoyne maps


# ==============================================================================
# 1. LOAD DATA AND SETUP
# ==============================================================================

# Clear environment
rm(list = ls())

# Set study name
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
library(tidyr)

sf_use_s2(TRUE)

# Set CRS and crop extent
target_crs <- "EPSG:4326"
e_wgs84    <- ext(108.0, 118.0, -36.0, -26.0)

# Output directory
out_dir <- file.path("plots", park, "spatial", "Plots_for_MAC")

# Bathymetry
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e_wgs84) %>%
  project(target_crs, method = "bilinear")

# Natural values ecosystem (27 classes)
naturalvalues <- rast("data/south-west network/spatial/rasters/ecosystem-types-27class-naland.tif") %>%
  crop(e_wgs84) %>%
  project(target_crs, method = "near")

# Predicted reef habitat (binomial probability surface)
predictedhabitat <- rast("data/south-west network/spatial/rasters/binomial_preds_reef_range_multi.tif") %>%
  crop(e_wgs84) %>%
  project(target_crs, method = "near")

# Verify all rasters are in WGS84
stopifnot(
  grepl("4326", crs(bathy,            describe = TRUE)$code),
  grepl("4326", crs(naturalvalues,    describe = TRUE)$code),
  grepl("4326", crs(predictedhabitat, describe = TRUE)$code)
)

# Clip natural values to 250 m shelf
mask_250              <- ifel(bathy >= -250, 1, NA)
mask_250_resamp       <- resample(mask_250, naturalvalues, method = "near")
naturalvalues_clipped <- mask(naturalvalues, mask_250_resamp)

# Aus outline
aus <- st_read("data/south-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid() %>%
  st_transform(4326)

# Terrestrial parks
terrnp <- st_read("data/south-west network/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park")) %>%
  st_make_valid() %>%
  st_transform(4326)

# Marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c(
    "Abrolhos", "Abrolhos Islands", "Bremer", "Eastern Recherche",
    "Ngari Capes", "Geographe", "South-west Corner",
    "Great Australian Bight", "Jurien", "Murat", "Jurien Bay",
    "Perth Canyon", "Southern Kangaroo Island", "Twilight",
    "Two Rocks", "Western Eyre", "Western Kangaroo Island",
    "Nuyts Archipelgo", "Thorny Passage", "Sir Joseph Banks Group",
    "Investigator", "West coast Bays", "Southern Spencer Gulf",
    "Upper Spencer Gulf", "Cottesloe Reef", "Rottnest", "Shoalwater Islands"
  )) %>%
  st_make_valid() %>%
  st_transform(4326)

# Sanctuary / no-take zones
mp_state_sanctuary <- marine_parks %>%
  dplyr::filter(epbc == "State",
                zone %in% c("Reef Observation Area", "Sanctuary Zone"))

mp_cwlth_sanctuary <- marine_parks %>%
  dplyr::filter(epbc == "Commonwealth",
                zone %in% c("National Park Zone"))


# ==============================================================================
# 2. CALCULATE HILLSHADE
# ==============================================================================

slope     <- terrain(bathy, v = "slope",  unit = "radians")
aspect    <- terrain(bathy, v = "aspect", unit = "radians")
hs        <- shade(slope, aspect, angle = 40, direction = 270, normalize = TRUE)
names(hs) <- "hillshade"


# ==============================================================================
# 3. LOOKUP TABLES AND COLOUR PALETTES
# ==============================================================================

# Natural values class names
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

# Benthic habitat colours
benthic_colours <- c(
  "Shelf unvegetated sediments"    = "cornsilk1",
  "Shelf vegetated sediments"      = "seagreen3",
  "Shallow reefs"                  = "darkgoldenrod1",
  "Mesophotic reefs"               = "khaki4",
  "Rariphotic reefs"               = "steelblue3",
  "Upper slope reefs"              = "indianred3",
  "Mid slope reefs"                = "palevioletred3",
  "Seamount reefs"                 = "mediumpurple3",
  "Upper slope sediments"          = "wheat1",
  "Mid slope sediments"            = "navajowhite1",
  "Lower slope reef and sediments" = "lightsteelblue2",
  "Abyssal reef and sediments"     = "slategrey",
  "Seamount sediments"             = "rosybrown2",
  "Shelf incising canyons"         = "grey50"
)

# Closure zone colours
closure_colours_state <- c(
  "Sanctuary Zone"        = "#fc887c",
  "Reef Observation Area" = "#ff4430"
)

closure_colours_cwlth <- c(
  "National Park Zone" = "#ffc8c2"
)

# Bathymetry colour palette (viridis-derived)
v             <- scales::viridis_pal(option = "viridis")(100)
bathy_palette <- colorRampPalette(c(
  v[1],  v[3],  v[6],  v[9],  v[12], v[15], v[18], v[22], v[26], v[30],
  v[34], v[38], v[42], v[46], v[52], v[58], v[65], v[72], v[79], v[86],
  v[92], v[96], v[100]
))(500)

# Shelf-clamped bathy and hillshade data frames (used by make_bathy_closures_map)
bathy_shelf        <- clamp(bathy, lower = -200, upper = 0, values = FALSE)
names(bathy_shelf) <- "depth"
bathy_df           <- as.data.frame(bathy_shelf, xy = TRUE, na.rm = TRUE)
colnames(bathy_df)[3] <- "depth"

hs_df <- as.data.frame(hs, xy = TRUE, na.rm = TRUE)
colnames(hs_df)[3] <- "hillshade"


# ==============================================================================
# 4. FUNCTIONS
# ==============================================================================

# --- Helper function ---
thin_breaks <- function(limits, step = 0.2) {
  b <- seq(from = floor(min(limits)   / step) * step,
           to   = ceiling(max(limits) / step) * step,
           by   = step)
  b[seq(1, length(b), by = 2)]
}

# --- Helper: reference lines clipped to water shallower than 200 m ------------
make_reference_lines <- function(limits,
                                 lats      = c(-26.5, -28, -31, -33),
                                 lons      = c(115.5),
                                 lon_ymax  = -34,     # northern cutoff for vertical lines
                                 lat_xmax  = c("-26.5" = 113.8),  # per-lat eastern cutoff
                                 depth_min = -250,
                                 depth_max = 0,
                                 spacing   = 0.002,   # sampling step, degrees
                                 min_pts   = 10) {    # drop slivers shorter than this

  build_one <- function(coord, type) {

    pts <- if (type == "lat") {
      x_end <- limits[2]
      key   <- as.character(coord)
      if (!is.null(lat_xmax) && key %in% names(lat_xmax)) {
        x_end <- min(x_end, lat_xmax[[key]])
      }
      if (x_end <= limits[1]) return(NULL)
      data.frame(x = seq(limits[1], x_end, by = spacing), y = coord)
    } else {
      y_end <- min(limits[4], lon_ymax)
      if (y_end <= limits[3]) return(NULL)
      data.frame(x = coord, y = seq(limits[3], y_end, by = spacing))
    }

    pts$depth <- terra::extract(bathy[[1]], as.matrix(pts[, c("x", "y")]))[, 1]
    pts$keep  <- !is.na(pts$depth) & pts$depth <= depth_max & pts$depth >= depth_min

    # remove any points falling inside land polygons
    if (any(pts$keep)) {
      pts_sf  <- st_as_sf(pts[pts$keep, c("x", "y")], coords = c("x", "y"), crs = 4326)
      on_land <- lengths(st_intersects(pts_sf, aus)) > 0
      pts$keep[pts$keep] <- !on_land
    }

    if (!any(pts$keep)) return(NULL)

    r       <- rle(pts$keep)
    pts$run <- rep(seq_along(r$lengths), r$lengths)

    out  <- pts[pts$keep, ]
    keep_runs <- names(which(table(out$run) >= min_pts))
    out  <- out[as.character(out$run) %in% keep_runs, ]
    if (nrow(out) == 0) return(NULL)

    data.frame(x     = out$x,
               y     = out$y,
               group = paste(type, coord, out$run, sep = "_"))
  }

  lats <- lats[lats >= limits[3] & lats <= limits[4]]
  lons <- lons[lons >= limits[1] & lons <= limits[2]]

  res <- c(lapply(lats, build_one, type = "lat"),
           lapply(lons, build_one, type = "lon"))
  res <- res[!vapply(res, is.null, logical(1))]

  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

# --- Helper: scale bar drawn in data coordinates ------------------------------
add_scalebar <- function(p, limits,
                         breaks_km = c(0, 20, 50, 100),
                         x_frac    = 0.03,   # left edge of bar, fraction of panel width
                         y_frac    = 0.03,   # base of bar, fraction of panel height
                         bar_h     = 0.008,  # bar height, fraction of panel height
                         text_size = 3,
                         box       = TRUE) {

  xr <- limits[2] - limits[1]
  yr <- limits[4] - limits[3]

  x0 <- limits[1] + x_frac * xr
  y0 <- limits[3] + y_frac * yr

  # km -> degrees longitude at the scale bar's latitude
  deg_per_km <- 1 / (111.320 * cos(y0 * pi / 180))
  xs   <- x0 + breaks_km * deg_per_km
  h    <- bar_h * yr
  barw <- max(xs) - x0

  segs <- data.frame(
    xmin = head(xs, -1),
    xmax = tail(xs, -1),
    fill = rep(c("grey15", "white"), length.out = length(xs) - 1),
    stringsAsFactors = FALSE
  )

  lab <- as.character(breaks_km)
  lab[length(lab)] <- paste(lab[length(lab)], "km")

  if (box) {
    p <- p + annotate("rect",
                      xmin      = x0 - 0.07 * barw,
                      xmax      = max(xs) + 0.20 * barw,
                      ymin      = y0 - 1.4 * h,
                      ymax      = y0 + 4.6 * h,
                      fill      = "white",
                      colour    = "grey60",
                      linewidth = 0.35)
  }

  p +
    annotate("rect",
             xmin = segs$xmin, xmax = segs$xmax,
             ymin = y0,        ymax = y0 + h,
             fill = segs$fill, colour = "grey15", linewidth = 0.25) +
    annotate("text",
             x = xs, y = y0 + 1.6 * h, label = lab,
             size = text_size, colour = "grey15", vjust = 0, hjust = 0.5)
}

# --- Classify benthic habitat per cell ---
classify_benthic <- function(nv_val, depth_val, prob_val,
                             reef_threshold = 0.5,
                             depth_breaks   = c(shallow    = -30,
                                                mesophotic = -70,
                                                rariphotic = -200)) {
  dplyr::case_when(
    !is.na(prob_val) & prob_val >= reef_threshold &
      !is.na(depth_val) & depth_val >= depth_breaks["shallow"] & depth_val <= 0
    ~ "Shallow reefs",

    !is.na(prob_val) & prob_val >= reef_threshold &
      !is.na(depth_val) & depth_val >= depth_breaks["mesophotic"] & depth_val < depth_breaks["shallow"]
    ~ "Mesophotic reefs",

    !is.na(prob_val) & prob_val >= reef_threshold &
      !is.na(depth_val) & depth_val >= depth_breaks["rariphotic"] & depth_val < depth_breaks["mesophotic"]
    ~ "Rariphotic reefs",

    !is.na(nv_val) & nv_val == 1  ~ "Shelf unvegetated sediments",
    !is.na(nv_val) & nv_val == 9  ~ "Shelf vegetated sediments",
    !is.na(nv_val) & nv_val == 16 ~ "Upper slope reefs",
    !is.na(nv_val) & nv_val == 17 ~ "Mid slope reefs",
    !is.na(nv_val) & nv_val == 18 ~ "Seamount reefs",
    !is.na(nv_val) & nv_val == 2  ~ "Upper slope sediments",
    !is.na(nv_val) & nv_val == 3  ~ "Mid slope sediments",
    !is.na(nv_val) & nv_val == 4  ~ "Lower slope reef and sediments",
    !is.na(nv_val) & nv_val == 5  ~ "Abyssal reef and sediments",
    !is.na(nv_val) & nv_val == 6  ~ "Seamount sediments",
    !is.na(nv_val) & nv_val == 7  ~ "Shelf incising canyons",

    TRUE ~ NA_character_
  )
}

# --- FUNCTION 1: Hillshade + benthic habitat + closure zones ---
make_benthic_closures_map <- function(plot_limits,
                                      reef_threshold  = 0.5,
                                      depth_breaks    = c(shallow    = -30,
                                                          mesophotic = -70,
                                                          rariphotic = -200),
                                      show_legend     = TRUE,
                                      legend_position = "bottomleft",
                                      title           = NULL,
                                      show_ref_lines  = TRUE,
                                      ref_lats        = c(-26.5, -28, -31, -33),
                                      ref_lons        = c(115.5),
                                      ref_lon_ymax    = -34,
                                      ref_lat_xmax    = c("-26.5" = 113.5),
                                      ref_colour      = "#1f4fd8",
                                      ref_linetype    = "dashed",
                                      ref_linewidth   = 0.45,
                                      show_scalebar   = TRUE,
                                      scalebar_x      = 0.03,
                                      scalebar_y      = NULL) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale)

  ext_plot <- ext(plot_limits[1], plot_limits[2], plot_limits[3], plot_limits[4])

  # ── 1. Crop rasters ──────────────────────────────────────────────────────────
  bathy_crop <- crop(bathy,                 ext_plot)
  nv_crop    <- crop(naturalvalues_clipped, ext_plot)
  ph_crop    <- crop(predictedhabitat,      ext_plot)
  hs_crop    <- crop(hs,                    ext_plot)

  ph_resamp  <- resample(ph_crop, bathy_crop, method = "bilinear")
  ph_resamp  <- mask(ph_resamp, resample(mask_250, bathy_crop, method = "near"))
  nv_resamp  <- resample(nv_crop, bathy_crop, method = "near")

  # ── 2. Build merged classification data frame ─────────────────────────────────
  bathy_single <- bathy_crop[[1]]; names(bathy_single) <- "depth"
  ph_single    <- ph_resamp[[1]];  names(ph_single)    <- "prob"
  nv_single    <- nv_resamp[[1]];  names(nv_single)    <- "nv"

  df <- as.data.frame(c(bathy_single, ph_single, nv_single), xy = TRUE, na.rm = FALSE)
  names(df) <- c("x", "y", "depth", "prob", "nv")

  df <- df %>%
    filter(!is.na(depth) | !is.na(nv)) %>%
    mutate(
      benthic_class = classify_benthic(nv, depth, prob, reef_threshold, depth_breaks)
    ) %>%
    filter(!is.na(benthic_class))

  # ── 3. Hillshade data frame ───────────────────────────────────────────────────
  hs_df_crop <- as.data.frame(hs_crop, xy = TRUE, na.rm = TRUE)
  colnames(hs_df_crop)[3] <- "hillshade"

  # ── 4. Set factor level order ─────────────────────────────────────────────────
  level_order <- c(
    "Shelf unvegetated sediments", "Shelf vegetated sediments",
    "Shallow reefs", "Mesophotic reefs", "Rariphotic reefs",
    "Upper slope reefs", "Upper slope sediments", "Mid slope reefs",
    "Lower slope reef and sediments", "Abyssal reef and sediments",
    "Seamount reefs", "Seamount sediments", "Shelf incising canyons",
    "Mid slope sediments"
  )

  present_classes  <- unique(df$benthic_class)
  level_order      <- level_order[level_order %in% present_classes]
  df$benthic_class <- factor(df$benthic_class, levels = level_order)
  present_colours  <- benthic_colours[names(benthic_colours) %in% present_classes]

  # ── 5. Closure layers within extent ──────────────────────────────────────────
  bbox_sf         <- st_as_sfc(st_bbox(c(xmin = plot_limits[1], xmax = plot_limits[2],
                                         ymin = plot_limits[3], ymax = plot_limits[4]),
                                       crs = 4326))
  state_in_extent <- st_filter(mp_state_sanctuary, bbox_sf)
  cwlth_in_extent <- st_filter(mp_cwlth_sanctuary, bbox_sf)

  has_state_sanc      <- nrow(state_in_extent) > 0
  has_cwlth_npz       <- nrow(cwlth_in_extent) > 0
  present_state_zones <- if (has_state_sanc) unique(state_in_extent$zone) else character(0)

  # ── 6. Build plot ─────────────────────────────────────────────────────────────
  p <- ggplot() +

    # Layer 1: hillshade base
    geom_tile(data = hs_df_crop, aes(x = x, y = y, fill = hillshade),
              alpha = 0.4, show.legend = FALSE) +
    scale_fill_gradient(low      = "#1a1a2e",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    # Layer 2: benthic classes
    new_scale_fill() +
    geom_tile(data = df, aes(x = x, y = y, fill = benthic_class)) +
    scale_fill_manual(
      name   = "Benthic habitat",
      values = present_colours[level_order],
      breaks = level_order,
      drop   = TRUE,
      guide  = if (show_legend) guide_legend(
        order          = 1,
        ncol           = 1,
        title.position = "top",
        override.aes   = list(size = 3, colour = NA)
      ) else "none"
    ) +

    # Layer 3: land
    new_scale_fill() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.15) +

    # Layer 4: terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.85) +
    scale_fill_manual(
      name   = "Terrestrial Parks",
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.85, colour = NA)
      ) else "none"
    )

  # Layer 5: state sanctuary / ROA zones (if present)
  if (has_state_sanc) {
    present_state_colours <- closure_colours_state[names(closure_colours_state) %in% present_state_zones]
    present_state_labels  <- c(
      "Reef Observation Area" = "Reef Observation Areas\n(no line fishing)",
      "Sanctuary Zone"        = "Sanctuary Zones\n(no fishing)"
    )[names(present_state_colours)]

    p <- p +
      new_scale_fill() +
      geom_sf(data = state_in_extent, aes(fill = zone),
              colour = NA, linewidth = 0.1, alpha = 0.75) +
      scale_fill_manual(
        name   = "State closures",
        values = present_state_colours,
        labels = present_state_labels,
        guide  = if (show_legend) guide_legend(
          order          = 2,
          title.position = "top",
          override.aes   = list(alpha = 0.75, colour = NA)
        ) else "none"
      )
  }

  # Layer 6: Commonwealth NPZ zones (if present)
  if (has_cwlth_npz) {
    p <- p +
      new_scale_fill() +
      geom_sf(data = cwlth_in_extent, aes(fill = zone),
              colour = NA, linewidth = 0.1, alpha = 0.75) +
      scale_fill_manual(
        name   = "Commonwealth closures",
        values = closure_colours_cwlth,
        labels = c("National Park Zone" = "National Park Zones\n(no fishing)"),
        guide  = if (show_legend) guide_legend(
          order          = 3,
          title.position = "top",
          override.aes   = list(alpha = 0.75, colour = NA)
        ) else "none"
      )
  }

  # Layer 7: marine park boundaries
  p <- p +
    geom_sf(data = marine_parks, fill = NA,
            colour = alpha("white", 0.65), linewidth = 0.3)

  # Layer 8: reference lines, clipped to water shallower than 200 m
  if (show_ref_lines) {
    ref_df <- make_reference_lines(limits = plot_limits,
                                   lats   = ref_lats,
                                   lons   = ref_lons,
                                   lon_ymax = ref_lon_ymax,
                                   lat_xmax = ref_lat_xmax,
                                   depth_min = -200,
                                   depth_max = 0)
    if (!is.null(ref_df) && nrow(ref_df) > 0) {
      p <- p +
        geom_path(data = ref_df,
                  aes(x = x, y = y, group = group),
                  colour    = ref_colour,
                  linetype  = ref_linetype,
                  linewidth = ref_linewidth,
                  lineend   = "butt")
    }
  }

  p <- p +
    coord_sf(xlim   = plot_limits[1:2],
             ylim   = plot_limits[3:4],
             crs    = 4326,
             datum  = sf::st_crs(4326),
             expand = FALSE) +
    labs(x = NULL, y = NULL, title = title) +
    theme_minimal() +
    theme(
      legend.key.size        = unit(0.50, "cm"),
      legend.key             = element_rect(fill = NA, colour = NA),
      legend.text            = element_text(size = 9),
      legend.title           = element_text(size = 10, face = "bold"),
      legend.position        = if (show_legend) "inside" else "none",
      legend.position.inside = if (legend_position == "topleft") c(0.01, 0.99) else c(0.01, 0.01),
      legend.justification   = if (legend_position == "topleft") c("left", "top") else c("left", "bottom"),
      legend.box             = "vertical",
      legend.spacing.y       = unit(0.1, "cm"),
      legend.background      = element_rect(fill = NA, colour = NA),
      legend.box.background  = element_rect(fill      = "white",
                                            colour    = "grey60",
                                            linewidth = 0.35),
      legend.margin          = margin(t = 5, r = 8, b = 5, l = 8),
      legend.box.margin      = margin(t = 3, r = 3, b = 3, l = 3),
      panel.grid             = element_blank(),
      panel.background       = element_rect(fill = "white", colour = NA),
      plot.background        = element_rect(fill = "white", colour = NA),
      panel.border           = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      axis.text              = element_text(size = 9, colour = "grey40"),
      axis.ticks             = element_line(colour = "grey60"),
      plot.title             = if (!is.null(title)) element_text(face = "bold", size = 13,
                                                                 hjust = 0, margin = margin(b = 4))
      else                       element_blank(),
      plot.margin = margin(t = 4, r = 6, b = 4, l = 4)
    )

  # Scale bar — stacked in the same left column as the legend box
  if (show_scalebar) {
    if (is.null(scalebar_y)) {
      scalebar_y <- if (legend_position == "topleft") 0.03 else 0.94
    }
    p <- add_scalebar(p,
                      limits    = plot_limits,
                      breaks_km = c(0, 20, 50, 100),
                      x_frac    = scalebar_x,
                      y_frac    = scalebar_y,
                      box       = FALSE)
  }

  return(p)
}

# --- FUNCTION 2: Bathymetry + closure zones ---
make_bathy_closures_map <- function(plot_limits,
                                    show_legend     = TRUE,
                                    legend_position = "bottomleft",
                                    palette         = bathy_palette) {

  require(tidyverse); require(terra); require(sf); require(ggnewscale)

  p <- ggplot() +

    # Layer 1: bathymetry
    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    # Layer 2: hillshade overlay
    new_scale_fill() +
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    # Layer 3: depth contours
    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks = c(-120, -60), colour = "white",
                 alpha = 0.35, linewidth = 0.25) +

    # Layer 4: land
    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    # Layer 5: terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      name   = "Terrestrial Parks",
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8, colour = NA)
      ) else "none"
    ) +

    # Layer 6: state sanctuary / ROA zones
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State closures",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas\n(no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones\n(no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75, colour = NA)
      ) else "none"
    ) +

    # Layer 7: Commonwealth NPZ zones
    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth closures",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Park Zones\n(no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75, colour = NA)
      ) else "none"
    ) +

    # Layer 8: marine park boundaries
    geom_sf(data = marine_parks, fill = NA,
            colour = alpha("white", 0.65), linewidth = 0.3) +

    coord_sf(xlim   = plot_limits[1:2],
             ylim   = plot_limits[3:4],
             crs    = 4326,
             datum  = sf::st_crs(4326),
             expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size        = unit(0.50, "cm"),
      legend.key             = element_rect(fill = NA, colour = NA),
      legend.text            = element_text(size = 9),
      legend.title           = element_text(size = 10, face = "bold"),
      legend.position        = if (show_legend) "inside" else "none",
      legend.position.inside = if (legend_position == "topleft") c(0.01, 0.99) else c(0.01, 0.01),
      legend.justification   = if (legend_position == "topleft") c("left", "top") else c("left", "bottom"),
      legend.box             = "vertical",
      legend.spacing.y       = unit(0.1, "cm"),
      legend.background      = element_rect(fill = NA, colour = NA),
      legend.box.background  = element_rect(fill      = "white",
                                            colour    = "grey60",
                                            linewidth = 0.35),
      legend.margin          = margin(t = 5, r = 8, b = 5, l = 8),
      legend.box.margin      = margin(t = 3, r = 3, b = 3, l = 3),
      panel.grid             = element_blank(),
      panel.background       = element_rect(fill = "white", colour = NA),
      plot.background        = element_rect(fill = "white", colour = NA),
      panel.border           = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      axis.text              = element_text(size = 9, colour = "grey40"),
      axis.ticks             = element_line(colour = "grey60"),
      plot.margin            = margin(t = 4, r = 6, b = 4, l = 4)
    )

  return(p)
}


# ==============================================================================
# 5. FIGURES 1-3: Benthic habitat with closures overlayed maps
# ==============================================================================

# ── Figure 1: South ───────────────────────────────────────────────────────────
p_south <- make_benthic_closures_map(
  plot_limits     = c(113.1, 116.5, -35, -31),
  reef_threshold  = 0.5,
  show_legend     = TRUE,
  legend_position = "topleft",
  title           = NULL
)

ggsave(file.path(out_dir, paste0(name, "-benthic-closures-south.png")),
       plot   = p_south,
       dpi    = 600,
       width  = 9,
       height = 12,
       bg     = "white")


# ── Figure 2: North ───────────────────────────────────────────────────────────
p_north <- make_benthic_closures_map(
  plot_limits     = c(112, 116, -31.2, -26.3),
  reef_threshold  = 0.5,
  show_legend     = TRUE,
  legend_position = "bottomleft",
  title           = NULL,
  scalebar_x      = 0.3,
  scalebar_y      = 0.03
)

ggsave(file.path(out_dir, paste0(name, "-benthic-closures-north.png")),
       plot   = p_north,
       dpi    = 600,
       width  = 9,
       height = 12,
       bg     = "white")

# ── Figure 3: Combined ────────────────────────────────────────────────────────
p_combined <- make_benthic_closures_map(
  plot_limits     = c(108.5, 117.2, -36, -26.1),
  reef_threshold  = 0.5,
  show_legend     = TRUE,
  legend_position = "bottom left",
  title           = NULL,
  scalebar_x      = 0.32,
  scalebar_y      = 0.03
)

ggsave(file.path(out_dir, paste0(name, "-benthic-closures-combined.png")),
       plot   = p_combined,
       dpi    = 600,
       width  = 9.5,
       height = 12,
       bg     = "white")


# ==============================================================================
# 6.  FIGURES 4-5: Bathymetry with closures maps
# ==============================================================================

# ── Figure 4: South ───────────────────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-bathy-closures-south.png")),
       plot = make_bathy_closures_map(
         plot_limits     = c(113.1, 116.5, -35, -31),
         show_legend     = TRUE,
         legend_position = "topleft"
       ),
       dpi    = 600,
       width  = 9,
       height = 12,
       bg     = "white")

# ── Figure 5: North ───────────────────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-bathy-closures-north.png")),
       plot = make_bathy_closures_map(
         plot_limits     = c(112, 116.0, -31.2, -26.3),
         show_legend     = TRUE,
         legend_position = "bottomleft"
       ),
       dpi    = 600,
       width  = 9,
       height = 12,
       bg     = "white")


# ==============================================================================
# 7. FIGURE 6: Bathymetry with closures — faceted park panels
# ==============================================================================
# Layout:
#   Row 1: Abrolhos (left)    | Jurien Bay (right)
#   Row 2: WA Metro (left)    | Legend (right)
#   Row 3: SWC (full width)
#
# All panels share equal degree-height (1.0°). Location names as rotated
# y-axis titles on each panel. One shared legend in row 2 right column.
# ==============================================================================

# --- Plot extents ---
abrolhos_facet_limits <- c(113.30, 114.30, -29.20, -28.20)   # 1.0° × 1.0°
jurien_facet_limits   <- c(114.50, 115.50, -31.00, -30.00)   # 1.0° × 1.0°
metro_facet_limits    <- c(115.05, 116.05, -32.75, -31.75)   # 1.0° × 1.0°
swc_facet_limits      <- c(113.30, 116.30, -35.00, -33.20)   # 3.0° × 1.8°

# --- Helper: add rotated y-axis location label to a panel ---
add_ylab <- function(p, label) {
  p + labs(y = label) +
    theme(
      axis.title.y = element_text(size = 11, face = "plain", angle = 90,
                                  margin = margin(r = 6), colour = "grey20")
    )
}

# --- Build individual panels with rotated y-axis location names ---
p_abrolhos_facet <- add_ylab(
  make_bathy_closures_map(plot_limits = abrolhos_facet_limits, show_legend = FALSE),
  "Abrolhos"
)

p_jurien_facet <- add_ylab(
  make_bathy_closures_map(plot_limits = jurien_facet_limits, show_legend = FALSE),
  "Jurien Bay"
)

p_metro_facet <- add_ylab(
  make_bathy_closures_map(plot_limits = metro_facet_limits, show_legend = FALSE),
  "WA Metro"
)

p_swc_facet <- add_ylab(
  make_bathy_closures_map(plot_limits = swc_facet_limits, show_legend = FALSE),
  "South-west Corner"
)

# --- Build shared legend panel ---
p_legend_source <- make_bathy_closures_map(
  plot_limits     = jurien_facet_limits,
  show_legend     = TRUE,
  legend_position = "topleft"
) +
  guides(
    fill     = guide_legend(ncol = 2),
    fill_new = guide_legend(ncol = 2)
  ) +
  theme(
    legend.position       = "right",
    legend.justification  = "center",
    legend.box            = "vertical",
    legend.key.size       = unit(0.55, "cm"),
    legend.text           = element_text(size = 12),
    legend.title          = element_text(size = 13, face = "bold"),
    legend.spacing.y      = unit(0.15, "cm"),
    legend.background     = element_rect(fill = NA, colour = NA),
    legend.box.background = element_rect(fill = "white", colour = NA),
    legend.margin         = margin(t = 8, r = 10, b = 8, l = 10)
  )

legend_grob <- cowplot::get_legend(p_legend_source)

legend_panel <- ggdraw(legend_grob) +
  theme(plot.background = element_rect(fill = "white", colour = NA))


# --- Assemble rows ---

# Row 1: Abrolhos | Jurien Bay
row1 <- cowplot::plot_grid(
  p_abrolhos_facet,
  p_jurien_facet,
  ncol       = 2,
  rel_widths = c(1, 1),
  align      = "hv",
  axis       = "tblr"
)

# Row 2: WA Metro | Legend
row2 <- cowplot::plot_grid(
  p_metro_facet,
  legend_panel,
  ncol       = 2,
  rel_widths = c(1, 1),
  align      = "hv",
  axis       = "tblr"
)

# Row 3: SWC (full width)
row3 <- cowplot::plot_grid(
  p_swc_facet,
  ncol = 1
)

# --- Final assembly ---
figure6 <- cowplot::plot_grid(
  row1,
  row2,
  row3,
  ncol        = 1,
  rel_heights = c(1, 1, 0.95)
) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        plot.margin     = margin(t = 5, r = 10, b = 5, l = 5))

ggsave(file.path(out_dir, paste0(name, "-bathy-closures-faceted-parks.png")),
       plot   = figure6,
       dpi    = 600,
       width  = 12,
       height = 16,
       bg     = "white")


# ==============================================================================
# 8. LOAD GASCOYNE SPATIAL DATA
# ==============================================================================

# Separate raster extent for Gascoyne (shifted north)
e_gasc <- ext(108.0, 122.0, -30.0, -16.0)

bathy_gasc <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  project("EPSG:4326", method = "bilinear") %>%
  crop(e_gasc)

bathy_shelf_gasc        <- clamp(bathy_gasc, lower = -200, upper = 0, values = FALSE)
names(bathy_shelf_gasc) <- "depth"
bathy_df_gasc           <- as.data.frame(bathy_shelf_gasc, xy = TRUE, na.rm = TRUE)
colnames(bathy_df_gasc)[3] <- "depth"

slope_gasc  <- terrain(bathy_gasc, v = "slope",  unit = "radians")
aspect_gasc <- terrain(bathy_gasc, v = "aspect", unit = "radians")
hs_gasc     <- shade(slope_gasc, aspect_gasc, angle = 35, direction = 315, normalize = TRUE)
names(hs_gasc) <- "hillshade"
hs_df_gasc     <- as.data.frame(hs_gasc, xy = TRUE, na.rm = TRUE)
colnames(hs_df_gasc)[3] <- "hillshade"

# Gascoyne marine parks
marine_parks_gasc <- st_read("data/south-west network/spatial/shapefiles/south-and-western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ningaloo", "Shark Bay", "Gascoyne",
                            "Abrolhos", "Abrolhos Islands",
                            "Murat", "Two Rocks", "West coast Bays")) %>%
  st_transform(4326)

mp_state_sanctuary_gasc <- marine_parks_gasc %>%
  dplyr::filter(epbc == "State",
                zone %in% c("Reef Observation Area", "Sanctuary Zone"))

mp_cwlth_sanctuary_gasc <- marine_parks_gasc %>%
  dplyr::filter(epbc == "Commonwealth",
                zone %in% c("National Park Zone"))


# ==============================================================================
# 9. ADDITIONAL MAP FUNCTIONS (zoom-in maps)
# ==============================================================================

# --- FUNCTION 3: Standard bathy map with sidebar legend ---
make_bathy_map <- function(plot_limits,
                           show_legend = TRUE,
                           label_size  = 3.5,
                           palette     = bathy_palette) {

  p <- ggplot() +

    # Layer 1: bathymetry
    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    # Layer 2: hillshade overlay
    new_scale_fill() +
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    # Layer 3: depth contours
    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    # Layer 4: land
    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    # Layer 5: terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    # Layer 6: state sanctuary / ROA zones
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas (no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    # Layer 7: Commonwealth NPZ zones
    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Parks Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size  = unit(0.5, "cm"),
      legend.text      = element_text(size = 9),
      legend.title     = element_text(size = 10),
      legend.position  = if (show_legend) "right" else "none",
      legend.box       = "vertical",
      legend.spacing.y = unit(0.3, "cm"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      axis.text        = element_text(size = 9,  colour = "grey40"),
      axis.ticks       = element_line(colour = "grey60"),
      panel.border     = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin      = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}

# --- FUNCTION 4: Bathy map with inside legend (SW network marine parks) ---
make_bathy_map_inset <- function(plot_limits,
                                 show_legend     = TRUE,
                                 legend_position = "topright",
                                 label_size      = 3.5,
                                 palette         = bathy_palette) {

  leg_xy  <- switch(legend_position,
                    "topright"    = c(0.995, 0.98),
                    "topleft"     = c(0.005, 0.98),
                    "bottomright" = c(0.995, 0.02),
                    "bottomleft"  = c(0.005, 0.02))
  leg_jus <- switch(legend_position,
                    "topright"    = c("right", "top"),
                    "topleft"     = c("left",  "top"),
                    "bottomright" = c("right", "bottom"),
                    "bottomleft"  = c("left",  "bottom"))

  p <- ggplot() +

    # Layer 1: bathymetry
    geom_tile(data = bathy_df, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    # Layer 2: hillshade overlay
    new_scale_fill() +
    geom_tile(data = hs_df, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    # Layer 3: depth contours
    geom_contour(data = bathy_df, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    # Layer 4: land
    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    # Layer 5: terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    # Annotate legend area to mask underlying data
    annotate("rect",
             xmin      = 117.7, xmax = 121.9,
             ymin      = -30.1, ymax = -24.1,
             fill      = alpha("white", 0.85),
             colour    = "grey70",
             linewidth = 0.3) +

    # Layer 6: state sanctuary / ROA zones
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas (no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    # Layer 7: Commonwealth NPZ zones
    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Parks Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size        = unit(0.5, "cm"),
      legend.key             = element_blank(),
      legend.text            = element_text(size = 9),
      legend.title           = element_text(size = 10),
      legend.position        = if (show_legend) "inside" else "none",
      legend.position.inside = leg_xy,
      legend.justification   = leg_jus,
      legend.box             = "vertical",
      legend.spacing.y       = unit(0.3, "cm"),
      legend.background      = element_blank(),
      legend.box.background  = element_blank(),
      legend.box.margin      = margin(0, 0, 0, 0),
      legend.margin          = margin(t = 5, r = 6, b = 5, l = 6),
      panel.grid             = element_blank(),
      panel.background       = element_rect(fill = "white", colour = NA),
      plot.background        = element_rect(fill = "white", colour = NA),
      axis.text              = element_text(size = 9,  colour = "grey40"),
      axis.ticks             = element_line(colour = "grey60"),
      panel.border           = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin            = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}

# --- FUNCTION 5: Bathy map with inside legend (Gascoyne marine parks) ---
make_bathy_map_inset_gasc <- function(plot_limits,
                                      show_legend     = TRUE,
                                      legend_position = "bottomright",
                                      label_size      = 3.5,
                                      palette         = bathy_palette) {

  leg_xy  <- switch(legend_position,
                    "topright"    = c(0.995, 0.98),
                    "topleft"     = c(0.005, 0.98),
                    "bottomright" = c(0.995, 0.02),
                    "bottomleft"  = c(0.005, 0.02))
  leg_jus <- switch(legend_position,
                    "topright"    = c("right", "top"),
                    "topleft"     = c("left",  "top"),
                    "bottomright" = c("right", "bottom"),
                    "bottomleft"  = c("left",  "bottom"))

  p <- ggplot() +

    # Layer 1: bathymetry
    geom_tile(data = bathy_df_gasc, aes(x = x, y = y, fill = depth)) +
    scale_fill_gradientn(
      colours  = palette,
      limits   = c(-200, 0),
      na.value = NA,
      name     = "Depth (m)",
      breaks   = c(0, -50, -100, -150, -200),
      labels   = c("0", "-50", "-100", "-150", "-200"),
      guide    = if (show_legend) guide_colorbar(
        barwidth       = 1.2,
        barheight      = 8,
        title.position = "top",
        ticks          = TRUE,
        order          = 1
      ) else "none"
    ) +

    # Layer 2: hillshade overlay
    new_scale_fill() +
    geom_tile(data = hs_df_gasc, aes(x = x, y = y, fill = hillshade),
              alpha = 0.3, show.legend = FALSE) +
    scale_fill_gradient(low      = "grey20",
                        high     = "#e8e8e8",
                        na.value = NA,
                        guide    = "none") +

    # Layer 3: depth contours
    geom_contour(data = bathy_df_gasc, aes(x = x, y = y, z = depth),
                 breaks    = c(-120, -60),
                 colour    = "white",
                 alpha     = 0.35,
                 linewidth = 0.25) +

    # Layer 4: land
    geom_sf(data = aus, fill = "seashell2", colour = "grey50", linewidth = 0.25) +

    # Layer 5: terrestrial parks
    new_scale_fill() +
    geom_sf(data = terrnp, aes(fill = leg_catego), colour = NA, alpha = 0.8) +
    scale_fill_manual(
      values = c("National Park"  = "#c4cea6",
                 "Nature Reserve" = "#e4d0bb"),
      name   = "Terrestrial Parks",
      guide  = if (show_legend) guide_legend(
        order          = 4,
        title.position = "top",
        override.aes   = list(alpha = 0.8)
      ) else "none"
    ) +

    # Layer 6: state sanctuary / ROA zones (Gascoyne parks)
    new_scale_fill() +
    geom_sf(data = mp_state_sanctuary_gasc, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "State",
      values = c("Sanctuary Zone"        = "#fc887c",
                 "Reef Observation Area" = "#ff4430"),
      labels = c("Reef Observation Area" = "Reef Observation Areas (no line fishing)",
                 "Sanctuary Zone"        = "Sanctuary Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 2,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    # Layer 7: Commonwealth NPZ zones (Gascoyne parks)
    new_scale_fill() +
    geom_sf(data = mp_cwlth_sanctuary_gasc, aes(fill = zone),
            colour = NA, linewidth = 0.15, alpha = 0.75) +
    scale_fill_manual(
      name   = "Commonwealth",
      values = c("National Park Zone" = "#ffc8c2"),
      labels = c("National Park Zone" = "National Parks Zones (no fishing)"),
      guide  = if (show_legend) guide_legend(
        order          = 3,
        title.position = "top",
        override.aes   = list(alpha = 0.75)
      ) else "none"
    ) +

    coord_sf(xlim = plot_limits[1:2], ylim = plot_limits[3:4],
             crs = 4326, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      legend.key.size        = unit(0.5, "cm"),
      legend.key             = element_blank(),
      legend.text            = element_text(size = 9),
      legend.title           = element_text(size = 10),
      legend.position        = if (show_legend) "inside" else "none",
      legend.position.inside = leg_xy,
      legend.justification   = leg_jus,
      legend.box             = "vertical",
      legend.spacing.y       = unit(0.3, "cm"),
      legend.background      = element_rect(fill = NA, colour = NA),
      legend.box.background  = element_rect(fill      = "white",
                                            colour    = "grey60",
                                            linewidth = 0.35),
      legend.box.margin      = margin(t = 3, r = 3, b = 3, l = 3),
      legend.margin          = margin(t = 5, r = 8, b = 5, l = 8),
      panel.grid             = element_blank(),
      panel.background       = element_rect(fill = "white", colour = NA),
      plot.background        = element_rect(fill = "white", colour = NA),
      axis.text              = element_text(size = 9,  colour = "grey40"),
      axis.ticks             = element_line(colour = "grey60"),
      panel.border           = element_rect(fill = NA, colour = "grey60", linewidth = 0.4),
      plot.margin            = margin(t = 5, r = 5, b = 5, l = 5)
    )

  return(p)
}


# ==============================================================================
# 10. FIGURES 7-8: WA overview maps
# ==============================================================================

# ── Figure 7: WA overview with sidebar legend ─────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-wa-overview-viridis.png")),
       plot = make_bathy_map(
         plot_limits = c(108.0, 122.0, -38.0, -24.0),
         show_legend = TRUE,
         palette     = bathy_palette
       ),
       dpi = 600, width = 10, height = 10, bg = "white")

# ── Figure 8: WA overview with inside legend ──────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-wa-overview-viridis-inset-legend.png")),
       plot = make_bathy_map_inset(
         plot_limits     = c(108.0, 122.0, -38.0, -24.0),
         show_legend     = TRUE,
         legend_position = "topright",
         palette         = bathy_palette
       ),
       dpi = 600, width = 10, height = 11, bg = "white")


# ==============================================================================
# 11. FIGURES 9-12: Individual park zoom-ins
# ==============================================================================

# ── Figure 9: South-west Corner ───────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-swc-viridis.png")),
       plot = make_bathy_map(
         plot_limits = c(114.2, 116.2, -34.6, -33.4),
         show_legend = TRUE,
         palette     = bathy_palette
       ),
       dpi = 600, width = 10, height = 6, bg = "white")

# ── Figure 10: Jurien Bay ─────────────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-jurien-viridis.png")),
       plot = make_bathy_map(
         plot_limits = c(114.5, 115.4, -30.8, -30.0),
         show_legend = FALSE,
         palette     = bathy_palette
       ),
       dpi = 600, width = 10, height = 8, bg = "white")

# ── Figure 11: Rockingham ─────────────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-rockingham-viridis.png")),
       plot = make_bathy_map(
         plot_limits = c(115.2, 116.0, -32.6, -31.8),
         show_legend = FALSE,
         palette     = bathy_palette
       ),
       dpi = 600, width = 10, height = 8, bg = "white")

# ── Figure 12: Abrolhos ───────────────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-abrolhos-viridis.png")),
       plot = make_bathy_map(
         plot_limits = c(113.3, 114.3, -29.2, -28.2),
         show_legend = FALSE,
         palette     = bathy_palette
       ),
       dpi = 600, width = 10, height = 8, bg = "white")


# ==============================================================================
# 12. FIGURES 13-14: Gascoyne maps
# ==============================================================================

# ── Figure 13: Gascoyne overview ──────────────────────────────────────────────
ggsave(file.path(out_dir, paste0(name, "-gascoyne-viridis-inset.png")),
       plot = make_bathy_map_inset_gasc(
         plot_limits     = c(108.0, 122.0, -30.0, -16.0),
         show_legend     = TRUE,
         legend_position = "bottomright",
         palette         = bathy_palette
       ),
       dpi = 600, width = 10, height = 11, bg = "white")

# ── Figure 14: Gascoyne zoomed ────────────────────────────────────────────────
ggsave(file.path(out_dir, "gascoyne-zoomed-viridis.png"),
       plot = make_bathy_map_inset_gasc(
         plot_limits     = c(112.0, 116.5, -26.5, -22.0),
         show_legend     = TRUE,
         legend_position = "bottomright",
         palette         = bathy_palette
       ),
       dpi = 600, width = 10, height = 9, bg = "white")


# ==============================================================================
# End of script
# ==============================================================================
