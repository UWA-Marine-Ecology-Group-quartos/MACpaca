###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Syntheses 71 (BRUV) and 84 (BOSS), plus lobster pot deployments
# Task:    Map sample locations by method at three extents - a whole-of-park
#          overview, Clio Bank, and Shallow Bank
# Author:  Claude Spencer & Henry Evans
# Date:    August 2026
#
# Standalone script - does not read 00_config.yml and does not depend on any
# other script in the pipeline.
###

rm(list = ls())

# TODO Run these once or as required:
# remotes::install_github("GlobalArchiveManual/CheckEM")
# CheckEM::ga_api_set_token()

library(tidyverse)
library(CheckEM)
library(sf)
library(terra)
library(ggnewscale)
library(scales)

options(timeout = 600) # increase if more time needed for large data downloads

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# ---------------------------------------------------------------------------
# TODO Settings
# ---------------------------------------------------------------------------

outdir  <- "plots/abrolhos_shallow-bank/sampling-maps"
outstub <- "Abrolhos_sample-locations-by-method"

# TODO Path to the lobster pot csv
lobster_file <- "data/abrolhos_shallow-bank/raw/abrolhosAMP_lobster-pots_all-years.csv"

# Northern cut off. Nothing north of this latitude is kept or drawn.
lat_max <- -27.8

# Plot extents: xmin, xmax, ymin, ymax
extents <- list(
  overview = list(
    limits     = c(113.2, 114.3, -29.4, -27.9),
    title      = "Abrolhos Marine Park",
    brk_x      = 0.4,
    brk_y      = 0.4,
    point_size = 1.2
  ),
  `clio-bank` = list(
    limits     = c(114.05, 114.25, -29.33, -29.2),
    title      = "Clio Bank",
    brk_x      = 0.05,
    brk_y      = 0.05,
    point_size = 2.2
  ),
  # Vertical extent matches the lobster pot deployments
  `shallow-bank` = list(
    limits     = c(113.2, 113.6, -28.18, -27.97),
    title      = "Shallow Bank",
    brk_x      = 0.1,
    brk_y      = 0.05,
    point_size = 2.0
  )
)

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ---------------------------------------------------------------------------
# Download the metadata for both syntheses
# ---------------------------------------------------------------------------
# Only the metadata endpoint is needed - no counts, lengths or habitat.

bruv_metadata <- CheckEM::ga_api_metadata(token = token,
                                          synthesis_id = "71") # BRUV

boss_metadata <- CheckEM::ga_api_metadata(token = token,
                                          synthesis_id = "84") # BOSS

# Only the columns used here are kept - the observer and successful columns come
# back with different types from each synthesis and will not bind
meta_cols <- c("campaignid", "sample", "date_time",
               "longitude_dd", "latitude_dd", "depth_m", "status")

samples_ga <- bind_rows(
  bruv_metadata %>% dplyr::select(any_of(meta_cols)) %>% mutate(method = "BRUV"),
  boss_metadata %>% dplyr::select(any_of(meta_cols)) %>% mutate(method = "BOSS")
) %>%
  # One BOSS sample has a typo in the date on GlobalArchive - 2002 not 2022
  mutate(date_time = if_else(year(date_time) %in% 2002,
                             date_time %m+% lubridate::years(20),
                             date_time)) %>%
  dplyr::transmute(
    campaignid,
    sample,
    method,
    year = as.character(year(date_time)),
    longitude_dd,
    latitude_dd
  )

# ---------------------------------------------------------------------------
# Lobster pots
# ---------------------------------------------------------------------------
lobster <- read_csv(lobster_file, show_col_types = FALSE) %>%
  dplyr::transmute(
    campaignid   = campaign,
    sample       = as.character(pot_number),
    method       = "Lobster pot",
    year         = as.character(year),
    longitude_dd = longitude,
    latitude_dd  = latitude
  ) %>%
  glimpse()

# ---------------------------------------------------------------------------
# Combine and trim
# ---------------------------------------------------------------------------
# Factor order sets both the legend order and the drawing order below:
# lobster pots are drawn first (bottom), then BRUVs, then BOSS on top.
samples <- bind_rows(samples_ga, lobster) %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd)) %>%
  dplyr::filter(latitude_dd <= lat_max) %>%
  dplyr::mutate(
    method = factor(method, levels = c("Lobster pot", "BRUV", "BOSS"))
  )

# TODO check what is left before plotting
samples %>% count(method, year)
samples %>% count(method, campaignid) %>% print(n = Inf)

samples %>%
  group_by(method) %>%
  summarise(across(c(longitude_dd, latitude_dd), list(min = min, max = max)))

# ---------------------------------------------------------------------------
# Spatial layers - read once at the widest extent, cropped per panel below
# ---------------------------------------------------------------------------
sf_use_s2(FALSE)

e_all <- ext(112.9, 114.4, -29.5, -27.7)

ausc <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e_all) %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

npz <- marine_parks_amp[marine_parks_amp$zone %in% "National Park Zone", ]

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e_all) %>%
  st_transform(4326)

# Bathymetry raster held as a SpatRaster so it can be cropped per panel
bathy_r <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e_all) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim()

# ---------------------------------------------------------------------------
# Scales
# ---------------------------------------------------------------------------
depth_fills <- scale_fill_manual(
  values = c("#a7cfe0", "#9acbec", "#98c4f7", "#a3bbff", "#81a1fc"),
  guide = "none"
)

method_cols <- c(
  "Lobster pot" = "#e31a1c",  # red
  "BRUV"        = "#1f9bff",  # bright blue
  "BOSS"        = "#1a9e3c"   # forest green
)

# Shape is mapped to survey year, not method. Built from the years actually
# present in the data so a new survey does not need a code change - extend
# shape_pool if there are ever more years than it holds.
shape_pool <- c(16, 17, 18, 15, 3, 4, 8, 7)

survey_years <- sort(unique(samples$year))

if (length(survey_years) > length(shape_pool)) {
  stop("More survey years than shapes in shape_pool - add more shapes")
}

year_shapes <- setNames(shape_pool[seq_along(survey_years)], survey_years)

# Diamonds and pluses read smaller than circles at the same size
shape_size_mult <- c("16" = 1, "17" = 1, "18" = 1.35, "15" = 1,
                     "3" = 1.2, "4" = 1.2, "8" = 1.2, "7" = 1.2)

# ---------------------------------------------------------------------------
# Plot builder
# ---------------------------------------------------------------------------
sample_map <- function(limits, title = NULL, brk_x = 0.2, brk_y = 0.2,
                       point_size = 1.6, dat = samples) {

  # Bathymetry cropped to a slightly padded version of the panel, so contours
  # run to the edges rather than stopping short
  pad <- 0.05
  bathy <- bathy_r %>%
    terra::crop(ext(limits[1] - pad, limits[2] + pad,
                    limits[3] - pad, limits[4] + pad)) %>%
    as.data.frame(xy = TRUE, na.rm = TRUE)
  names(bathy)[3] <- "Depth"

  # Samples inside this panel only, so the legends drop methods and years that
  # are not present
  dat_panel <- dat %>%
    dplyr::filter(longitude_dd >= limits[1], longitude_dd <= limits[2],
                  latitude_dd  >= limits[3], latitude_dd  <= limits[4]) %>%
    dplyr::mutate(method = droplevels(method))

  message(title, ": ", nrow(dat_panel), " samples")

  p <- ggplot() +
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
    geom_sf(data = npz, fill = "#7bbc63", alpha = 2/5, colour = NA) +
    geom_sf(data = marine_parks_amp, fill = NA, colour = "grey30", linewidth = 0.3) +
    geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, linewidth = 0.3)

  # One geom_point per method x year, methods added in factor order so lobster
  # pots sit at the bottom of the stack and BOSS on top. Splitting by year as
  # well lets each shape carry its own size multiplier.
  for (m in levels(dat_panel$method)) {
    for (yy in sort(unique(dat_panel$year[dat_panel$method == m]))) {

      shp <- as.character(year_shapes[[yy]])

      p <- p +
        geom_point(
          data = dat_panel %>% dplyr::filter(method == m, year == yy),
          aes(x = longitude_dd, y = latitude_dd,
              colour = method, shape = year),
          size = point_size * shape_size_mult[[shp]],
          alpha = 0.9
        )
    }
  }

  p +
    scale_colour_manual(name = "Method", values = method_cols, drop = TRUE) +
    scale_shape_manual(name = "Year", values = year_shapes, drop = TRUE) +
    guides(
      colour = guide_legend(order = 1, override.aes = list(size = 3, shape = 16)),
      shape  = guide_legend(order = 2, override.aes = list(size = 3, colour = "grey20"))
    ) +
    labs(x = "Longitude", y = "Latitude", title = title) +
    scale_x_continuous(breaks = scales::breaks_width(brk_x)) +
    scale_y_continuous(breaks = scales::breaks_width(brk_y)) +
    coord_sf(
      xlim = c(limits[1], limits[2]),
      ylim = c(limits[3], limits[4]),
      crs = 4326
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "#b9d1d6", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "grey70",
                                      linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks        = element_line(colour = "grey70", linewidth = 0.4),
      axis.ticks.length = unit(0.15, "cm"),
      legend.position = "right",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(face = "bold", size = 13)
    )
}

# ---------------------------------------------------------------------------
# Build and save each panel
# ---------------------------------------------------------------------------
for (nm in names(extents)) {

  cfg <- extents[[nm]]

  p <- sample_map(
    limits     = cfg$limits,
    title      = cfg$title,
    brk_x      = cfg$brk_x,
    brk_y      = cfg$brk_y,
    point_size = cfg$point_size
  )

  print(p)

  ggsave(
    filename = file.path(outdir, paste0(outstub, "_", nm, ".png")),
    plot = p,
    height = 7,
    width = 8,
    dpi = 300,
    bg = "white"
  )

  saveRDS(p, file.path(outdir, paste0(outstub, "_", nm, ".rds")))
}
