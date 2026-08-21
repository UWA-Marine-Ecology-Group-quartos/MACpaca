###
# Project: NESP 5.6 Project - North-west Network Report
# Data:    BRAN2023 monthly SST, AusBathyTopo 2024 250 m topography,
#          marine park shapefiles, aus outline
# Task:    Extract January 2025 SST from BRAN2023 (Leeuwin Current heatwave)
#          and map over North-west Network extent with MPAs and land topography
# Author:  Annika Leunig
# Date:    July 2026
# Outputs: 1. January2025 mean SST map (Leeuwin Current marine heatwave)
#             with marine park boundaries and land topography overlaid
###

# Table of contents
#     1.  Set up and load data
#     2.  BRAN2023 download and SST extraction functions
#     3.  Download and extract January 2025 SST
#     4.  FIGURE 1: January 2025 mean SST map


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================
rm(list = ls())

# Set names
name <- "north-west"
park <- "network"

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(ggnewscale)
library(cowplot)
library(RColorBrewer)
library(ncdf4)
library(xml2)

# Allow long downloads
options(timeout = 6000)

# Standardise every layer on GDA2020 geographic (EPSG:7844), matching the
# north-west network KEF map script
aus_crs <- 7844

# Set cropping / download extents
e     <- ext(106, 133, -28, -11)
e_vec <- c(106, 133, -28, -11)

# Final map panel extent
plot_limits <- c(109, 130, -26.5, -12.5)

# ── Load spatial files ────────────────────────────────────────────────────────
# Terrestrial parks
terrnp <- st_read("data/north-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Terrestrial__.shp") %>%
  dplyr::filter(TYPE %in% c("Nature Reserve", "National Park")) %>%
  st_transform(aus_crs)

# Aus outline
aus <- st_read("data/north-west network/spatial/shapefiles/STE_2021_AUST_GDA2020.shp") %>%
  st_make_valid() %>%
  st_transform(aus_crs)
ausc <- st_crop(aus, e)

# Marine parks - north-west network (Commonwealth AMPs + WA state marine parks,
# matches the north-west network KEF map script)
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
    "Lalang-gaddam", "Rowley Shoals", "Scott Reef")) %>%
  st_transform(aus_crs)

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc == "Commonwealth")

# ── Land topography (hillshade + DEM tint) ────────────────────────────────────
topo <- rast("data/north-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  project(paste0("EPSG:", aus_crs)) %>%
  crop(e) %>%
  aggregate(fact = 10) %>%
  clamp(lower = 1, values = FALSE)

# Hillshade from slope + aspect (sun from the NW, 30 deg altitude)
slope  <- terrain(topo, "slope", unit = "radians")
aspect <- terrain(topo, "aspect", unit = "radians")
hill   <- shade(slope, aspect, 30, 270)
names(hill) <- "shades"

# Inland water below sea level (e.g. Lake Eyre) - flagged for a blue fill
lakes <- terra::ifel(topo <= 0, 1, NA)

# Greyscale palette for the hillshade underlay
pal_greys <- hcl.colors(1000, "Grays")


# ==============================================================================
# 2. BRAN2023 DOWNLOAD AND SST EXTRACTION FUNCTIONS
# ==============================================================================
# Download a single monthly BRAN2023 ocean temperature file
download_BRAN2023_monthly <- function(year, month, destination) {

  dir.create(destination, showWarnings = FALSE, recursive = TRUE)

  month_pad <- sprintf("%02d", month)
  filename  <- paste0("ocean_temp_", year, "_", month_pad, ".nc")
  destfile  <- file.path(destination, filename)
  url       <- paste0("https://thredds.nci.org.au/thredds/fileServer/gb6/BRAN/BRAN2023/daily/", filename)

  if (file.exists(destfile)) {
    message("Already exists, skipping: ", filename)
    return(destfile)
  }

  message("Downloading: ", filename, "\n  ", url)
  tryCatch({
    download.file(url, destfile = destfile, mode = "wb", quiet = FALSE)
    return(destfile)
  }, error = function(e) {
    message("Failed: ", filename, " — ", conditionMessage(e))
    return(NULL)
  })
}

# Extract surface SST from a monthly file
extract_sst_monthly_mean <- function(nc_file, extent_vec) {

  nc <- nc_open(nc_file)

  depth_dim <- nc$dim[["st_ocean"]]
  if (is.null(depth_dim)) depth_dim <- nc$dim[["depth"]]
  n_depth   <- length(depth_dim$vals)

  time_dim  <- nc$dim[["Time"]]
  if (is.null(time_dim)) time_dim <- nc$dim[["time"]]
  n_time    <- length(time_dim$vals)

  nc_close(nc)

  message("  Depth levels: ", n_depth, " | Time steps: ", n_time)

  r <- rast(nc_file, subds = "temp")

  sst_idx  <- seq(1L, n_depth * n_time, by = n_depth)
  sst      <- r[[sst_idx]]
  sst_crop <- crop(sst, ext(extent_vec[1], extent_vec[2],
                            extent_vec[3], extent_vec[4]))
  mean(sst_crop, na.rm = TRUE)
}

# ==============================================================================
# 3. DOWNLOAD AND EXTRACT JANUARY 2025 SST
# ==============================================================================
# ── Download and write file (if doesn't exist already) ────────────────────────
dest_dir <- "data/north-west network/spatial/rasters/BRAN/BRAN2023_SST_monthly/"

file_january2025 <- download_BRAN2023_monthly(2025, 1, dest_dir)

sst_january2025 <- extract_sst_monthly_mean(file_january2025, e_vec)
names(sst_january2025) <- "sst_mean_january2025"

writeRaster(sst_january2025,
            "data/north-west network/spatial/rasters/BRAN/BRAN2023_SST_january2025_mean.tif",
overwrite = TRUE)

# Load raster (if written already)
sst_january2025 <- rast("data/north-west network/spatial/rasters/BRAN/BRAN2023_SST_january2025_mean.tif")

# CRS is set explicitly here (BRAN2023 is on a regular lat/lon grid, WGS84)
# before reprojecting to match the GDA2020 (7844) CRS used for the map
crs(sst_january2025) <- "EPSG:4326"
sst_january2025 <- project(sst_january2025, paste0("EPSG:", aus_crs))

# ==============================================================================
# 4. FIGURE 1: JANUARY 2025 MEAN SST MAP
# ==============================================================================
sst_mean_df <- as.data.frame(sst_january2025, xy = TRUE, na.rm = TRUE)
colnames(sst_mean_df)[3] <- "sst"

p_sst_mean <- ggplot() +

  # Layer 1: Land topography - greyscale hillshade underlay + green-to-brown tint
  geom_spatraster(data = hill, alpha = 1, maxcell = Inf, show.legend = FALSE) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = topo, maxcell = Inf, show.legend = FALSE) +
  scale_fill_hypso_tint_c(palette = "dem_poster", alpha = 0.6, na.value = "transparent") +
  new_scale_fill() +

  # Layer 1b: Inland water - below sea level, filled blue
  geom_spatraster(data = lakes, maxcell = Inf, show.legend = FALSE) +
  scale_fill_gradientn(colours = c("#abd3e5", "#abd3e5"), na.value = "transparent") +
  new_scale_fill() +

  # Layer 2: SST mean raster
  geom_raster(data = sst_mean_df, aes(x = x, y = y, fill = sst), interpolate = FALSE) +
  scale_fill_gradientn(
    name     = "SST (°C)  January 2025",
    colours  = rev(brewer.pal(11, "RdYlBu")),
    na.value = NA,
    guide    = guide_colourbar(
      direction      = "horizontal",
      barwidth       = unit(10, "cm"),
      barheight      = unit(0.5, "cm"),
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +

  # Coastline
  geom_sf(data = ausc, fill = NA, colour = "grey60", linewidth = 0.15) +

  # Layer 3: Commonwealth marine parks (north-west network)
  geom_sf(data = marine_parks_amp,
          fill = alpha("white", 0.4), colour = "white", linewidth = 0.35) +

  coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
           ylim = c(plot_limits[3], plot_limits[4]),
           crs = aus_crs, expand = FALSE) +
  scale_x_continuous(breaks = seq(plot_limits[1], plot_limits[2], by = 5)) +
  scale_y_continuous(breaks = seq(plot_limits[3], plot_limits[4], by = 2)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_minimal() +
  theme(
    legend.key.size      = unit(0.5, "cm"),
    legend.text          = element_text(size = 11),
    legend.title         = element_text(size = 12),
    legend.position      = "bottom",
    legend.box           = "horizontal",
    legend.margin        = margin(t = 4),
    panel.grid           = element_blank(),
    panel.background     = element_rect(fill = "#abd3e5", colour = NA),
    plot.background      = element_rect(fill = "white",   colour = NA),
    axis.text            = element_text(size = 13, colour = "grey40"),
    axis.ticks           = element_line(colour = "grey60"),
    plot.margin          = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave(paste(paste0("plots/", park, "/spatial/SST/", name),
             "SST-january2025.png", sep = "-"),
       plot = p_sst_mean, dpi = 300, width = 12, height = 7, bg = "white"
)

# ==============================================================================
# End of script
# ==============================================================================
