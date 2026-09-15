###
# Project: NESP 5.6 Project - South west Corner Report
# Data:    CAPAD 2022 marine parks, Exclusive Economic Zone (Perth Treaty)
#          limits and AusBathyTopo 2024 250 m bathymetry/topography
# Task:    Create Australia-wide marine parks overview (national context) map
# Author:  Annika Leunig (modified from Claude Spencer's code)
# Date:    Feb 2026
# Outputs: 1. Australia-wide marine parks overview map (AMP zones, state marine
#             parks, bathymetry/topography hillshade and external territories)
###

# Table of contents
#     1.  Set up and load data
#     2.  Prepare marine park layers
#     3.  Bathymetry and topography hillshading
#     4.  Plot inputs
#     5.  FIGURE 1: Australia-wide marine parks overview map


# ==============================================================================
# 1. SET UP AND LOAD DATA
# ==============================================================================

# Clear your environment
rm(list = ls())

# Set the study name and marine park name (for folder structure)
name <- "south-west"
park <- "network"

# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(CheckEM)
library(ggpattern)
library(ggnewscale)
library(scales)     # load AFTER terra so scales::rescale() wins
library(tidyterra)
library(patchwork)

# ── Load spatial files ──────────────────────────────────────────────────────
# CAPAD Australian Protected Areas Database (CAPAD) 2024 - Marine
# NOTE: as of the 2024 release, CAPAD no longer populates ZONE_TYPE for ANY
# feature (Commonwealth or State/Territory) - it comes back 100% NA. Kept here
# for state/territory attributes (name, iucn, type, etc.) and the sanctuary
# split below, but Commonwealth zoning now comes from a separate live source
# (see step 2).
marine.parks <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp") %>%
  clean_names() %>%
  sf::st_make_valid() %>%
  glimpse()

marine.parks_2022 <- st_read("data/south-west network/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  clean_names() %>%
  sf::st_make_valid() %>%
  glimpse()

# Exclusive Economic Zone (Perth Treaty) limits
eez <- st_read("data/south-west network/spatial/shapefiles/Exclusive_Economic_Zone_(Perth_Treaty)_limits.shp") %>%
  glimpse()


# ==============================================================================
# 2. PREPARE MARINE PARK LAYERS
# ==============================================================================

# ── State marine parks ──────────────────────────────────────────────────────
# TODO / KNOWN ISSUE: this sanctuary/non-sanctuary split originally relied on
# zone_type (via str_detect(zone_type, "Sanctuary|No take")), but zone_type is
# now NA for every state/territory row in CAPAD 2024 (confirmed: 4073/4073 NA).
# This classification is currently WRONG - every state feature is falling into
# the "State Marine Park" catch-all below, with none correctly flagged as
# Sanctuary Zone. This needs a proper fix (likely via `iucn`, `type`, or a
# per-jurisdiction join) before this map can be trusted - do not publish
# figures from this layer until it's resolved.
state.mps <- marine.parks_2022 %>%
  dplyr::filter(!epbc %in% "Commonwealth") %>%
  dplyr::mutate(
    sanctuary = dplyr::case_when(
      str_detect(zone_type, "Sanctuary|No take|Marine National Park|Preservation|Scientific Research") ~ "Sanctuary Zone",
      TRUE ~ "State Marine Park"
    )
  )

is_gc <- st_geometry_type(state.mps) == "GEOMETRYCOLLECTION"
state_gc    <- state.mps[is_gc, ] %>% st_collection_extract("POLYGON")
state_clean <- state.mps[!is_gc, ]
state.mps <- rbind(state_clean, state_gc) %>% st_cast("MULTIPOLYGON")

count(st_drop_geometry(state.mps), state, sanctuary)
# ── Australian (Commonwealth) marine parks ──────────────────────────────────
# CAPAD 2024 dropped zone naming for AMPs entirely (zone_type NA for all 1264
# Commonwealth rows), so Commonwealth zoning is now pulled live from Parks
# Australia / DCCEEW's own zoning layer instead of from CAPAD. This layer is
# independently maintained and more current than CAPAD's snapshot (e.g. it
# reflects the Feb 2025 South-east re-zoning).
amp_zones <- c(
  "Special Purpose Zone",
  "National Park Zone",
  "Habitat Protection Zone",
  "Recreational Use Zone",
  "Multiple Use Zone",
  "Sanctuary Zone"
)

amp_zoning_url <- "https://gis.environment.gov.au/gispubmap/rest/services/ogc_services/Australian_Marine_Parks/MapServer/0/query?where=1%3D1&outFields=*&f=geojson"

# zonename carries a bracketed sub-classification for some zones, e.g.
# "Special Purpose Zone (Trawl)", "Habitat Protection Zone (Macquarie)" -
# stripped here the same way the old CAPAD zone_type parsing did, to collapse
# back down to the 6-category amp_zones vocabulary.
fed.mps <- st_read(amp_zoning_url) %>%
  clean_names() %>%
  st_make_valid() %>%
  st_transform(st_crs(marine.parks)) %>%   # WGS84 -> GDA94, match CAPAD layers
  mutate(zone_type = str_replace_all(zonename, "\\s*\\([^\\)]+\\)", "")) %>%
  filter(zone_type %in% amp_zones) %>%
  glimpse()

sort(unique(fed.mps$zone_type))
# sanity check - should be 0 rows / empty if every zone matched amp_zones
st_read(amp_zoning_url) %>%
  st_drop_geometry() %>%
  clean_names() %>%
  mutate(zone_type = str_replace_all(zonename, "\\s*\\([^\\)]+\\)", "")) %>%
  filter(!zone_type %in% amp_zones) %>%
  count(zonename)


# ==============================================================================
# 3. BATHYMETRY AND TOPOGRAPHY HILLSHADING
# ==============================================================================
# (unchanged from original)

bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  aggregate(fact = 10) %>%
  clamp(upper = 0, values = F)

slope  <- terrain(bathy, "slope", unit = "radians")
aspect <- terrain(bathy, "aspect", unit = "radians")
hillbath <- shade(slope, aspect, 10, 0)
names(hillbath) <- "shades"

pal_greys <- hcl.colors(1000, "Grays")

index <- hillbath %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_colsbathy <- pal_greys[index]

topo <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  aggregate(fact = 10) %>%
  clamp(lower = 1, values = F)

slope  <- terrain(topo, "slope", unit = "radians")
aspect <- terrain(topo, "aspect", unit = "radians")
hill <- shade(slope, aspect, 30, 270)
names(hill) <- "shades"

index <- hill %>%
  mutate(index_col = rescale(shades, to = c(1, length(pal_greys)))) %>%
  mutate(index_col = round(index_col)) %>%
  pull(index_col)
vector_cols <- pal_greys[index]


# ==============================================================================
# 4. PLOT INPUTS
# ==============================================================================
# (unchanged from original - plot_limits, cities data.frame, etc.)

plot_limits <- c(94.0, 170.0, -48.0, -9.0)

cities <- data.frame(
  city  = c("Darwin", "Brisbane", "Sydney", "Canberra", "Adelaide", "Melbourne", "Perth"),
  x     = c(130.8444, 153.0260, 151.2093, 149.1310, 138.6007, 144.9631, 115.8617),
  y     = c(-12.4637, -27.4705, -33.8688, -35.2802, -34.9285, -37.8136, -31.9514),
  hjust = c(0,         0,         0,        0,         0,         1,         1)
)
cities$lab_x <- cities$x + ifelse(cities$hjust == 0, 0.7, -0.7)


# ==============================================================================
# 5. FIGURE 1: AUSTRALIA-WIDE MARINE PARKS OVERVIEW MAP
# ==============================================================================
p1 <- ggplot() +
  geom_spatraster(data = hillbath, fill = vector_colsbathy, maxcell = Inf,
                  alpha = 1) +
  geom_spatraster(data = bathy, show.legend = F, alpha = 0.6) +
  scale_fill_gradientn(colours = c("#061442", "#2b63b5", "#9dc9e1"),
                       values = rescale(c(-6221, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = state.mps, aes(fill = sanctuary), colour = NA) +
  scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                               "State Marine Park" = "grey80"),
                    name = "State Marine Parks",
                    guide = guide_legend(ncol = 1, order = 2)) +
  new_scale_fill() +
  geom_sf(data = fed.mps,
          aes(fill = zone_type),
          colour = NA,
          alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "Special Purpose Zone" = "#6daff4",
      "National Park Zone" = "#7bbc63",
      "Habitat Protection Zone" = "#fff8a3",
      "Recreational Use Zone" = "#ffb36b",
      "Multiple Use Zone" = "#b9e6fb",
      "Sanctuary Zone" = "#f7c0d8"
    ),
    name = "Australian Marine Parks",
    guide = guide_legend(ncol = 2, order = 1)
  ) +
  new_scale_fill() +
  geom_sf(data = eez, colour = "grey20", linetype = 2, fill = NA) +
  geom_spatraster(data = hill, alpha = 1, show.legend = F) +
  scale_fill_gradientn(colors = pal_greys, na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = topo, show.legend = F) +
  scale_fill_hypso_tint_c(palette = "dem_poster",
                          alpha = 0.6,
                          na.value = "transparent") +

  # ── Capital cities ──
  geom_point(data = cities, aes(x = x, y = y),
             shape = 9, size = 1) +
  geom_text(data = cities, aes(x = lab_x, y = y, label = city, hjust = hjust),
            size = 3) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.key.size = unit(0.3, "cm"),
        legend.key.spacing.y = unit(0.1, "cm"),
        legend.key.spacing.x = unit(0.2, "cm"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_blank(),
        axis.ticks = element_line(colour = "grey80", linewidth = 0.3),
        legend.margin = margin(0, 0, 0, 0)) +
  labs(x = NULL, y = NULL) +
  coord_sf(xlim = c(plot_limits[1], plot_limits[2]),
           ylim = c(plot_limits[3], plot_limits[4]))

# p1

# Save plot (main map only, no insets - kept for reference/QA)
ggsave(paste(paste0('plots/', park, '/spatial/'), 'australia-overview.png'),
       plot = p1, dpi = 600, width = 8, height = 6, bg = "white")

# ==============================================================================
# 6. INSET MAPS FOR REMOTE TERRITORIES
# ==============================================================================

# Bounding boxes: c(xmin, xmax, ymin, ymax)
# NOTE: check these against your actual fed.mps/eez extents once rendered -
# tighten or widen so the full dashed EEZ circle is visible in each inset.
macquarie_bbox <- c(152, 165, -59, -50)
himi_bbox      <- c(66.3, 80.0, -57.3, -48.7)

make_inset <- function(bbox, title) {
  ext_box <- ext(bbox[1], bbox[2], bbox[3], bbox[4])
  bathy_c    <- crop(bathy, ext_box)
  topo_c     <- crop(topo, ext_box)
  hillbath_c <- crop(hillbath, ext_box)
  hill_c     <- crop(hill, ext_box)

  ggplot() +
    geom_spatraster(data = hillbath_c, fill = pal_greys[
      round(rescale(values(hillbath_c), to = c(1, length(pal_greys))))
    ], maxcell = Inf) +
    geom_spatraster(data = bathy_c, show.legend = FALSE, alpha = 0.6) +
    scale_fill_gradientn(colours = c("#061442", "#2b63b5", "#9dc9e1"),
                         values = rescale(c(-6221, -120, 0))) +
    new_scale_fill() +
    geom_sf(data = state.mps, aes(fill = sanctuary), colour = NA, show.legend = FALSE) +
    scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                 "State Marine Park" =  "grey80")) +
    new_scale_fill() +
    geom_sf(data = fed.mps, aes(fill = zone_type), colour = NA, alpha = 0.7,
            show.legend = FALSE) +
    scale_fill_manual(values = c(
      "Special Purpose Zone" = "#6daff4", "National Park Zone" = "#7bbc63",
      "Habitat Protection Zone" = "#fff8a3", "Recreational Use Zone" = "#ffb36b",
      "Multiple Use Zone" = "#b9e6fb", "Sanctuary Zone" = "#f7c0d8"
    )) +
    new_scale_fill() +
    geom_sf(data = eez, colour = "grey20", linetype = 2, linewidth = 0.4, fill = NA) +
    geom_spatraster(data = hill_c, alpha = 1, show.legend = FALSE) +
    scale_fill_gradientn(colors = pal_greys, na.value = NA) +
    new_scale_fill() +
    geom_spatraster(data = topo_c, show.legend = FALSE) +
    scale_fill_hypso_tint_c(palette = "dem_poster", alpha = 0.6, na.value = "transparent") +
    annotate("text", x = mean(bbox[1:2]), y = bbox[4], label = title,
             size = 2.3, vjust = -0.4, hjust = 0.5) +
    coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      panel.background = element_rect(fill = NA, colour = NA),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      plot.background  = element_rect(fill = NA, colour = NA),
      plot.margin = margin(t = 8, r = 2, b = 2, l = 2)
    )
}

# ── Vector-only inset (no bathymetry/topo raster) ──
# Needed for territories outside the AusBathyTopo raster's coverage
# (xmin/xmax 92.0-172.0 E, ymin/ymax -60.0 to -8.0 S). Heard & McDonald
# Islands sit at ~73 E, well outside this range, so crop() on bathy/topo
# fails with "extents do not overlap". Macquarie Island (~159 E) IS inside
# the raster extent, so it keeps using the full terrain-shaded make_inset().
make_inset_novector <- function(bbox, title) {
  ggplot() +
    geom_sf(data = state.mps, aes(fill = sanctuary), colour = NA, show.legend = FALSE) +
    scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                 "State Marine Park" = "grey80")) +
    new_scale_fill() +
    geom_sf(data = fed.mps, aes(fill = zone_type), colour = NA, alpha = 0.7,
            show.legend = FALSE) +
    scale_fill_manual(values = c(
      "Special Purpose Zone" = "#6daff4", "National Park Zone" = "#7bbc63",
      "Habitat Protection Zone" = "#fff8a3", "Recreational Use Zone" = "#ffb36b",
      "Multiple Use Zone" = "#b9e6fb", "Sanctuary Zone" = "#f7c0d8"
    )) +
    geom_sf(data = eez, colour = "grey20", linetype = 2, linewidth = 0.4, fill = NA) +
    annotate("text", x = mean(bbox[1:2]), y = bbox[4], label = title,
             size = 2.3, vjust = -0.4, hjust = 0.5) +
    coord_sf(xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE, clip = "off") +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "#54648B", colour = NA),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.6),
      plot.background  = element_rect(fill = NA, colour = NA),
      plot.margin = margin(t = 8, r = 2, b = 2, l = 2)
    )
}

inset_macquarie <- make_inset(macquarie_bbox, "Macquarie Island")
inset_himi      <- make_inset_novector(himi_bbox, "Heard & McDonald Islands")

# ── Compose main map + insets ──
# left/bottom/right/top are fractions (0-1) of the full p1 canvas.
# Adjust these once you see the rendered output.

p1 <- p1 + theme(plot.background = element_rect(fill = "white", colour = NA))

p1_final <- p1 +
  inset_element(inset_himi,      left = 0.02, bottom = 0.03, right = 0.22, top = 0.20) +
  inset_element(inset_macquarie, left = 0.80, bottom = 0.03, right = 0.98, top = 0.20)

# p1_final

ggsave(paste0('plots/', park, '/spatial/australia-overview.png'),
       plot = p1_final, dpi = 600, width = 8, height = 6, bg = "white")

# ==============================================================================
# End of script
# ==============================================================================
nrow(eez)
nrow(distinct(st_drop_geometry(eez)))

nrow(fed.mps)
fed.mps %>% st_drop_geometry() %>% count(polygonid) %>% filter(n > 1)

nrow(state.mps)
state.mps %>% st_drop_geometry() %>% count(pa_id, pa_pid) %>% filter(n > 1)

names(eez)
st_drop_geometry(eez) %>% distinct(across(everything())) %>% as.data.frame()
