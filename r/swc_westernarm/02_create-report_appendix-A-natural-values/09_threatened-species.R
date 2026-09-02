###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Fish data synthesis (stereo-BRUV MaxN) + CheckEM life history
# Task:    List threatened species recorded in the samples and map where they
#          were seen
# Author:  Annika Leunig
# Date:    July 2026
###

# Clear your environment
rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park
# This is a record of what was observed, not a model, so it uses every survey
# year rather than the fish modelling subset. Years with no BRUV campaign
# contribute nothing, since only the BRUVs carry fish counts.
years <- unlist(config$years)

# Load libraries
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(ggnewscale)
library(CheckEM)

# Make sure the output folders exist
for (d in c(paste0("plots/", park, "/fish/threatened/"),
            paste0("data/", park, "/tidy/"))) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Which listing categories count as threatened ----------------------------
# TODO Check these against the values printed further down. A category spelled
# differently in CheckEM to the way it is written here will be silently dropped.
# Near Threatened is not a threatened category under the IUCN Red List proper -
# drop it from iucn_categories if the report should only carry VU/EN/CR.
epbc_categories <- c("Critically Endangered", "Endangered", "Vulnerable",
                     "Conservation Dependent")

iucn_categories <- c("Critically Endangered", "Endangered", "Vulnerable",
                     "Near Threatened")

# Sharks and rays only. TODO Check these against the class values printed
# further down - CheckEM is not always consistent about Chondrichthyes vs
# Elasmobranchii, and chimaeras sit under Holocephali.
elasmo_classes <- c("Elasmobranchii", "Chondrichthyes", "Holocephali")

# Data --------------------------------------------------------------------
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS"))

if (!"method" %in% names(metadata)) {
  stop("metadata.RDS has no `method` column - re-run 01_call-API.R, which adds ",
       "it when the BRUV and BOSS syntheses are combined.")
}

# Fish records come from the BRUVs only, and only for the years modelled
metadata_fish <- metadata %>%
  dplyr::filter(method %in% "BRUV") %>%
  dplyr::filter(as.character(year) %in% years) %>%
  dplyr::select(-method)

message("BRUV samples used: ", nrow(metadata_fish),
        " across years ", paste(sort(unique(as.character(metadata_fish$year))),
                                collapse = ", "))

# _count-with-zeros.RDS carries a row per species per sample including true
# absences, so filtering to count > 0 below gives genuine occurrence records
count <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  dplyr::semi_join(metadata_fish, by = c("campaignid", "sample")) %>%
  dplyr::select(campaignid, sample, family, genus, species, count)

# Listing status comes from the CheckEM life history table. class is carried
# through so elasmobranchs can be told apart from bony fishes in the output -
# most threatened records on BRUVs are sharks and rays.
life_history <- CheckEM::australia_life_history %>%
  dplyr::select(family, genus, species, class,
                australian_common_name,
                epbc_threat_status, iucn_ranking) %>%
  dplyr::distinct()

count_lh <- count %>%
  dplyr::left_join(life_history, by = c("family", "genus", "species")) %>%
  dplyr::mutate(scientific_name = paste(genus, species))

# Everything actually seen, one row per species per sample
observations <- count_lh %>%
  dplyr::filter(count > 0)

# TODO Check nothing important failed the life history join. Anything listed
# here has no listing status and cannot be assessed - usually unresolved
# genus/spp records, but check for real species that are simply missing.
unmatched <- observations %>%
  dplyr::filter(is.na(class) & is.na(epbc_threat_status) & is.na(iucn_ranking)) %>%
  dplyr::distinct(family, genus, species)

message("Species observed with no life history match: ", nrow(unmatched))
if (nrow(unmatched) > 0) print(unmatched, n = Inf)

# TODO Check the categories present in the data match the two vectors above
message("EPBC values present:")
print(sort(unique(observations$epbc_threat_status)))

message("IUCN values present:")
print(sort(unique(observations$iucn_ranking)))

# TODO Check the class values match elasmo_classes above
message("Classes present:")
print(sort(unique(observations$class)))

# Threatened species list -------------------------------------------------
# The table further down covers every threatened species; the plots later in
# this script narrow it to sharks and rays only.
threatened_obs <- observations %>%
  dplyr::filter(epbc_threat_status %in% epbc_categories |
                  iucn_ranking %in% iucn_categories) %>%
  dplyr::left_join(
    metadata_fish %>%
      dplyr::select(campaignid, sample, year, status,
                    longitude_dd, latitude_dd),
    by = c("campaignid", "sample")
  )

if (nrow(threatened_obs) == 0) {
  stop("No threatened species found. Check the category spellings printed ",
       "above against epbc_categories and iucn_categories.")
}

threatened_species <- threatened_obs %>%
  dplyr::group_by(scientific_name, australian_common_name, family, class,
                  epbc_threat_status, iucn_ranking) %>%
  dplyr::summarise(
    n_samples   = dplyr::n_distinct(paste(campaignid, sample)),
    total_maxn  = sum(count, na.rm = TRUE),
    max_maxn    = max(count, na.rm = TRUE),
    years_seen  = paste(sort(unique(as.character(year))), collapse = ", "),
    zones_seen  = paste(sort(unique(as.character(status))), collapse = ", "),
    .groups = "drop"
  ) %>%
  # Reported as the IUCN category, with an asterisk marking species that are
  # also listed under the EPBC Act. The asterisk is explained in the table
  # caption in 09_quarto.qmd - change it there too if this changes.
  dplyr::mutate(
    listing = paste0(
      dplyr::coalesce(iucn_ranking, "Not listed"),
      dplyr::if_else(epbc_threat_status %in% epbc_categories, "*", "")
    )
  ) %>%
  dplyr::arrange(dplyr::desc(n_samples), scientific_name)

# TODO Check this list against the current EPBC and IUCN listings before it goes
# in the report - the CheckEM table is a snapshot and listings are revised
print(threatened_species, n = Inf)

saveRDS(threatened_species,
        paste0("data/", park, "/tidy/", name, "_threatened-species.RDS"))
write_csv(threatened_species,
          paste0("data/", park, "/tidy/", name, "_threatened-species.csv"))

saveRDS(threatened_obs,
        paste0("data/", park, "/tidy/", name, "_threatened-occurrences.RDS"))

# Plots below are sharks and rays only - narrow the occurrence records here.
# The table above keeps every threatened species.
threatened_obs_elasmo <- threatened_obs %>%
  dplyr::filter(class %in% elasmo_classes)

message("Shark and ray occurrence records for the plots: ",
        nrow(threatened_obs_elasmo), " across ",
        dplyr::n_distinct(threatened_obs_elasmo$scientific_name), " species")

# Spatial layers for the maps ---------------------------------------------
sf_use_s2(FALSE)

# TODO Set cropping extent - larger than the plot window
e <- ext(114.0, 116.0, -34.7, -33.1)

# TODO Check the shapefile paths match the network you are working in
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

ausc <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

# Frame the maps on the BRUV samples rather than a hardcoded box
# TODO Widen map_pad if the coastline falls outside the frame
map_pad <- 0.1

map_limits <- metadata_fish %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd)) %>%
  dplyr::summarise(
    xmin = min(longitude_dd) - map_pad,
    xmax = max(longitude_dd) + map_pad,
    ymin = min(latitude_dd)  - map_pad,
    ymax = max(latitude_dd)  + map_pad
  ) %>%
  unlist(use.names = FALSE)

message("Map extent: ", paste(round(map_limits, 3), collapse = ", "))

# All BRUV drops, drawn underneath the occurrences so absence is visible
all_drops <- metadata_fish %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd)) %>%
  dplyr::distinct(campaignid, sample, longitude_dd, latitude_dd)

# TODO Spacing of the longitude and latitude labels, in degrees
label_interval_x <- 0.3
label_interval_y <- 0.3

x_breaks <- seq(floor(map_limits[1] / label_interval_x) * label_interval_x,
                ceiling(map_limits[2] / label_interval_x) * label_interval_x,
                by = label_interval_x)

y_breaks <- seq(floor(map_limits[3] / label_interval_y) * label_interval_y,
                ceiling(map_limits[4] / label_interval_y) * label_interval_y,
                by = label_interval_y)

base_map <- function() {
  list(
    geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.1),
    geom_sf(data = marine_parks_amp, fill = NA, colour = "#7bbc63", linewidth = 0.2),
    geom_sf(data = marine_parks_state, fill = NA, colour = "#bfd054", linewidth = 0.2),
    geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.3, alpha = 0.8),
    scale_x_continuous(breaks = x_breaks),
    scale_y_continuous(breaks = y_breaks),
    coord_sf(xlim = map_limits[1:2], ylim = map_limits[3:4], crs = 4326),
    labs(x = NULL, y = NULL),
    theme_minimal()
  )
}

# Bright, distinct colours per survey year - one entry per year actually
# present in the threatened species observations.
year_colours <- c("2020" = "#9C27B0", "2023" = "#1E88E5",
                   "2024" = "#009688", "2025" = "#43A047")

# Faceted overview - one panel per threatened species ---------------------
occ_points <- threatened_obs_elasmo %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd))

p_overview <- ggplot() +
  base_map() +
  geom_point(data = all_drops,
             aes(x = longitude_dd, y = latitude_dd),
             colour = "grey80", size = 0.3) +
  geom_point(data = occ_points,
             aes(x = longitude_dd, y = latitude_dd, size = count,
                 colour = as.factor(year)),
             alpha = 0.8) +
  scale_size_continuous(name = "MaxN", range = c(1.5, 5)) +
  scale_colour_manual(name = "Year", values = year_colours) +
  guides(colour = guide_legend(order = 1),
         size = guide_legend(order = 2)) +
  facet_wrap(~scientific_name) +
  theme(strip.text = element_text(face = "italic"),
        legend.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.box = "vertical")

print(p_overview)

# TODO Height scales with the number of species - check the rendered figure
n_spp <- dplyr::n_distinct(occ_points$scientific_name)
overview_height <- max(5, 2.6 * ceiling(n_spp / 3))

ggsave(
  filename = paste0("plots/", park, "/fish/", name,
                    "_threatened-species-occurrence_",
                    paste(years, collapse = "-"), ".png"),
  plot = p_overview,
  height = overview_height, width = 9, dpi = 300, units = "in", bg = "white"
)

saveRDS(p_overview,
        paste0("plots/", park, "/fish/", name,
               "_threatened-species-occurrence_",
               paste(years, collapse = "-"), ".rds"))

# One map per species -----------------------------------------------------
for (spp in sort(unique(occ_points$scientific_name))) {

  message("Building occurrence map for: ", spp)

  spp_points <- occ_points %>%
    dplyr::filter(scientific_name %in% spp)

  spp_row <- threatened_species %>%
    dplyr::filter(scientific_name %in% spp) %>%
    dplyr::slice(1)

  subtitle <- paste0(
    dplyr::coalesce(spp_row$australian_common_name, "No common name"),
    " | EPBC: ", dplyr::coalesce(spp_row$epbc_threat_status, "Not listed"),
    " | IUCN: ", dplyr::coalesce(spp_row$iucn_ranking, "Not listed")
  )

  p_spp <- ggplot() +
    base_map() +
    geom_point(data = all_drops,
               aes(x = longitude_dd, y = latitude_dd),
               colour = "grey80", size = 0.5) +
    geom_point(data = spp_points,
               aes(x = longitude_dd, y = latitude_dd, size = count,
                   colour = as.factor(year)),
               alpha = 0.85) +
    scale_size_continuous(name = "MaxN", range = c(2, 6)) +
    scale_colour_manual(name = "Year", values = year_colours) +
    guides(colour = guide_legend(order = 1),
           size = guide_legend(order = 2)) +
    labs(title = spp, subtitle = subtitle) +
    theme(plot.title = element_text(face = "bold.italic"),
          legend.title = element_text(face = "bold"))

  print(p_spp)

  out_name <- spp %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\s+", "-")

  ggsave(
    filename = paste0("plots/", park, "/fish/threatened/", name,
                      "_threatened-occurrence_", out_name, ".png"),
    plot = p_spp,
    height = 5, width = 6, dpi = 300, units = "in", bg = "white"
  )

  saveRDS(p_spp,
          paste0("plots/", park, "/fish/threatened/", name,
                 "_threatened-occurrence_", out_name, ".rds"))
}

# Species x year grid - rows are species, columns are year, 4 species per
# page so the grid stays readable ------------------------------------------
grid_years <- sort(unique(occ_points$year))
spp_list   <- sort(unique(occ_points$scientific_name))

spp_per_page <- 4
n_pages <- ceiling(length(spp_list) / spp_per_page)

for (pg in seq_len(n_pages)) {

  spp_page <- spp_list[(((pg - 1) * spp_per_page) + 1):
                          min(pg * spp_per_page, length(spp_list))]

  message("Building species x year grid, page ", pg, ": ",
          paste(spp_page, collapse = ", "))

  grid_points <- occ_points %>%
    dplyr::filter(scientific_name %in% spp_page) %>%
    dplyr::mutate(scientific_name = factor(scientific_name, levels = spp_page),
                  year = factor(year, levels = grid_years))

  p_grid <- ggplot() +
    base_map() +
    geom_point(data = all_drops,
               aes(x = longitude_dd, y = latitude_dd),
               colour = "grey80", size = 0.3) +
    geom_point(data = grid_points,
               aes(x = longitude_dd, y = latitude_dd, size = count,
                   colour = year),
               alpha = 0.8) +
    scale_size_continuous(name = "MaxN", range = c(1.5, 5)) +
    scale_colour_manual(name = "Year", values = year_colours, drop = FALSE) +
    guides(colour = guide_legend(order = 1),
           size = guide_legend(order = 2)) +
    facet_grid(scientific_name ~ year, drop = FALSE) +
    theme(strip.text.y = element_text(face = "italic"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.box = "vertical")

  print(p_grid)

  ggsave(
    filename = paste0("plots/", park, "/fish/", name,
                      "_threatened-species-grid_page-", pg, ".png"),
    plot = p_grid,
    height = 10, width = 9, dpi = 300, units = "in", bg = "white"
  )

  saveRDS(p_grid,
          paste0("plots/", park, "/fish/", name,
                 "_threatened-species-grid_page-", pg, ".rds"))
}
