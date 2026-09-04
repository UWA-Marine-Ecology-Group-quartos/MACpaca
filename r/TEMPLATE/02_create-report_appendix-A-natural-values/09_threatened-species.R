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
library(ggforce)
library(CheckEM)

# Make sure the output folders exist
for (d in c(paste0("plots/", park, "/fish/"),
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
  # Reported as the IUCN category. The species name carries a footnote marker
  # for its EPBC status instead - * Conservation Dependent, ^ Vulnerable.
  # Explained in the table caption in 10_quarto.qmd - change it there too if
  # this changes.
  dplyr::mutate(
    listing = dplyr::coalesce(iucn_ranking, "Not listed"),
    species_label = paste0(
      scientific_name,
      dplyr::case_when(
        epbc_threat_status %in% "Conservation Dependent" ~ "*",
        epbc_threat_status %in% "Vulnerable"              ~ "^",
        TRUE                                               ~ ""
      )
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
# Same extent as 02_spatial-layers.R for this park
e <- ext(NA, NA, NA, NA) # [TEMPLATE]

# TODO Check the shapefile paths match the network you are working in
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("[TEMPLATE]")) # TODO select relevant parks

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
# TODO set one colour per survey year to match years above
year_colours <- c("[TEMPLATE]" = "[TEMPLATE]") # [TEMPLATE]

# Faceted overview plots - one panel per species, 3 per row, and paginated
# at 4 rows (12 species) per page so a long species list splits across pages
# instead of one huge figure. Labels are wrapped so long names (e.g.
# Rhynchobatus australiae) don't get clipped at the panel edge.
facet_cols    <- 3
rows_per_page <- 4
spp_per_page  <- facet_cols * rows_per_page

save_paginated_overview <- function(occ_points, file_stem) {
  # Species are split into pages by hand (rather than letting
  # ggforce::facet_wrap_paginate do it) so each page's facet_wrap only
  # reserves as many rows as it actually needs. facet_wrap_paginate always
  # grids at the full rows_per_page, so a partial last page left a blank
  # row of white space between the last real row and the legend.
  spp_order <- sort(unique(occ_points$scientific_name))
  n_pages   <- ceiling(length(spp_order) / spp_per_page)

  for (pg in seq_len(n_pages)) {

    spp_page  <- spp_order[(((pg - 1) * spp_per_page) + 1):
                              min(pg * spp_per_page, length(spp_order))]
    page_nrow <- ceiling(length(spp_page) / facet_cols)

    page_points <- occ_points %>%
      dplyr::filter(scientific_name %in% spp_page) %>%
      dplyr::mutate(scientific_name = factor(scientific_name, levels = spp_page))

    p <- ggplot() +
      base_map() +
      geom_point(data = all_drops,
                 aes(x = longitude_dd, y = latitude_dd),
                 colour = "grey80", size = 0.3) +
      geom_point(data = page_points,
                 # Sizes run from MaxN 1 (smallest dot) to MaxN 4 (largest
                 # dot); anything above 4 is capped here (rather than via
                 # scale `oob`, which this ggplot2 version doesn't accept)
                 # so a few high-count outliers don't shrink every other dot
                 # down to near-invisible.
                 aes(x = longitude_dd, y = latitude_dd, size = pmin(count, 4),
                     colour = as.factor(year)),
                 alpha = 0.8) +
      scale_size_continuous(name = "MaxN", range = c(1.5, 5),
                            limits = c(1, 4), breaks = 1:4,
                            labels = c("1", "2", "3", "4+")) +
      scale_colour_manual(name = "Year", values = year_colours) +
      # override.aes bumps up the Year legend's dot size - the actual points
      # on the map stay small (alpha = 0.8 above), just the legend key
      # swatches are drawn bigger so the colours are easier to tell apart.
      guides(colour = guide_legend(order = 1, override.aes = list(size = 6)),
             size = guide_legend(order = 2)) +
      facet_wrap(~scientific_name, ncol = facet_cols, nrow = page_nrow,
                 labeller = ggplot2::label_wrap_gen(width = 14)) +
      theme(strip.text = element_text(face = "italic", size = 16),
            axis.text = element_text(size = 18),
            legend.text = element_text(size = 19),
            legend.title = element_text(face = "bold", size = 21),
            legend.key.size = unit(1.4, "lines"),
            legend.position = "bottom",
            legend.box = "vertical",
            legend.box.spacing = unit(2, "pt"),
            panel.spacing = unit(1.2, "lines"))

    print(p)

    page_stem <- paste0("plots/", park, "/fish/", name, "_", file_stem,
                        "_page", pg, "_", paste(years, collapse = "-"))

    # RDS is what the report reads - fig-width/fig-height are set by hand in
    # the qmd, same as every other RDS figure in this appendix. PNG is a
    # fixed-size copy saved alongside, in case it's ever needed.
    ggsave(
      filename = paste0(page_stem, ".png"),
      plot = p,
      height = 8, width = 10, dpi = 600, units = "in", bg = "white"
    )
    saveRDS(p, paste0(page_stem, ".rds"))
  }

  message(file_stem, ": ", dplyr::n_distinct(occ_points$scientific_name),
          " species across ", n_pages, " page(s).")
  n_pages
}

# Threatened species - occurrence overview ---------------------------------
occ_points <- threatened_obs_elasmo %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd))

save_paginated_overview(occ_points, "threatened-species-occurrence")

# Non-threatened sharks and rays - occurrence overview ----------------------
# Every other observed elasmobranch, i.e. not already covered by the
# threatened species plots above.
non_threatened_obs <- observations %>%
  dplyr::filter(class %in% elasmo_classes) %>%
  dplyr::filter(!(epbc_threat_status %in% epbc_categories |
                    iucn_ranking %in% iucn_categories)) %>%
  dplyr::filter(!scientific_name %in% "Unknown spp") %>%
  dplyr::left_join(
    metadata_fish %>%
      dplyr::select(campaignid, sample, year, status,
                    longitude_dd, latitude_dd),
    by = c("campaignid", "sample")
  )

if (nrow(non_threatened_obs) == 0) {
  stop("No non-threatened shark/ray species found. Check elasmo_classes ",
       "against the class values printed above.")
}

occ_points_other <- non_threatened_obs %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd))

save_paginated_overview(occ_points_other, "non-threatened-species-occurrence")
