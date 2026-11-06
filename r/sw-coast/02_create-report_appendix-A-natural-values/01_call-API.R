###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Marine Park monitoring data syntheses - Abrolhos to Eastern Recherche
# Task:    Call GlobalArchive API to download and combine every synthesis along the coast
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
###
rm(list = ls())

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# Load libraries needed -----

# TODO Run these once or as required:
# remotes::install_github("GlobalArchiveManual/CheckEM")
# CheckEM::ga_api_set_token()

library(tidyverse)
library(CheckEM)
library(sf)
options(timeout=600) # increase if more time needed for large data downloads

# Make sure the output folders exist
for (d in c(paste0("data/", park, "/raw/"), paste0("data/", park, "/tidy/"))) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Syntheses ----
# TODO Check this is every synthesis between the Abrolhos and Eastern Recherche
# BRUV - fish and benthos
bruv_ids <- c(abrolhos          = "71",
              geographe         = "72",
              swc_westernarm    = "76",
              swc_easternarm    = "69",
              eastern_recherche = "70")

# BOSS - benthos only. 78 is run before 77 so the shared Investigator campaign
# comes from 78, which has clean coordinates
boss_ids <- c(abrolhos          = "84",
              two_rocks         = "86",
              geographe         = "85",
              swc_westernarm    = "87",
              swc_easternarm    = "78",
              eastern_recherche = "77")

# Study extent - Abrolhos (north) to Eastern Recherche (east)
# TODO Check nothing you want is dropped by this
e_study <- c(xmin = 108, xmax = 124.5, ymin = -38, ymax = -24)

# Bind a list of tables from different syntheses. The same column can come
# back as a different type from each synthesis, so mismatches are reconciled
bind_syntheses <- function(lst) {
  lst <- lapply(lst, function(x) {
    if ("date_time" %in% names(x) && is.character(x$date_time)) {
      x$date_time <- lubridate::as_datetime(x$date_time, tz = "UTC")
    }
    x
  })
  cols <- unique(unlist(lapply(lst, names)))
  for (col in cols) {
    cls <- unique(unlist(lapply(lst, function(x) if (col %in% names(x)) class(x[[col]])[1])))
    if (length(cls) > 1) {
      to <- if (all(cls %in% c("integer", "numeric", "logical"))) as.numeric else as.character
      lst <- lapply(lst, function(x) { if (col %in% names(x)) x[[col]] <- to(x[[col]]); x })
    }
  }
  bind_rows(lst)
}

# BRUV ----
# Each synthesis goes in its own folder so the files don't overwrite.
bruv_meta <- list(); bruv_benthos <- list(); bruv_count <- list(); bruv_length <- list()
seen <- character()

for (id in bruv_ids) {
  message("BRUV synthesis ", id)
  dir_id <- paste0("data/", park, "/raw/synthesis_", id, "/")
  if (!dir.exists(dir_id)) dir.create(dir_id, recursive = TRUE)

  CheckEM::ga_api_all_data(synthesis_id = id,
                           token = token,
                           dir = dir_id,
                           include_zeros = TRUE)

  m <- readRDS(paste0(dir_id, "metadata.RDS"))
  keep <- setdiff(unique(m$campaignid), seen)
  seen <- c(seen, keep)

  bruv_meta[[id]]    <- m %>% dplyr::filter(campaignid %in% keep)
  bruv_benthos[[id]] <- readRDS(paste0(dir_id, "benthos_summarised.RDS")) %>% dplyr::filter(campaignid %in% keep)
  bruv_count[[id]]   <- readRDS(paste0(dir_id, "_count-with-zeros.RDS"))  %>% dplyr::filter(campaignid %in% keep)
  bruv_length[[id]]  <- readRDS(paste0(dir_id, "_length-with-zeros.RDS")) %>% dplyr::filter(campaignid %in% keep)
}

metadata <- bind_syntheses(bruv_meta) %>%
  mutate(method = "BRUV")

benthos_summarised <- bind_syntheses(bruv_benthos)

# Zeros only come back for the species in each synthesis, so after binding a
# species seen in one synthesis has no zero rows in the others - fill them in
complete_zeros <- function(df, meta) {
  sample_cols  <- setdiff(intersect(names(df), names(meta)), c("campaignid", "sample"))
  species_cols <- setdiff(names(df), c("campaignid", "sample", "family", "genus", "species",
                                       "count", "length_mm", sample_cols))
  df %>%
    tidyr::complete(tidyr::nesting(campaignid, sample),
                    tidyr::nesting(family, genus, species),
                    fill = list(count = 0)) %>%
    dplyr::group_by(campaignid, sample) %>%
    tidyr::fill(dplyr::all_of(sample_cols), .direction = "downup") %>%
    dplyr::group_by(family, genus, species) %>%
    tidyr::fill(dplyr::all_of(species_cols), .direction = "downup") %>%
    dplyr::ungroup()
}

count  <- bind_syntheses(bruv_count)  %>% complete_zeros(metadata)
length <- bind_syntheses(bruv_length) %>% complete_zeros(metadata)

# BOSS ----
habitat_classes <- c("consolidated", "macroalgae", "sessile_invertebrates",
                     "seagrasses", "unconsolidated", "unscorable")

# Replicates the CheckEM summarising applied to the BRUV synthesis
summarise_benthos <- function(raw, meta) {

  wide <- raw %>%
    dplyr::mutate(
      habitat = dplyr::case_when(
        level_2 == "Substrate" & level_3 == "Consolidated (hard)"   ~ "consolidated",
        level_2 == "Substrate" & level_3 == "Unconsolidated (soft)" ~ "unconsolidated",
        level_2 == "Macroalgae"                                     ~ "macroalgae",
        level_2 == "Seagrasses"                                     ~ "seagrasses",
        level_2 == "Unscorable"                                     ~ "unscorable",
        level_2 == "Fishes"                                         ~ NA_character_,
        level_1 == "Biota"                                          ~ "sessile_invertebrates",
        .default = NA_character_
      )
    ) %>%
    dplyr::group_by(sample_url) %>%
    dplyr::mutate(total_points_annotated = sum(count, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(habitat)) %>%
    dplyr::group_by(sample_url, total_points_annotated, habitat) %>%
    dplyr::summarise(n = sum(count, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = habitat, values_from = n, values_fill = 0)

  # Add back any class absent from these annotations, as a true zero
  missing <- setdiff(habitat_classes, names(wide))
  if (length(missing) > 0) wide[missing] <- 0

  wide %>%
    dplyr::left_join(dplyr::distinct(meta, sample_url, campaignid, sample),
                     by = "sample_url") %>%
    dplyr::select(sample_url, campaignid, sample, total_points_annotated,
                  all_of(habitat_classes))
}

boss_meta <- list(); boss_benthos <- list()

for (id in boss_ids) {
  message("BOSS synthesis ", id)

  # ga_api_all_data() cannot be used on a benthos only synthesis, so the
  # metadata and habitat endpoints are called directly
  bm <- CheckEM::ga_api_metadata(token = token, synthesis_id = id)

  # Some syntheses return longitude/latitude rather than longitude_dd/latitude_dd
  if (!"longitude_dd" %in% names(bm)) {
    bm <- bm %>% dplyr::rename(longitude_dd = longitude, latitude_dd = latitude)
  }

  # Synthesis 77 returns broken coordinates - repaired from the field sheet
  # (same fix as eastern-recherche/01_call-API.R)
  if (id == "77") {
    field <- read_csv("data/eastern-recherche/raw/Salisbury_Investigator_MBH_BOSS_habitat_Metadata.csv",
                      show_col_types = FALSE) %>%
      dplyr::select(sample_key = Sample, longitude_field = Longitude, latitude_field = Latitude) %>%
      dplyr::distinct(sample_key, .keep_all = TRUE)

    bm <- bm %>%
      mutate(sample_key = str_remove(sample, "^NA_")) %>%
      left_join(field, by = "sample_key") %>%
      mutate(longitude_dd = longitude_field,
             latitude_dd  = latitude_field) %>%
      dplyr::select(-sample_key, -longitude_field, -latitude_field)
  }

  keep <- setdiff(unique(bm$campaignid), seen)
  seen <- c(seen, keep)
  bm <- bm %>% dplyr::filter(campaignid %in% keep)

  if (nrow(bm) == 0) next

  raw <- CheckEM::ga_api_habitat(token = token, synthesis_id = id) %>%
    semi_join(bm, by = "sample_url")

  boss_meta[[id]]    <- bm
  boss_benthos[[id]] <- summarise_benthos(raw, bm)
}

boss_metadata <- bind_syntheses(boss_meta) %>%
  mutate(method = "BOSS")

boss_benthos_summarised <- bind_syntheses(boss_benthos)

saveRDS(boss_metadata,           paste0("data/", park, "/raw/boss_metadata.RDS"))
saveRDS(boss_benthos_summarised, paste0("data/", park, "/raw/boss_benthos_summarised.RDS"))

# Combine BRUV and BOSS ----
metadata <- bind_syntheses(list(metadata, boss_metadata)) %>%
  dplyr::filter(dplyr::between(longitude_dd, e_study[["xmin"]], e_study[["xmax"]]),
                dplyr::between(latitude_dd,  e_study[["ymin"]], e_study[["ymax"]]))

# TODO Check every campaign is here and sits where you expect
metadata %>%
  dplyr::group_by(method, campaignid) %>%
  dplyr::summarise(n = dplyr::n_distinct(sample),
                   xmin = round(min(longitude_dd), 2), xmax = round(max(longitude_dd), 2),
                   ymin = round(min(latitude_dd), 2),  ymax = round(max(latitude_dd), 2),
                   .groups = "drop") %>%
  print(n = Inf)

# Status ----
# Set spatially so it is consistent across syntheses - samples in a National
# Park Zone or state Sanctuary Zone are No-Take, everything else is Fished.
# These zone_types must match the ones used to predict in 05 and 06
sf_use_s2(FALSE)

no_take <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(zone_type %in% c("National Park Zone (IUCN II)",
                                 "Sanctuary Zone (IUCN VI)",
                                 "Sanctuary Zone (IUCN IA)",
                                 "Sanctuary Zone")) %>%
  st_transform(4326) %>%
  st_make_valid()

in_no_take <- metadata %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_intersects(no_take) %>%
  lengths() > 0

# Convert to factors now that BOSS has been combined in
metadata <- metadata %>%
  # Some 2022 campaign dates are entered as 2002 in GlobalArchive
  mutate(date_time = if_else(year(date_time) == 2002 & str_starts(campaignid, "2022"),
                             update(date_time, year = 2022), date_time)) %>%
  mutate(status = if_else(in_no_take, "No-Take", "Fished"),
         method = as.factor(method),
         year   = as.factor(year(date_time)),
         status = as.factor(status)) %>%
  glimpse()

# TODO Check the years against 00_config.yml
metadata %>%
  dplyr::count(method, year, status) %>%
  print(n = Inf)

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# Combine the BRUV and BOSS benthos - bind_rows leaves NAs where a class was
# absent in one synthesis; these are true zeros. Percentages are recalculated
benthos_summarised <- bind_syntheses(list(benthos_summarised, boss_benthos_summarised)) %>%
  dplyr::select(-ends_with("_percent")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  mutate(across(-c(sample_url, campaignid, sample, total_points_annotated),
                ~ .x / total_points_annotated,
                .names = "{.col}_percent")) %>%
  semi_join(metadata, by = c("campaignid", "sample"))

# Fish data trimmed to the study extent
count  <- count  %>% semi_join(metadata, by = c("campaignid", "sample"))
length <- length %>% semi_join(metadata, by = c("campaignid", "sample"))

saveRDS(count,  paste0("data/", park, "/raw/_count-with-zeros.RDS"))
saveRDS(length, paste0("data/", park, "/raw/_length-with-zeros.RDS"))
saveRDS(metadata %>% dplyr::filter(method %in% "BRUV"),
        paste0("data/", park, "/raw/bruv_metadata.RDS"))

# Tidy and join habitat with metadata
tidy_habitat <- benthos_summarised %>%
  left_join(metadata, by = c("sample_url", "campaignid", "sample")) %>%
  glimpse()

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

# Create the sampling effort summary table ----
sf_use_s2(FALSE)

count  <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS"))
length <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS"))

# Which AMP zone does each sample sit in? ----
# TODO Check the shapefile path matches the network you are working in
marine_parks_amp <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  # Commonwealth only - drop this filter (and add epbc to the select below) if
  # you also want to report state marine park zones
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326) %>%
  dplyr::select(park_name = name, zone) %>%
  st_make_valid()

# Samples that fall outside any Commonwealth park return NA from the join
sample_zones <- metadata %>%
  dplyr::distinct(campaignid, method, sample, longitude_dd, latitude_dd) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_join(marine_parks_amp, join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  mutate(zone = replace_na(zone, "Coastal waters"))

# TODO Check these are the zones you expect - the labels must match the ones in
# zone_abbrev below or the abbreviation will come back as NA.
sample_zones %>%
  dplyr::count(method, park_name, zone) %>%
  print(n = Inf)

# Shortened zone names to keep the table narrow enough for the PDF page
zone_abbrev <- c("National Park Zone"      = "NPZ",
                 "Habitat Protection Zone" = "HPZ",
                 "Multiple Use Zone"       = "MUZ",
                 "Special Purpose Zone"    = "SPZ",
                 "Recreational Use Zone"   = "RUZ",
                 "Sanctuary Zone"          = "SZ",
                 "General Use Zone"        = "GUZ",
                 "Coastal waters"          = "Coastal waters")

zones_by_campaign <- sample_zones %>%
  mutate(zone_short = dplyr::coalesce(unname(zone_abbrev[zone]), zone)) %>%
  # Several parks in this report, so the park name goes in front of the zone
  mutate(zone_short = if_else(is.na(park_name), zone_short, paste(park_name, zone_short))) %>%
  dplyr::distinct(campaignid, method, zone_short) %>%
  dplyr::arrange(campaignid, method, zone_short) %>%
  dplyr::group_by(campaignid, method) %>%
  dplyr::summarise(areas = paste(zone_short, collapse = ", "), .groups = "drop")

# Which data types were collected in each campaign? ----
samples <- metadata %>%
  dplyr::distinct(campaignid, method, sample)

n_samples_with <- function(df, label) {
  samples %>%
    dplyr::semi_join(dplyr::distinct(df, campaignid, sample),
                     by = c("campaignid", "sample")) %>%
    dplyr::count(campaignid, method, name = label)
}

data_types <- samples %>%
  dplyr::distinct(campaignid, method) %>%
  left_join(n_samples_with(benthos_summarised, "habitat_count"), by = c("campaignid", "method")) %>%
  left_join(n_samples_with(count,  "fish_count"),  by = c("campaignid", "method")) %>%
  left_join(n_samples_with(length, "fish_length"), by = c("campaignid", "method")) %>%
  mutate(across(c(habitat_count, fish_count, fish_length), ~ replace_na(.x, 0)))

# Sampling dates, as a month (or month range) and year ----
format_month_range <- function(start, end) {
  start_year  <- year(start)
  end_year    <- year(end)
  start_month <- as.character(month(start, label = TRUE, abbr = TRUE))
  end_month   <- as.character(month(end,   label = TRUE, abbr = TRUE))

  dplyr::case_when(
    start_year == end_year & start_month == end_month ~ paste(start_month, start_year),
    start_year == end_year ~ paste0(start_month, "-", end_month, " ", start_year),
    .default = paste0(start_month, " ", start_year, " - ", end_month, " ", end_year)
  )
}

# Build the table ----
sampling_summary <- metadata %>%
  dplyr::distinct(campaignid, method, sample, date_time) %>%
  dplyr::group_by(campaignid, method) %>%
  dplyr::summarise(date_start = min(date_time, na.rm = TRUE),
                   date_end   = max(date_time, na.rm = TRUE),
                   n_samples  = dplyr::n_distinct(sample),
                   .groups = "drop") %>%
  mutate(dates = format_month_range(date_start, date_end)) %>%
  left_join(zones_by_campaign, by = c("campaignid", "method")) %>%
  left_join(data_types,        by = c("campaignid", "method")) %>%
  dplyr::arrange(date_start, campaignid) %>%
  dplyr::select(dates, campaignid, method, areas, n_samples,
                habitat_count, fish_count, fish_length)


# TODO Check the sample numbers match what you expect from the field
print(sampling_summary, n = Inf)

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.RDS"))
write_csv(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.csv"))
