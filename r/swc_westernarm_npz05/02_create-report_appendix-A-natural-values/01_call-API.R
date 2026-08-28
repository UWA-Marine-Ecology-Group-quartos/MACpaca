###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Marine Park monitoring data syntheses
# Task:    Call GlobalArchive API to download data syntheses
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

# Load the metadata, count and length ----
# Synthesis 76 = South-west Corner stereo-BRUVs
CheckEM::ga_api_all_data(synthesis_id = "76",
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE)

# ga_api_all_data() writes to dir and leaves objects in the global environment.
# Read them back explicitly so this script does not depend on that behaviour.
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  mutate(method = "BRUV")

benthos_summarised <- readRDS(paste0("data/", park, "/raw/benthos_summarised.RDS"))

# Keep an unmodified copy of the BRUV-only objects. metadata.RDS is overwritten
# with the combined BRUV + BOSS version at the end of this script, so re-running
# from part way through would otherwise double up the BOSS rows.
saveRDS(metadata,           paste0("data/", park, "/raw/bruv_metadata.RDS"))
saveRDS(benthos_summarised, paste0("data/", park, "/raw/bruv_benthos_summarised.RDS"))

# BOSS ----
boss_metadata <- CheckEM::ga_api_metadata(token = token,
                                          synthesis_id = "87") %>%
  mutate(method = "BOSS")

# TODO Check the campaigns and their spatial spread against the BRUVs
message("BOSS campaigns:")
boss_metadata %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(n = dplyr::n_distinct(sample),
                   xmin = round(min(longitude_dd, na.rm = TRUE), 4),
                   xmax = round(max(longitude_dd, na.rm = TRUE), 4),
                   ymin = round(min(latitude_dd,  na.rm = TRUE), 4),
                   ymax = round(max(latitude_dd,  na.rm = TRUE), 4),
                   .groups = "drop") %>%
  print(n = Inf)

message("BRUV footprint for comparison: ",
        paste(round(c(min(metadata$longitude_dd), max(metadata$longitude_dd),
                      min(metadata$latitude_dd),  max(metadata$latitude_dd)), 4),
              collapse = ", "))

saveRDS(boss_metadata, paste0("data/", park, "/raw/boss_metadata.RDS"))

# ga_api_habitat() returns the RAW long-format benthos counts (one row per CATAMI
# class per sample), not the summarised wide table that ga_api_all_data() saves.
# It has to be pivoted the same way before it will bind to benthos_summarised.
boss_benthos_raw <- CheckEM::ga_api_habitat(token = token,
                                            synthesis_id = "87") %>%
  semi_join(boss_metadata, by = "sample_url")

# Fail loudly rather than silently carrying on with BRUV-only habitat
if (nrow(boss_benthos_raw) == 0) {
  stop("ga_api_habitat() returned no rows for the BOSS synthesis - check the ",
       "synthesis ID and that the BOSS campaigns have annotated habitat.")
}

saveRDS(boss_benthos_raw, paste0("data/", park, "/raw/boss_benthos_raw.RDS"))

# Replicates the CheckEM summarising applied to the BRUV synthesis. Verified
# against benthos_summarised.RDS - reproduces all six classes and
# total_points_annotated exactly.
# Note total_points_annotated is the sum of ALL annotated points, including
# level_2 == "Fishes", which is not assigned to any habitat class. Percentages
# therefore do not sum to 1 on samples with fish points. This matches CheckEM.
habitat_classes <- c("consolidated", "macroalgae", "sessile_invertebrates",
                     "seagrasses", "unconsolidated", "unscorable")

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
    dplyr::mutate(across(all_of(habitat_classes),
                         ~ .x / total_points_annotated,
                         .names = "{.col}_percent")) %>%
    dplyr::left_join(dplyr::distinct(meta, sample_url, campaignid, sample),
                     by = "sample_url") %>%
    dplyr::select(sample_url, campaignid, sample, total_points_annotated,
                  all_of(habitat_classes), dplyr::ends_with("_percent"))
}

boss_benthos_summarised <- summarise_benthos(boss_benthos_raw, boss_metadata)

# TODO Check the class totals look sensible for a downward-facing drop camera -
# expect more unconsolidated and less macroalgae than the forward-facing BRUVs
boss_benthos_summarised %>%
  dplyr::summarise(across(all_of(habitat_classes), sum)) %>%
  print()

saveRDS(boss_benthos_summarised, paste0("data/", park, "/raw/boss_benthos_summarised.RDS"))

# date_time can come back as an ISO 8601 character from some syntheses while the
# BRUV synthesis returns a datetime - bind_rows will error on the mismatch
if (is.character(boss_metadata$date_time)) {
  boss_metadata <- boss_metadata %>%
    mutate(date_time = lubridate::ymd_hms(date_time, tz = "UTC"))
}

# Columns that are entirely NA in one synthesis come back typed by whatever the
# API defaulted to, so the same column can be integer on one side and character
# on the other and bind_rows() errors out. Here it is observer_habitat_downward:
# NA integer for the forward-facing BRUVs, annotator URLs for the BOSS drops.
# Reconcile every shared column whose type differs rather than patching them one
# at a time, since which columns clash changes from synthesis to synthesis.
harmonise_types <- function(a, b) {
  shared <- intersect(names(a), names(b))
  for (col in shared) {
    ca <- class(a[[col]])[1]
    cb <- class(b[[col]])[1]
    if (identical(ca, cb)) next

    if (ca == "character" || cb == "character") {
      a[[col]] <- as.character(a[[col]])
      b[[col]] <- as.character(b[[col]])
      message("  ", col, ": ", ca, " / ", cb, " -> character")
    } else if (all(c(ca, cb) %in% c("integer", "numeric", "logical"))) {
      a[[col]] <- as.numeric(a[[col]])
      b[[col]] <- as.numeric(b[[col]])
      message("  ", col, ": ", ca, " / ", cb, " -> numeric")
    } else {
      stop("Cannot reconcile column '", col, "': ", ca, " vs ", cb,
           " - handle this one explicitly before the bind.")
    }
  }
  list(bruv = a, boss = b)
}

message("Reconciling column types before the bind:")
aligned       <- harmonise_types(metadata, boss_metadata)
metadata      <- aligned$bruv
boss_metadata <- aligned$boss

# Combine the BRUV and BOSS metadata
metadata <- bind_rows(metadata, boss_metadata)

# Guard against BOSS silently dropping out of the bind
if (!all(c("BRUV", "BOSS") %in% metadata$method)) {
  stop("Expected both BRUV and BOSS in metadata after the bind - check the join.")
}

# Combine the BRUV and BOSS benthos - bind_rows leaves NAs where a class was
# absent in one method; these are true zeros
benthos_summarised <- bind_rows(benthos_summarised, boss_benthos_summarised) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# Convert to factors now that any BOSS data has been combined in
metadata <- metadata %>%
  mutate(method = as.factor(method),
         year   = as.factor(year(date_time)),
         status = as.factor(status)) %>%
  glimpse()

# TODO Check status is populated for the BOSS samples - synthesis 76 returns
# clean No-Take/Fished for the BRUVs, but the BOSS synthesis may not.
# TODO Also check which years the BOSS campaigns land in - any year here that is
# not in config years will be carried in the benthos but not modelled by script 06
metadata %>%
  dplyr::count(method, year, status) %>%
  print(n = Inf)

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

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
# TODO Because all BOSS samples are retained, park_name will show any parks
# outside the South-west Corner that BOSS campaigns fall within - check whether
# those campaigns belong in this report
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
# Expected BRUV counts: 2020-06 = 39, 2020-10 = 272, 2023-03 = 32,
# 2024-10 = 188, 2025-04 = 143
print(sampling_summary, n = Inf)

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.RDS"))
write_csv(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.csv"))
