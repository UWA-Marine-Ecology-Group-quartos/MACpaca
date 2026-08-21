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

name  <- config$name
park  <- config$park
years <- config$years

# Load libraries needed -----

# TODO Run these once or as required:
# remotes::install_github("GlobalArchiveManual/CheckEM")
# CheckEM::ga_api_set_token()

library(tidyverse)
library(CheckEM)
library(sf)
options(timeout=600) # increase if more time needed for large data downloads

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Load the metadata, count and length ----
CheckEM::ga_api_all_data(synthesis_id = "72", # TODO change synthesis ID for different project
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE)

# `method` distinguishes the two platforms downstream. BOSS is benthos only, so
# script 04 filters back to BRUV before building the fish metrics.
#
bruv_metadata_path <- paste0("data/", park, "/raw/bruv_metadata.RDS")

if (file.exists(bruv_metadata_path)) {
  metadata <- readRDS(bruv_metadata_path)
} else {
  metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
    mutate(method = "BRUV")
  saveRDS(metadata, bruv_metadata_path)
}

# ga_api_all_data() rewrites this each run, so it is always BRUV only here
benthos_summarised <- readRDS(paste0("data/", park, "/raw/benthos_summarised.RDS"))

# BOSS drop camera ----
# Synthesis 72 is BRUV only, so the BOSS benthos is pulled separately from
# synthesis 85. ga_api_all_data() cannot be used on a benthos-only synthesis -
# the empty count table returns sample_url as an integer and the internal join
# fails. Call the metadata and habitat endpoints directly instead.
boss_synthesis_id <- "85"

boss_metadata_all <- CheckEM::ga_api_metadata(token = token,
                                              synthesis_id = boss_synthesis_id) %>%
  mutate(method = "BOSS")

# Some syntheses return the coordinates as longitude/latitude rather than
# longitude_dd/latitude_dd. Left unpatched this silently drops every BOSS row at
# the is.finite(longitude_dd) filters in scripts 02 and 07.
if (!"longitude_dd" %in% names(boss_metadata_all)) {
  boss_metadata_all <- boss_metadata_all %>%
    dplyr::rename(longitude_dd = longitude, latitude_dd = latitude)
}

# Synthesis 85 may cover more than Geographe. Trim to the study extent so
# out-of-area campaigns cannot blow out the bathymetry crop in script 02.
# TODO Keep this identical to `e` in 02_spatial-layers.R
e_boss <- c(xmin = 115.04, xmax = 115.60, ymin = -33.67, ymax = -33.346)

boss_metadata <- boss_metadata_all %>%
  dplyr::filter(dplyr::between(longitude_dd, e_boss[["xmin"]], e_boss[["xmax"]]),
                dplyr::between(latitude_dd,  e_boss[["ymin"]], e_boss[["ymax"]]))

# TODO Check which campaigns were kept and dropped, and that this is what you expect
message("BOSS campaigns retained:")
print(dplyr::count(boss_metadata, campaignid))
message("BOSS campaigns dropped as out of area:")
print(dplyr::count(dplyr::anti_join(boss_metadata_all, boss_metadata,
                                    by = "sample_url"), campaignid))

if (nrow(boss_metadata) == 0) {
  stop("No BOSS samples fall inside the study extent - check e_boss and the ",
       "coordinate columns returned by synthesis ", boss_synthesis_id, ".")
}

# BOSS years are restricted to config$years - 2021 is dropped here, at ingest.
# Synthesis 85 holds a 2021 Geographe campaign with no matching BRUV survey and
# sparse coverage, so it is not carried through to the benthos models. `year` is
# a factor in every model in script 05, so keeping it would add a third level
# that is fitted but never predicted onto, and every downstream filename would
# become 2014-2021-2024 rather than the 2014-2024 that script 09 reads.
# To reinstate it, set boss_years <- unique(year(boss_metadata$date_time)).
boss_years <- as.character(years)

boss_metadata <- boss_metadata %>%
  dplyr::filter(as.character(lubridate::year(date_time)) %in% boss_years)

message("BOSS samples by year after the year filter:")
boss_metadata %>%
  dplyr::count(campaignid, year = lubridate::year(date_time)) %>%
  print()

if (nrow(boss_metadata) == 0) {
  stop("No BOSS samples remain after filtering to ",
       paste(boss_years, collapse = ", "), " - check boss_years.")
}

saveRDS(boss_metadata, paste0("data/", park, "/raw/boss_metadata.RDS"))

# ga_api_habitat() returns the RAW long-format benthos counts (one row per CATAMI
# class per sample), not the summarised wide table ga_api_all_data() saves. It
# has to be pivoted the same way before it will bind to benthos_summarised.
boss_benthos_raw <- CheckEM::ga_api_habitat(token = token,
                                            synthesis_id = boss_synthesis_id) %>%
  semi_join(boss_metadata, by = "sample_url")

# Fail loudly rather than silently carrying on with BRUV-only habitat
if (nrow(boss_benthos_raw) == 0) {
  stop("ga_api_habitat() returned no rows for synthesis ", boss_synthesis_id,
       " - check the ID and that the BOSS campaigns have annotated habitat.")
}

saveRDS(boss_benthos_raw, paste0("data/", park, "/raw/boss_benthos_raw.RDS"))

# Replicates the summarising CheckEM applies to the BRUV synthesis.
# Note total_points_annotated is the sum of ALL annotated points, including
# level_2 == "Fishes", which is not assigned to a habitat class - so percentages
# do not sum to 1 on samples with fish points. This matches CheckEM's behaviour.
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

# TODO Sanity check the class totals - a downward-facing drop camera should show
# noticeably more unconsolidated and less macroalgae than the forward BRUVs
boss_benthos_summarised %>%
  dplyr::summarise(across(all_of(habitat_classes), sum)) %>%
  print()

saveRDS(boss_benthos_summarised, paste0("data/", park, "/raw/boss_benthos_summarised.RDS"))

# Combine BRUV and BOSS ----
# date_time can come back as an ISO 8601 character from some syntheses while the
# BRUV synthesis returns a datetime - bind_rows errors on the mismatch
if (is.character(boss_metadata$date_time)) {
  boss_metadata <- boss_metadata %>%
    mutate(date_time = lubridate::ymd_hms(date_time, tz = "UTC"))
}

# The two syntheses can return the same column as different types. Columns that
# are entirely NA in one synthesis come back typed by whatever the API defaulted
# to rather than by their content - e.g. observer_length is character for the
# BRUVs and integer for the BOSS, because no BOSS sample has a length annotator.
# bind_rows refuses to combine those, so reconcile shared columns first.
harmonise_types <- function(a, b) {

  for (nm in intersect(names(a), names(b))) {

    if (identical(class(a[[nm]]), class(b[[nm]]))) next

    if (is.character(a[[nm]]) || is.character(b[[nm]])) {
      a[[nm]] <- as.character(a[[nm]])
      b[[nm]] <- as.character(b[[nm]])
    } else if (is.numeric(a[[nm]]) && is.numeric(b[[nm]])) {
      a[[nm]] <- as.numeric(a[[nm]])
      b[[nm]] <- as.numeric(b[[nm]])
    } else {
      warning("Type mismatch in '", nm, "' that could not be reconciled: ",
              class(a[[nm]])[1], " vs ", class(b[[nm]])[1])
      next
    }

    message("Reconciled type for column: ", nm)
  }

  list(a = a, b = b)
}

harmonised    <- harmonise_types(metadata, boss_metadata)
metadata      <- harmonised$a
boss_metadata <- harmonised$b

metadata <- bind_rows(metadata, boss_metadata) %>%
  dplyr::distinct(campaignid, sample, .keep_all = TRUE)

if (!all(c("BRUV", "BOSS") %in% metadata$method)) {
  stop("Expected both BRUV and BOSS in metadata after the bind - check the join.")
}

# bind_rows leaves NAs where a class was absent in one method; these are true zeros
benthos_summarised <- bind_rows(benthos_summarised, boss_benthos_summarised) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

# Convert to factors now that BOSS has been combined in
metadata <- metadata %>%
  mutate(method = as.factor(method),
         year   = as.factor(year(date_time)),
         status = as.factor(status)) %>%
  glimpse()

# TODO Check status is populated for the BOSS samples. Scripts 03, 05 and 07 all
# join on status, so an NA there will silently drop every BOSS row.
metadata %>%
  dplyr::count(method, year, status) %>%
  print(n = Inf)

if (any(is.na(metadata$status))) {
  warning(sum(is.na(metadata$status)), " samples have a missing status - ",
          "fill these before running 02 or they will be dropped downstream.")
}

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# Tidy and join habitat with metadata
tidy_habitat <- benthos_summarised %>%
  dplyr::select(-any_of("sample_url")) %>%
  left_join(metadata, by = c("campaignid", "sample")) %>%
  glimpse()

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

# Create the sampling effort summary table ----
sf_use_s2(FALSE)

count  <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS"))
length <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS"))

# Which marine park zone does each sample sit in? ----
# Geographe samples span both Commonwealth (Geographe AMP) and State (Ngari
# Capes) waters, so unlike the template this keeps both jurisdictions. Add
# `dplyr::filter(epbc %in% "Commonwealth")` back in if you only want AMP zones.
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>% # TODO select relevant parks
  st_transform(4326) %>%
  dplyr::select(park_name = name, epbc, zone) %>%
  st_make_valid()

# Samples that fall outside any marine park return NA from the join
sample_zones <- metadata %>%
  dplyr::distinct(campaignid, method, sample, longitude_dd, latitude_dd) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_join(marine_parks, join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  mutate(zone = replace_na(zone, "Coastal waters"))

# TODO Check these are the zones you expect - the labels must match the ones in
# zone_abbrev below or the abbreviation will come back as NA
sample_zones %>%
  dplyr::count(method, park_name, zone) %>%
  print(n = Inf)

# Shortened zone names to keep the table narrow enough for the PDF page
zone_abbrev <- c("National Park Zone"      = "NPZ",
                 "Habitat Protection Zone" = "HPZ",
                 "Multiple Use Zone"       = "MUZ",
                 "Special Purpose Zone"    = "SPZ",
                 "Recreational Use Zone"   = "RUZ",
                 "Recreation Area"         = "RA",
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
print(sampling_summary, n = Inf)

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.RDS"))
write_csv(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.csv"))

