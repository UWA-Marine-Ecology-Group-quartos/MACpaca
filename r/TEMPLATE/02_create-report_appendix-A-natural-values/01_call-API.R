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

# Load libraries needed -----

# TODO Run these once or as required:
# remotes::install_github("GlobalArchiveManual/CheckEM")
# CheckEM::ga_api_set_token()

library(tidyverse)
library(CheckEM)
options(timeout=600) # increase if more time needed for large data downloads

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Load the metadata, count and length ----
CheckEM::ga_api_all_data(synthesis_id = "47", # TODO change synthesis ID for different project
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE)

metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  # TODO Set the method for this synthesis. If you add a second synthesis later,
  # delete this line and set method inside the bind_rows() that combines them
  mutate(method = as.factor("BRUV"),
         year = as.factor(year(date_time)),
         status = as.factor(status)) %>%
  glimpse()

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# Tidy and join habitat with metadata
tidy_habitat <- benthos_summarised %>%
  left_join(metadata) %>%
  glimpse()

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

# Create the sampling effort summary table ----
library(sf)
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
# zone_abbrev below or the abbreviation will come back as NA
sample_zones %>%
  dplyr::count(method, zone) %>%
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
print(sampling_summary, n = Inf)

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.RDS"))
write_csv(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.csv"))
