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

# TODO Set the extent of the study
e <- c(114.05, 114.25, -29.35, -29.15) # xmin, xmax, ymin, ymax

# Load the metadata, count and length ----
CheckEM::ga_api_all_data(synthesis_id = "71", # BRUV - fish and benthos
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE)

# Format the metadata ----
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::mutate(method = "BRUV",
                year   = as.character(year(date_time)),
                status = as.character(status)) %>%
  dplyr::filter(longitude_dd >= e[1], longitude_dd <= e[2],
                latitude_dd  >= e[3], latitude_dd  <= e[4])

# Correct the status where GlobalArchive is wrong ----
apply_status_overrides <- function(df, overrides) {

  if (is.null(overrides) || length(overrides) == 0) {
    message("No status_overrides set in 00_config.yml - status left as GlobalArchive returned it")
    return(df)
  }

  for (ov in overrides) {

    if (is.null(ov$status)) stop("Every entry in status_overrides: needs a status:")

    rows <- rep(TRUE, nrow(df))
    if (!is.null(ov$method))     rows <- rows & df$method     %in% as.character(ov$method)
    if (!is.null(ov$year))       rows <- rows & df$year       %in% as.character(ov$year)
    if (!is.null(ov$campaignid)) rows <- rows & df$campaignid %in% as.character(ov$campaignid)

    if (!any(rows)) {
      warning("status_overrides entry matched no samples - check the spelling in 00_config.yml: ",
              paste(unlist(ov), collapse = " / "))
      next
    }

    message(sprintf("status override | method: %s | year: %s | %d samples | %s -> %s",
                    if (is.null(ov$method)) "any" else paste(ov$method, collapse = "/"),
                    if (is.null(ov$year))   "any" else paste(ov$year, collapse = "/"),
                    sum(rows),
                    paste(sort(unique(df$status[rows])), collapse = " + "),
                    ov$status))

    df$status[rows] <- ov$status
  }

  df
}

metadata <- apply_status_overrides(metadata, config$status_overrides) %>%
  dplyr::mutate(year   = as.factor(year),
                status = as.factor(status),
                method = as.factor(method)) %>%
  glimpse()

# TODO check the status is now what you expect.
metadata %>%
  count(method, year, status) %>%
  print(n = Inf)

metadata %>%
  count(method, year, campaignid) %>%
  print(n = Inf)

# TODO check nothing was lost to the extent filter - this should be 0
readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::filter(latitude_dd < -29) %>%
  dplyr::filter(!(longitude_dd >= e[1] & longitude_dd <= e[2] &
                    latitude_dd >= e[3] & latitude_dd <= e[4])) %>%
  nrow()

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# Tidy and join habitat with metadata ----
benthos_summarised <- readRDS(paste0("data/", park, "/raw/benthos_summarised.RDS"))
tidy_habitat <- benthos_summarised %>%
  inner_join(metadata, by = c("sample_url", "campaignid", "sample")) %>%
  glimpse()

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

# Trim the zero filled fish data to the study area ----
count <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  semi_join(metadata, by = c("campaignid", "sample")) %>%
  glimpse()

saveRDS(count, paste0("data/", park, "/raw/_count-with-zeros.RDS"))

length <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS")) %>%
  semi_join(metadata, by = c("campaignid", "sample")) %>%
  glimpse()

saveRDS(length, paste0("data/", park, "/raw/_length-with-zeros.RDS"))

# Create the sampling effort summary table ----
# One row per campaign x method, summarising when it was sampled, which AMP
# zones the samples fall in, how many samples were deployed, and which data
# types came back. Saved to data/{park}/tidy/ so 09_quarto.qmd can read it in
# directly.

library(sf)
sf_use_s2(FALSE)

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

# TODO note the park_name that comes back here - it is the value needed by the
# marine park filters in scripts 04, 05, 06, 07 and 08, which still say
# c("Ngari Capes", "Geographe", "South-west Corner")
sample_zones %>%
  dplyr::count(park_name) %>%
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
# Counts the samples in each campaign that have a row in each dataset
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

# NOTE Column names are left as tidy snake_case here - the report labels
# ("Campaign name", "No. of samples" etc.) are applied by the gt table at the
# top of 09_quarto.qmd, so nothing downstream needs backticks

# TODO Check the sample numbers match what you expect from the field
print(sampling_summary, n = Inf)

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.RDS"))
write_csv(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.csv"))
