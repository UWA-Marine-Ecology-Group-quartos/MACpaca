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

# TODO Set the extent of the study - must match 02_spatial-layers.R
e <- c(120.7, 121.8, -34.6, -33.8) # xmin, xmax, ymin, ymax

# Load the metadata, count and length ----
# TODO change synthesis IDs for different projects
# file_prefix keeps the two syntheses in separate files so they don't overwrite
CheckEM::ga_api_all_data(synthesis_id = "69", # BRUV - fish and benthos
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE,
                         file_prefix = "bruv")

# Load the BOSS benthos ----
# ga_api_all_data() cannot be used on a benthos only synthesis - the empty count
# table returns sample_url as an integer and the internal join fails. Call the
# metadata and benthos endpoints directly instead.
boss_metadata <- CheckEM::ga_api_metadata(token = token,
                                          synthesis_id = "78") # BOSS

saveRDS(boss_metadata, paste0("data/", park, "/raw/boss_metadata.RDS"))

boss_samples <- boss_metadata %>%
  dplyr::select(sample_url, campaignid, sample)

# This repeats the benthos summarising step from inside ga_api_all_data()
boss_benthos_summarised <- CheckEM::ga_api_habitat(token = token,
                                                   synthesis_id = "78") %>%
  semi_join(boss_metadata, by = "sample_url") %>%
  dplyr::mutate(habitat = case_when(level_2 %in% "Macroalgae" ~ level_2,
                                    level_2 %in% "Seagrasses" ~ level_2,
                                    level_2 %in% "Substrate" & level_3 %in% "Consolidated (hard)" ~ "Consolidated",
                                    level_2 %in% "Substrate" & level_3 %in% "Unconsolidated (soft)" ~ "Unconsolidated",
                                    level_2 %in% "Sponges" ~ "Sessile invertebrates",
                                    level_2 %in% "Sessile invertebrates" ~ level_2,
                                    level_2 %in% "Bryozoa" ~ "Sessile invertebrates",
                                    level_2 %in% "Cnidaria" ~ "Sessile invertebrates",
                                    level_2 %in% "Echinoderms" ~ "Sessile invertebrates",
                                    level_2 %in% "Ascidians" ~ "Sessile invertebrates",
                                    .default = level_2)) %>%
  left_join(boss_samples, by = "sample_url") %>%
  dplyr::select(sample_url, campaignid, sample, habitat, count) %>%
  dplyr::group_by(sample_url, campaignid, sample, habitat) %>%
  dplyr::tally(count, name = "count") %>%
  dplyr::mutate(total_points_annotated = sum(count)) %>%
  dplyr::ungroup() %>%
  pivot_wider(names_from = "habitat", values_from = "count", values_fill = 0) %>%
  dplyr::select(-c(any_of("Fishes"))) %>%
  CheckEM::clean_names() %>%
  dplyr::mutate(across(.cols = 5:last_col(),
                       .fns = ~ .x / total_points_annotated,
                       .names = "{.col}_percent")) %>%
  glimpse()

saveRDS(boss_benthos_summarised, paste0("data/", park, "/raw/boss_benthos_summarised.RDS"))

# Combine the BRUV and BOSS metadata ----
# Only the columns used downstream are kept - the observer and successful
# columns come back with different types from each synthesis and will not bind
meta_cols <- c("sample_url", "campaignid", "sample", "opcode", "period",
               "date_time", "longitude_dd", "latitude_dd", "depth_m",
               "status", "site", "location")

metadata <- bind_rows(
  readRDS(paste0("data/", park, "/raw/bruv_metadata.RDS")) %>%
    dplyr::select(any_of(meta_cols)) %>% mutate(method = "BRUV"),
  readRDS(paste0("data/", park, "/raw/boss_metadata.RDS")) %>%
    dplyr::select(any_of(meta_cols)) %>% mutate(method = "BOSS")
) %>%
  # One BOSS sample has a typo in the date on GlobalArchive - 2002 not 2022
  # NOTE lubridate:: is explicit here - if you ever add years <- config$years to
  # this script it will mask lubridate::years() and this line will break
  mutate(date_time = if_else(year(date_time) %in% 2002,
                             date_time %m+% lubridate::years(20),
                             date_time)) %>%
  # Held as character until the status overrides below have been applied, then
  # converted to factors
  mutate(year   = as.character(year(date_time)),
         status = as.character(status),
         method = as.character(method)) %>%
  filter(longitude_dd >= e[1], longitude_dd <= e[2],
         latitude_dd  >= e[3], latitude_dd  <= e[4])

# Correct the status where GlobalArchive is wrong ----
# Some campaigns come back from GlobalArchive with a status that does not match
# the zoning the samples actually sit in. Rather than hard coding a fix into
# this script for every park, the corrections live under status_overrides: in
# 00_config.yml. Each override matches on any combination of method, year and
# campaignid, and sets status for every sample in that group.
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
  mutate(year   = as.factor(year),
         status = as.factor(status),
         method = as.factor(method)) %>%
  glimpse()

# TODO check the status is now what you expect. The levels must be spelt exactly
# "No-Take" and "Fished" - 05, 06 and 08 match on those strings
metadata %>%
  count(method, year, status) %>%
  print(n = Inf)

# TODO check the years in the study area, then set years: in 00_config.yml
metadata %>%
  count(method, year, campaignid) %>%
  print(n = Inf)

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# Combine the BRUV and BOSS benthos ----
# The two methods can return different habitat columns, so bind_rows leaves NAs
# where a class was absent - these are true zeros
benthos_summarised <- bind_rows(
  readRDS(paste0("data/", park, "/raw/bruv_benthos_summarised.RDS")) %>% mutate(method = "BRUV"),
  readRDS(paste0("data/", park, "/raw/boss_benthos_summarised.RDS")) %>% mutate(method = "BOSS")
) %>%
  dplyr::select(-ends_with("_percent")) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  mutate(across(-c(sample_url, campaignid, sample, total_points_annotated, method),
                ~ .x / total_points_annotated,
                .names = "{.col}_percent")) %>%
  glimpse()

# Tidy and join habitat with metadata
tidy_habitat <- benthos_summarised %>%
  inner_join(metadata, by = c("sample_url", "campaignid", "sample", "method")) %>%
  glimpse()

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

# Trim the zero filled fish data to the study area ----
# These come from the BRUV call only, which is what 04 and 08 expect
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
# directly with knitr::kable().

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
# Counts the samples in each campaign that have a row in each dataset. Fish
# count and length come from the BRUV synthesis only, so BOSS campaigns will
# return zero for both
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
# ("Campaign name", "No. of samples" etc.) are applied by the gt/kableExtra
# table at the top of 09_quarto.qmd, so nothing downstream needs backticks

# TODO Check the sample numbers match what you expect from the field
print(sampling_summary, n = Inf)

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.RDS"))
write_csv(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.csv"))
