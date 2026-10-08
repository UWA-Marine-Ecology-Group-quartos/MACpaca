###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Western rock lobster pot synthesis, Abrolhos (Yamatji Shallow Bank)
# Task:    Format lobster lengths and create per pot metrics
# Author:
# Date:    October 2026
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
maturity_mm <- config$maturity_mm
large_mm    <- config$large_mm

library(tidyverse)

dir.create(paste0("data/", park, "/tidy/"), recursive = TRUE, showWarnings = FALSE)

# Metadata ----
# opcode is "<pot number>.<day>", so the pot number is everything before the dot
metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  dplyr::mutate(date       = as.Date(substr(date_time_local, 1, 10)),
                year       = as.character(year(date)),
                pot_number = sub("\\.[0-9]+$", "", opcode),
                status     = if_else(str_detect(tolower(status), "no"), "No-Take", "Fished"),
                # successful_count is blank on GlobalArchive, so blank is treated as successful
                successful = !successful_count %in% FALSE) %>%
  dplyr::filter(year %in% config$years) %>%
  dplyr::select(sample_url, campaignid, sample, pot_number, date, year, status,
                longitude_dd, latitude_dd, depth_m, successful) %>%
  glimpse()

# Strings ----
# Strings were assigned by hand - see data/abrolhos/manual/lobster/README.md
strings <- read.csv("data/abrolhos/manual/lobster/pot_strings.csv",
                    colClasses = c(pot_key = "character", string = "character")) %>%
  dplyr::select(pot_key, string)

metadata <- metadata %>%
  dplyr::mutate(pot_key = paste(year, date, pot_number, sep = "_")) %>%
  dplyr::left_join(strings, by = "pot_key") %>%
  dplyr::select(-pot_key)

# TODO check - pots without a string are dropped from the Appendix C models
metadata %>%
  dplyr::filter(is.na(string)) %>%
  print(n = Inf)

# Lengths ----
# One row per lobster. Stage F and M are sex, AD and J were not sexed
lobsters <- readRDS(paste0("data/", park, "/raw/length.RDS")) %>%
  dplyr::filter(!is.na(length_mm)) %>%
  dplyr::mutate(count = as.integer(count)) %>%
  tidyr::uncount(count) %>%
  dplyr::mutate(sex = case_when(stage %in% "F" ~ "Female",
                                stage %in% "M" ~ "Male",
                                .default = "Unknown")) %>%
  dplyr::select(sample_url, length_mm, sex) %>%
  dplyr::inner_join(metadata, by = "sample_url") %>%
  glimpse()

saveRDS(lobsters, paste0("data/", park, "/tidy/", name, "_lobster-lengths.rds"))

# Metrics per pot ----
pot_counts <- lobsters %>%
  dplyr::group_by(sample_url) %>%
  dplyr::summarise(all           = n(),
                   sub_maturity  = sum(length_mm <  maturity_mm),
                   mature_female = sum(length_mm >= maturity_mm & sex %in% "Female"),
                   mature_male   = sum(length_mm >= maturity_mm & sex %in% "Male"),
                   large_female  = sum(length_mm >= large_mm & sex %in% "Female"),
                   large_male    = sum(length_mm >= large_mm & sex %in% "Male"),
                   .groups = "drop")

# Only pots that fished properly, and pots that caught nothing are kept as zeros
tidy_metrics <- metadata %>%
  dplyr::filter(successful) %>%
  dplyr::left_join(pot_counts, by = "sample_url") %>%
  dplyr::mutate(across(all:large_male, ~ replace_na(.x, 0))) %>%
  tidyr::pivot_longer(all:large_male, names_to = "response", values_to = "count") %>%
  glimpse()

saveRDS(tidy_metrics, paste0("data/", park, "/tidy/", name, "_lobster-metrics.rds"))

# Sampling summary ----
sampling_summary <- metadata %>%
  dplyr::group_by(year, status) %>%
  dplyr::summarise(pots_deployed = n(),
                   pots_counted  = sum(successful),
                   .groups = "drop") %>%
  dplyr::left_join(dplyr::count(lobsters, year, status, name = "lobsters_measured"),
                   by = c("year", "status")) %>%
  dplyr::mutate(lobsters_measured = replace_na(lobsters_measured, 0)) %>%
  print()

saveRDS(sampling_summary, paste0("data/", park, "/tidy/", name, "_sampling-summary.rds"))
