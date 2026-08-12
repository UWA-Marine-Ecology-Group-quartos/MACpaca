###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    2025 western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Format the 2025 lobster pot survey into tidy pot and measurement tables
# Author:  Henry Evans
# Date:    August 2026
###

rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(CheckEM)

source("r/abrolhos/functions/standardise_pot_number.R")

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
name <- config$name

campaign <- "2025-04_Abrolhos_lobster-pots"

raw_dir  <- "data/abrolhos/raw/lobster/2025"
tidy_dir <- "data/abrolhos/tidy/lobster"
dir.create(tidy_dir, recursive = TRUE, showWarnings = FALSE)

# Pot metadata -----------------------------------------------------------------

pots <- read.csv(file.path(raw_dir, "2025_Yamatji_Lobster.csv"),
                 fileEncoding = "UTF-8-BOM") %>%
  clean_names() %>%
  dplyr::mutate(time_set       = mdy_hms(date_time_set, tz = "UTC"),
                time_retrieved = mdy_hms(date_time_retrieved, tz = "UTC")) %>%
  dplyr::mutate(date_time_set_local       = with_tz(time_set, tzone = "Australia/Perth"),
                date_time_retrieved_local = with_tz(time_retrieved, tzone = "Australia/Perth")) %>%
  # Some pots have set and retrieved times entered the wrong way around
  dplyr::mutate(
    swapped = !is.na(date_time_set_local) & !is.na(date_time_retrieved_local) &
      date_time_retrieved_local < date_time_set_local,
    new_set       = if_else(swapped, date_time_retrieved_local, date_time_set_local),
    new_retrieved = if_else(swapped, date_time_set_local, date_time_retrieved_local)
  ) %>%
  dplyr::mutate(date_time_set_local       = new_set,
                date_time_retrieved_local = new_retrieved) %>%
  dplyr::mutate(pot_number = standardise_pot_number(pot_number),
                date_retrieved = date(date_time_retrieved_local)) %>%
  # Three pots retrieved just after midnight were logged against the previous day
  dplyr::mutate(date_retrieved = case_when(
    as.character(date_time_retrieved_local) %in% c("2025-04-09 09:32:18",
                                                   "2025-04-09 09:34:12",
                                                   "2025-04-09 09:35:43") ~ date("2025-04-10"),
    .default = date_retrieved)) %>%
  # Pots that fished but could not be counted reliably, flagged during fieldwork
  dplyr::mutate(successful = case_when(
    date_retrieved %in% date("2025-04-09") & pot_number %in% "4"  ~ FALSE,
    date_retrieved %in% date("2025-04-10") & pot_number %in% "16" ~ FALSE,
    date_retrieved %in% date("2025-04-09") & pot_number %in% "36" ~ FALSE,
    .default = TRUE)) %>%
  dplyr::mutate(campaign  = campaign,
                year      = 2025,
                soak_time_hours = as.numeric(difftime(date_time_retrieved_local,
                                                      date_time_set_local,
                                                      units = "hours"))) %>%
  dplyr::select(campaign, year, date_retrieved, pot_number,
                longitude = x, latitude = y, depth_m,
                soak_time_hours, successful) %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude)) %>%
  glimpse()

# Animal measurements ----------------------------------------------------------

# Day 3 uses different column headings to days 1 and 2, so each sheet is read
# separately before being standardised
read_day <- function(file, date_retrieved) {
  raw <- read_excel(file.path(raw_dir, file), skip = 1) %>%
    clean_names()

  if ("pot_lat_dd" %in% names(raw)) {
    raw <- dplyr::rename(raw, pot_lat = pot_lat_dd, pot_long = pot_long_dd)
  }

  raw %>%
    dplyr::rename(pot_number = pot_no) %>%
    dplyr::mutate(date_retrieved = date(date_retrieved),
                  pot_number = standardise_pot_number(pot_number),
                  # A handful of latitudes were entered without the negative sign
                  pot_lat = if_else(pot_lat > 0, pot_lat * -1, pot_lat)) %>%
    tidyr::fill(pot_lat, pot_long) %>%
    dplyr::select(date_retrieved, pot_number, pot_lat, pot_long,
                  clength, sex, colour, setose, tar_spot, egg_stage,
                  old_ant, old_legs, new_ant, new_legs, reg_ant, reg_legs,
                  dtagno1, rec)
}

measurements <- bind_rows(
  read_day("UWA_WRL_Day-1.xlsx", "2025-04-09"),
  read_day("UWA_WRL_Day-2.xlsx", "2025-04-10"),
  read_day("UWA_WRL_Day-3.xlsx", "2025-04-11")
) %>%
  dplyr::mutate(campaign = campaign,
                year     = 2025,
                species  = "Western Rock Lobster",
                carapace_length_mm = suppressWarnings(as.numeric(clength)),
                sex = case_when(sex %in% c("M", "Male")   ~ "Male",
                                sex %in% c("F", "Female") ~ "Female",
                                .default = "Unknown"),
                recapture  = if_else(!is.na(rec) & !rec %in% c("", "-"), "Yes", "No"),
                tag_number = as.character(dtagno1)) %>%
  dplyr::select(campaign, year, date_retrieved, pot_number, species,
                carapace_length_mm, sex, colour, setose, tar_spot, egg_stage,
                damage_old_ant = old_ant, damage_old_legs = old_legs,
                damage_new_ant = new_ant, damage_new_legs = new_legs,
                damage_reg_ant = reg_ant, damage_reg_legs = reg_legs,
                tag_number, recapture) %>%
  # 2025 records absent values as "-", 2026 leaves them blank
  dplyr::mutate(across(starts_with("damage_"),
                       ~ suppressWarnings(as.integer(na_if(trimws(as.character(.x)), "-")))),
                across(c(colour, setose, tar_spot, egg_stage, tag_number),
                       ~ na_if(na_if(trimws(as.character(.x)), "-"), ""))) %>%
  dplyr::filter(!is.na(carapace_length_mm)) %>%
  glimpse()

# Check every measured animal belongs to a known pot before writing
unmatched <- measurements %>%
  dplyr::anti_join(pots, by = c("date_retrieved", "pot_number"))

if (nrow(unmatched) > 0) {
  message("WARNING: ", nrow(unmatched),
          " measurements have no matching pot in the metadata:")
  print(dplyr::count(unmatched, date_retrieved, pot_number))
}

write.csv(pots, file.path(tidy_dir, paste0(name, "_lobster-pots_2025.csv")),
          row.names = FALSE)
write.csv(measurements, file.path(tidy_dir, paste0(name, "_lobster-measurements_2025.csv")),
          row.names = FALSE)

message("2025: ", nrow(pots), " pots, ", nrow(measurements), " measured lobsters")
