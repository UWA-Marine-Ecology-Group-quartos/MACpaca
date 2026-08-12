###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    2026 western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Format the 2026 Survey123 export into tidy pot and measurement tables
# Author:  Henry Evans
# Date:    August 2026
###

rm(list = ls())

library(tidyverse)
library(lubridate)
library(CheckEM)

source("r/abrolhos/functions/standardise_pot_number.R")

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
name <- config$name

campaign <- "2026-04_Abrolhos_lobster-pots"

raw_dir  <- "data/abrolhos/raw/lobster/2026"
tidy_dir <- "data/abrolhos/tidy/lobster"
dir.create(tidy_dir, recursive = TRUE, showWarnings = FALSE)

# The Survey123 export holds every pot ever entered into the form, including
# 2024 trials in Geographe and Esperance, so it is filtered down to this survey
survey_extent <- list(xmin = 113.0, xmax = 114.0, ymin = -28.5, ymax = -27.5)

# Pot metadata -----------------------------------------------------------------

pots_raw <- read.csv(file.path(raw_dir, "pot_metadata_0.csv"),
                     fileEncoding = "UTF-8-BOM") %>%
  clean_names() %>%
  dplyr::mutate(time_retrieved = mdy_hms(date_time_retrieved, tz = "UTC"),
                date_time_retrieved_local = with_tz(time_retrieved, tzone = "Australia/Perth")) %>%
  dplyr::filter(!opcode %in% c("Test", " Test"), !pot_number %in% c("Test", " Test")) %>%
  dplyr::filter(year(date_time_retrieved_local) %in% 2026) %>%
  dplyr::filter(between(x, survey_extent$xmin, survey_extent$xmax),
                between(y, survey_extent$ymin, survey_extent$ymax))

# Three pots lost GPS fix on retrieval; the correct position was written into
# the comment field in the field app
pots <- pots_raw %>%
  dplyr::mutate(
    corrected_lat  = suppressWarnings(as.numeric(str_extract(comment, "-\\d{2}\\.\\d+"))),
    corrected_long = suppressWarnings(as.numeric(str_extract(comment, "11\\d\\.\\d+"))),
    latitude  = if_else(str_detect(comment, regex("location (lost|off)", ignore_case = TRUE)) &
                          !is.na(corrected_lat), corrected_lat, y),
    longitude = if_else(str_detect(comment, regex("location (lost|off)", ignore_case = TRUE)) &
                          !is.na(corrected_long), corrected_long, x)
  ) %>%
  dplyr::mutate(campaign = campaign,
                year     = 2026,
                date_retrieved = date(date_time_retrieved_local),
                pot_number = standardise_pot_number(pot_number),
                # Set times were not recorded in 2026, so soak time is unknown
                soak_time_hours = NA_real_,
                # Pots that were lost or fished with the door open cannot be counted
                successful = !str_detect(comment,
                                         regex("lost pot|trap door missing|submerged",
                                               ignore_case = TRUE))) %>%
  dplyr::filter(!is.na(longitude), !is.na(latitude), !is.na(pot_number)) %>%
  dplyr::select(campaign, year, date_retrieved, pot_number, pot_id,
                longitude, latitude, depth_m, soak_time_hours, successful) %>%
  glimpse()

# Animal measurements ----------------------------------------------------------

measurements <- read.csv(file.path(raw_dir, "count_and_length_1.csv"),
                         fileEncoding = "UTF-8-BOM") %>%
  clean_names() %>%
  dplyr::inner_join(dplyr::select(pots, pot_id, date_retrieved, pot_number),
                    by = "pot_id") %>%
  dplyr::mutate(campaign = campaign,
                year     = 2026,
                species  = if_else(str_detect(species, "Western Rock Lobster"),
                                   "Western Rock Lobster", species),
                carapace_length_mm = suppressWarnings(as.numeric(carapace_length_mm)),
                sex = case_when(sex %in% c("M", "Male")   ~ "Male",
                                sex %in% c("F", "Female") ~ "Female",
                                .default = "Unknown"),
                recapture  = if_else(recapture %in% c("Yes", "yes", "TRUE"), "Yes", "No"),
                tag_number = as.character(tag_number),
                setose     = setose_pleops) %>%
  dplyr::select(campaign, year, date_retrieved, pot_number, species,
                carapace_length_mm, sex, colour, setose, tar_spot, egg_stage,
                damage_old_ant, damage_old_legs, damage_new_ant, damage_new_legs,
                damage_reg_ant, damage_reg_legs, tag_number, recapture) %>%
  dplyr::mutate(across(starts_with("damage_"),
                       ~ suppressWarnings(as.integer(na_if(trimws(as.character(.x)), "-")))),
                across(c(colour, setose, tar_spot, egg_stage, tag_number),
                       ~ na_if(na_if(trimws(as.character(.x)), "-"), ""))) %>%
  glimpse()

# Non-lobster catch is kept separately rather than discarded
bycatch <- measurements %>%
  dplyr::filter(!species %in% "Western Rock Lobster")

lobsters <- measurements %>%
  dplyr::filter(species %in% "Western Rock Lobster", !is.na(carapace_length_mm))

unmatched <- lobsters %>%
  dplyr::anti_join(pots, by = c("date_retrieved", "pot_number"))

if (nrow(unmatched) > 0) {
  message("WARNING: ", nrow(unmatched),
          " measurements have no matching pot in the metadata:")
  print(dplyr::count(unmatched, date_retrieved, pot_number))
}

pots_out <- dplyr::select(pots, -pot_id)

write.csv(pots_out, file.path(tidy_dir, paste0(name, "_lobster-pots_2026.csv")),
          row.names = FALSE)
write.csv(lobsters, file.path(tidy_dir, paste0(name, "_lobster-measurements_2026.csv")),
          row.names = FALSE)
write.csv(bycatch, file.path(tidy_dir, paste0(name, "_lobster-bycatch_2026.csv")),
          row.names = FALSE)

message("2026: ", nrow(pots_out), " pots, ", nrow(lobsters), " measured lobsters, ",
        nrow(bycatch), " bycatch records")
