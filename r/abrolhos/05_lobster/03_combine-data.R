###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Combine survey years, assign marine park zones and derive catch rates
# Author:  Henry Evans
# Date:    August 2026
###

rm(list = ls())

library(tidyverse)
library(sf)

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
name  <- config$name
years <- config$years
legal_size_mm <- config$legal_size_mm
large_male_mm <- config$large_male_mm

tidy_dir <- "data/abrolhos/tidy/lobster"

# Combine survey years ---------------------------------------------------------

# pot_number is forced to character because 2025 numbers are all numeric while
# 2026 includes extra pots labelled F3, F3-2 and FIII
read_tidy <- function(prefix, year) {
  read.csv(file.path(tidy_dir, paste0(name, "_lobster-", prefix, "_", year, ".csv")),
           colClasses = c(pot_number = "character")) %>%
    dplyr::mutate(date_retrieved = as.Date(date_retrieved))
}

pots <- years %>%
  purrr::map(~ read_tidy("pots", .x)) %>%
  bind_rows()

measurements <- years %>%
  purrr::map(~ read_tidy("measurements", .x)) %>%
  bind_rows()

# Attach strings ---------------------------------------------------------------

# Pots were set in strings that were repeated in the second year, so string is
# the repeated measures unit in the catch rate models. String was assigned by
# hand, so the lookup lives under data/abrolhos/manual/ rather than raw/ - it
# cannot be rebuilt from a Survey123 export and must survive one overwriting the
# raw folder. A string is one physical line of pots, so string 14 is kept whole
# even though it crosses a zone boundary - 13 of the 14 strings sit within a
# single zone, and string 14 is the only place zone varies within a string.
strings <- read.csv("data/abrolhos/manual/lobster/pot_strings.csv",
                    colClasses = c(pot_number = "character",
                                   string     = "character")) %>%
  dplyr::select(pot_key, string)

pots <- pots %>%
  dplyr::mutate(pot_key = paste(year, date_retrieved, pot_number, sep = "_")) %>%
  dplyr::left_join(strings, by = "pot_key") %>%
  dplyr::select(-pot_key)

unstringed <- dplyr::filter(pots, is.na(string) | string %in% "")
if (nrow(unstringed) > 0) {
  message("WARNING: ", nrow(unstringed), " pots have no string assigned:")
  print(dplyr::count(unstringed, year, date_retrieved, pot_number))
}

# Assign marine park zones -----------------------------------------------------

parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp",
                 quiet = TRUE) %>%
  dplyr::filter(name %in% "Abrolhos", epbc %in% "Commonwealth") %>%
  st_transform(4326) %>%
  dplyr::select(zone, zone_type)

# A left join keeps pots that fall outside the park rather than silently
# dropping them, as st_intersection would
pots_zoned <- pots %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_join(parks, join = st_intersects, left = TRUE) %>%
  st_drop_geometry() %>%
  dplyr::mutate(zone = replace_na(zone, "Outside marine park"))

outside <- dplyr::filter(pots_zoned, zone %in% "Outside marine park")
if (nrow(outside) > 0) {
  message(nrow(outside), " pots fall outside the Abrolhos marine park:")
  print(dplyr::count(outside, year, zone))
}

# Attach zone to each measured animal
measurements_zoned <- measurements %>%
  dplyr::left_join(dplyr::select(pots_zoned, year, date_retrieved, pot_number,
                                 zone, longitude, latitude, depth_m, successful),
                   by = c("year", "date_retrieved", "pot_number")) %>%
  dplyr::mutate(size_class = if_else(carapace_length_mm > legal_size_mm,
                                     "Legal", "Sublegal"))

unmatched <- dplyr::filter(measurements_zoned, is.na(zone))
if (nrow(unmatched) > 0) {
  message("WARNING: ", nrow(unmatched), " measurements could not be matched to a pot")
}

# Catch rates ------------------------------------------------------------------

# Pots that caught nothing must be carried through as zeros or catch rates will
# be biased high. Only pots that fished properly are used.
successful_pots <- dplyr::filter(pots_zoned, successful)

# Legal sized animals are also counted by sex, so males and females can be
# modelled separately. Sex is only counted within the legal size class because
# that is what the sex specific models use.
# The handful of animals recorded as sex "Unknown" are counted in n_legal but in
# neither sex column, so n_legal_male + n_legal_female <= n_legal by design.
legal_by_sex <- measurements_zoned %>%
  dplyr::filter(!is.na(carapace_length_mm),
                size_class %in% "Legal",
                sex %in% c("Male", "Female")) %>%
  dplyr::mutate(sex = tolower(sex)) %>%
  dplyr::count(year, date_retrieved, pot_number, sex, name = "count") %>%
  tidyr::pivot_wider(names_from = sex, values_from = count,
                     values_fill = 0, names_prefix = "n_legal_")

# A survey year could conceivably catch no legal animals of one sex, which would
# leave that column missing rather than zero
for (column in c("n_legal_male", "n_legal_female")) {
  if (!column %in% names(legal_by_sex)) legal_by_sex[[column]] <- 0L
}

# Large males are counted separately again. They are the part of the catch a
# fishery removes first, so they are the most likely place to see an effect of
# protection, and they are a subset of the legal males above.
large_males <- measurements_zoned %>%
  dplyr::filter(!is.na(carapace_length_mm),
                sex %in% "Male",
                carapace_length_mm >= large_male_mm) %>%
  dplyr::count(year, date_retrieved, pot_number, name = "n_male_large")

catch_per_pot <- measurements_zoned %>%
  dplyr::filter(!is.na(carapace_length_mm)) %>%
  dplyr::count(year, date_retrieved, pot_number, size_class, name = "count") %>%
  tidyr::pivot_wider(names_from = size_class, values_from = count,
                     values_fill = 0, names_prefix = "n_") %>%
  dplyr::full_join(legal_by_sex, by = c("year", "date_retrieved", "pot_number")) %>%
  dplyr::full_join(large_males, by = c("year", "date_retrieved", "pot_number")) %>%
  dplyr::right_join(successful_pots, by = c("year", "date_retrieved", "pot_number")) %>%
  dplyr::mutate(across(starts_with("n_"), ~ replace_na(.x, 0)),
                n_total = n_Legal + n_Sublegal) %>%
  dplyr::rename(n_legal = n_Legal, n_sublegal = n_Sublegal) %>%
  dplyr::select(campaign, year, date_retrieved, pot_number, string, zone,
                longitude, latitude, depth_m, soak_time_hours,
                n_legal, n_legal_male, n_legal_female, n_male_large,
                n_sublegal, n_total)

# The sexes must partition the legal catch, give or take the unknowns, and the
# large males must sit inside the legal males
stopifnot(all(catch_per_pot$n_legal_male + catch_per_pot$n_legal_female <=
                catch_per_pot$n_legal),
          large_male_mm >= legal_size_mm,
          all(catch_per_pot$n_male_large <= catch_per_pot$n_legal_male))

catch_by_zone <- catch_per_pot %>%
  dplyr::group_by(year, zone) %>%
  dplyr::summarise(n_pots = n(),
                   total_lobsters = sum(n_total),
                   mean_per_pot   = mean(n_total),
                   se_per_pot     = sd(n_total) / sqrt(n()),
                   mean_legal     = mean(n_legal),
                   se_legal       = sd(n_legal) / sqrt(n()),
                   mean_legal_male   = mean(n_legal_male),
                   se_legal_male     = sd(n_legal_male) / sqrt(n()),
                   mean_legal_female = mean(n_legal_female),
                   se_legal_female   = sd(n_legal_female) / sqrt(n()),
                   mean_male_large   = mean(n_male_large),
                   se_male_large     = sd(n_male_large) / sqrt(n()),
                   .groups = "drop")

write.csv(pots_zoned, file.path(tidy_dir, paste0(name, "_lobster-pots_all-years.csv")),
          row.names = FALSE)
write.csv(measurements_zoned, file.path(tidy_dir, paste0(name, "_lobster-measurements_all-years.csv")),
          row.names = FALSE)
write.csv(catch_per_pot, file.path(tidy_dir, paste0(name, "_lobster-catch-per-pot.csv")),
          row.names = FALSE)
write.csv(catch_by_zone, file.path(tidy_dir, paste0(name, "_lobster-catch-by-zone.csv")),
          row.names = FALSE)

message("\nPots by year and zone:")
print(dplyr::count(pots_zoned, year, zone))
message("\nCatch rates:")
print(as.data.frame(catch_by_zone))
