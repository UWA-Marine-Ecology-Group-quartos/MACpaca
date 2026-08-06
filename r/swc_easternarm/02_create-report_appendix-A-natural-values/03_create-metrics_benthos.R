###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Habitat data synthesis
# Task:    Combine and format benthos data for full subsets modelling
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
###

rm(list = ls())

library(tidyverse)

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

benthos_raw <- readRDS(paste0("data/", park, "/raw/", name, "_benthos.RDS"))

# Not every park returns all of these classes - add them as zero if missing
for (v in c("bioturbation", "unscorable", "worms")) {
  if (!v %in% names(benthos_raw)) benthos_raw[[v]] <- 0
}

benthos <- benthos_raw %>%
  dplyr::select(campaignid, sample, year, status, method, macroalgae, seagrasses,
                sand = unconsolidated, rock = consolidated,
                sessile_invertebrates, bioturbation, unscorable, worms,
                total_pts = total_points_annotated) %>%
  # Worms are scored as sessile invertebrates
  dplyr::mutate(sessile_invertebrates = sessile_invertebrates + worms) %>%
  # Unscorable and bioturbation points are not modelled, so they come out of the
  # denominator rather than counting as absences of every class
  dplyr::mutate(total_pts = total_pts - unscorable - bioturbation) %>%
  dplyr::select(-c(bioturbation, unscorable, worms)) %>%
  dplyr::mutate(reef = macroalgae + rock + sessile_invertebrates) %>%
  glimpse()

length(unique(benthos$sample))

# Should return zero rows - a sample with no scorable points left cannot be modelled
benthos %>%
  dplyr::filter(total_pts < 1)

# TODO Check annotation effort - BOSS annotates many more points per sample than
# BRUV, and total_pts sets the binomial weight of each sample in 05
benthos %>%
  dplyr::group_by(method, year) %>%
  dplyr::summarise(n = n(), min = min(total_pts), median = median(total_pts),
                   max = max(total_pts))

# Should now be zero for every method - the classes account for all points
benthos %>%
  dplyr::mutate(unclassified = total_pts - (macroalgae + seagrasses + sand + rock + sessile_invertebrates)) %>%
  dplyr::group_by(method) %>%
  dplyr::summarise(samples_affected = sum(unclassified != 0),
                   percent_of_points = sum(unclassified) / sum(total_pts) * 100)

saveRDS(benthos, paste0("data/", park, "/tidy/", name, "_benthos-count.RDS"))
