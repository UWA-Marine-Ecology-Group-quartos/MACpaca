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

# TODO Check which classes came back
message("Benthos columns: ", paste(names(benthos_raw), collapse = ", "))

benthos <- benthos_raw %>%
  dplyr::select(campaignid, sample, year, status, macroalgae, seagrasses,
                sand = unconsolidated, rock = consolidated,
                sessile_invertebrates,
                total_points_annotated) %>%
  dplyr::mutate(total_pts = macroalgae + seagrasses + sand + rock +
                  sessile_invertebrates) %>%
  glimpse()

# How much was excluded, and from where
message("Points annotated: ", sum(benthos$total_points_annotated),
        " | scored to a habitat class: ", sum(benthos$total_pts),
        " | excluded (unscorable + fishes): ",
        sum(benthos$total_points_annotated) - sum(benthos$total_pts))

benthos %>%
  dplyr::group_by(campaignid) %>%
  dplyr::summarise(n_samples = dplyr::n(),
                   annotated = sum(total_points_annotated),
                   scored    = sum(total_pts),
                   pct_excluded = round(100 * (1 - scored / annotated), 1),
                   .groups = "drop") %>%
  print(n = Inf)

# A sample with no scorable points cannot be modelled - cbind(x, 0 - x) is not a
# valid binomial response. Fail loudly rather than letting mgcv complain later.
if (any(benthos$total_pts == 0)) {
  message("Samples with zero scorable points (these are dropped):")
  benthos %>%
    dplyr::filter(total_pts == 0) %>%
    dplyr::select(campaignid, sample, total_points_annotated) %>%
    print(n = Inf)

  benthos <- benthos %>% dplyr::filter(total_pts > 0)
}

benthos <- benthos %>%
  dplyr::select(-total_points_annotated) %>%
  dplyr::mutate(reef = macroalgae + rock + sessile_invertebrates) %>%
  glimpse()

length(unique(benthos$sample))

saveRDS(benthos, paste0("data/", park, "/tidy/", name, "_benthos-count.RDS"))
