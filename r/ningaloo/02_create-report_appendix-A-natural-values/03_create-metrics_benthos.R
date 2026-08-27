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

benthos <- readRDS(paste0("data/", park, "/raw/", name, "_benthos.RDS")) %>%
  dplyr::select(campaignid, sample, year, status, macroalgae, seagrasses,
                sand = unconsolidated, rock = consolidated,
                sessile_invertebrates, total_pts = total_points_annotated) %>%
  dplyr::mutate(reef = macroalgae + rock + sessile_invertebrates) %>%
  glimpse()

length(unique(benthos$sample))

saveRDS(benthos, paste0("data/", park, "/tidy/", name, "_benthos-count.RDS"))
