###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Tidy benthos and fish syntheses (written by 03_ and 04_)
# Task:    Re-fit the final hand-picked GAMs so Appendix C has model objects
# Author:  Annika Leunig
# Date:    August 2026
###
# =============================================================================
# The model formulas below are copied VERBATIM from the "select best models"
# block at the bottom of 05_model-data_benthos.R and 06_model-data_fish.R. If
# those selections change, change them here too - 02_ matches each fitted model
# back to its FSS candidate row by predictor set, so a mismatch shows up as a
# "does not match any FSS candidate row" warning and an unbolded table row.
# =============================================================================

rm(list = ls())

# Locate this folder. Works in RStudio, in a plain R session, and when Quarto
# renders (Quarto sets the working directory to the document's own folder).
# rstudioapi::isAvailable() returns FALSE outside RStudio, whereas
# getActiveDocumentContext() calls verifyAvailable() and hard-errors.
if (!exists("appc_dir") || !file.exists(file.path(appc_dir, "00_config.yml"))) {
  appc_dir <- local({
    cands <- getwd()
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      p <- tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),
                    error = function(e) "")
      if (nzchar(p)) cands <- c(p, cands)
    }
    hit <- cands[file.exists(file.path(cands, "00_config.yml"))]
    if (!length(hit)) {
      stop("Could not find 00_config.yml.\n",
           "Set appc_dir <- \"<path to the appendix-C folder>\" before sourcing, ",
           "or setwd() to that folder.")
    }
    hit[1]
  })
}

config <- yaml::read_yaml(file.path(appc_dir, "00_config.yml"))

name            <- config$name
park            <- config$park
years           <- unlist(config$years)
combine_benthos <- config$combine_benthos

year_levels <- as.character(sort(years))

library(here)
library(tidyverse)
library(mgcv)
library(CheckEM)

outdir <- here("output", "model-output", park, "appendix-C")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. HABITAT - mirrors 05_model-data_benthos.R
# =============================================================================
# TODO If combine_benthos is TRUE for this park, drop the `year` filter below
# and follow the pooled-data pattern in 05_model-data_benthos.R instead.

metadata_bathy_derivatives <- readRDS(
  here("data", park, "tidy", paste0(name, "_metadata-bathymetry-derivatives.rds"))
) %>%
  clean_names()

habi <- readRDS(here("data", park, "tidy", paste0(name, "_benthos-count.RDS"))) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::filter(geoscience_roughness < 4) %>%   # TODO matches the outlier filter in 05 - comment out here too if it is commented out there
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

stopifnot(!any(is.na(habi$year)))

# ---- FINAL MODELS (verbatim from 05_model-data_benthos.R) -------------------
# TODO Paste the final hand-picked habitat GAM for each response below,
# copied verbatim from the bottom of 05_model-data_benthos.R. [TEMPLATE]

final_models_habitat <- list(
  sand                  = NULL, # [TEMPLATE]
  macroalgae            = NULL, # [TEMPLATE]
  seagrasses            = NULL, # [TEMPLATE]
  rock                  = NULL, # [TEMPLATE]
  sessile_invertebrates = NULL, # [TEMPLATE]
  reef                  = NULL  # [TEMPLATE]
)

saveRDS(final_models_habitat, file.path(outdir, paste0(name, "_final-models_habitat.rds")))
saveRDS(habi,                 file.path(outdir, paste0(name, "_habitat-data.rds")))

# =============================================================================
# 2. FISH - mirrors 06_model-data_fish.R
# =============================================================================

tidy_maxn <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-count.rds"))) %>%
  dplyr::filter(geoscience_roughness < 4) %>%   # TODO matches the outlier filter in 06 - comment out here too if it is commented out there
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

tidy_b20 <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-b20.rds"))) %>%
  dplyr::filter(geoscience_roughness < 4) %>%   # TODO matches the outlier filter in 06 - comment out here too if it is commented out there
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  dplyr::mutate(year   = factor(as.character(year), levels = year_levels),
                status = factor(as.character(status))) %>%
  glimpse()

stopifnot(!any(is.na(fabund$year)))
stopifnot(!any(is.na(fabund$status)))

# `reef` is a covariate in most final fish models, so it has to be present in
# BOTH tidy syntheses before they are stacked - a missing column in tidy-b20
# would silently become NA rows and drop the whole b20 fit.
stopifnot(all(c("reef") %in% names(tidy_maxn)))
stopifnot(all(c("reef") %in% names(tidy_b20)))

# ---- FINAL MODELS (verbatim from 06_model-data_fish.R) ----------------------
# TODO Paste the final hand-picked fish GAM for each metric below, copied
# verbatim from the bottom of 06_model-data_fish.R. [TEMPLATE]

final_models_fish <- list(
  species_richness = NULL, # [TEMPLATE]
  total_abundance  = NULL, # [TEMPLATE]
  b20              = NULL, # [TEMPLATE]
  cti              = NULL  # [TEMPLATE]
)

saveRDS(final_models_fish, file.path(outdir, paste0(name, "_final-models_fish.rds")))
saveRDS(fabund,            file.path(outdir, paste0(name, "_fish-data.rds")))

message("Final models written to: ", outdir)
