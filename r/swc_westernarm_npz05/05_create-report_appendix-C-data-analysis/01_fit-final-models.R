###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Tidy benthos synthesis (written by 03_)
# Task:    Re-fit the final hand-picked GAMs so Appendix C has model objects
# Author:  Annika Leunig
# Date:    August 2026
###

rm(list = ls())

# Locate folder
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

# Data
metadata_bathy_derivatives <- readRDS(
  here("data", park, "tidy", paste0(name, "_metadata-bathymetry-derivatives.rds"))
) %>%
  clean_names()

habi <- readRDS(here("data", park, "tidy", paste0(name, "_benthos-count.RDS"))) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  # dplyr::filter(geoscience_roughness < 4) %>%   # TODO mirror 05 if switched on
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

stopifnot(!any(is.na(habi$year)))

# Final models
# Sand
m_sand <- gam(cbind(sand, total_pts - sand) ~
                year +
                s(geoscience_depth, k = 3, bs = "cr", by = year) +
                s(geoscience_detrended, k = 3, bs = "cr", by = year) +
                s(geoscience_roughness, k = 3, bs = "cr", by = year),
              data = habi, method = "REML", family = binomial("logit"))

# Rock
m_rock <- gam(cbind(rock, total_pts - rock) ~
                year +
                s(geoscience_depth, k = 3, bs = "cr", by = year) +
                s(geoscience_detrended, k = 3, bs = "cr", by = year) +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Macroalgae
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 year +
                 s(geoscience_depth, k = 3, bs = "cr", by = year) +
                 s(geoscience_detrended, k = 3, bs = "cr", by = year) +
                 s(geoscience_roughness, k = 3, bs = "cr", by = year),
               data = habi, method = "REML", family = binomial("logit"))

# Seagrass
m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
                    year +
                    s(geoscience_aspect, k = 3, bs = "cc", by = year) +
                    s(geoscience_depth, k = 3, bs = "cr", by = year) +
                    s(geoscience_roughness, k = 3, bs = "cr", by = year),
                  data = habi, method = "REML", family = binomial("logit"))

# Inverts
m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   year +
                   s(geoscience_depth, k = 3, bs = "cr", by = year) +
                   s(geoscience_detrended, k = 3, bs = "cr", by = year) +
                   s(geoscience_roughness, k = 3, bs = "cr", by = year),
                 data = habi, method = "REML", family = binomial("logit"))

# Reef
m_reef <- gam(cbind(reef, total_pts - reef) ~
                year +
                s(geoscience_depth, k = 3, bs = "cr", by = year) +
                s(geoscience_detrended, k = 3, bs = "cr", by = year) +
                s(geoscience_roughness, k = 3, bs = "cr", by = year),
              data = habi, method = "REML", family = binomial("logit"))

# Guard
if (!combine_benthos &&
    !all(vapply(list(m_sand, m_rock, m_macro, m_seagrass, m_inverts, m_reef),
                function(m) "year" %in% all.vars(formula(m)), logical(1)))) {
  stop("combine_benthos is FALSE but a final habitat model has no year term.")
}

# Save
final_models_habitat <- list(
  sand                  = m_sand,
  macroalgae            = m_macro,
  seagrasses            = m_seagrass,
  rock                  = m_rock,
  sessile_invertebrates = m_inverts,
  reef                  = m_reef
)

saveRDS(final_models_habitat, file.path(outdir, paste0(name, "_final-models_habitat.rds")))
saveRDS(habi,                 file.path(outdir, paste0(name, "_habitat-data.rds")))

message("Final models written to: ", outdir)
