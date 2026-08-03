###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Folder:  create-appendix-C  (self-contained, does not modify 03-06)
# Task:    Re-fit the final hand-picked habitat GAMs so Appendix C has its
#          own model objects. Reads only the tidy output 03_create-metrics_
#          benthos.R already saved to disk - does not touch that script or
#          05_model-data_benthos.R.
# Author:  Claude Spencer & Henry Evans
###

rm(list = ls())

# 00_config.yml is duplicated into this folder too.
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

library(CheckEM)
library(tidyverse)
library(mgcv)

# ---- Re-build the modelling frame exactly as 05_model-data_benthos.R does ----
metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names()

habi <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::filter(geoscience_roughness < 4) %>% # TODO keep in sync with 05_model-data_benthos.R if that filter ever changes
  glimpse()

# ---- Final hand-picked models -------------------------------------------
# TODO these formulas are DUPLICATED from 05_model-data_benthos.R (not
# sourced from it, per your request not to touch that script). If you
# re-run FSS and change any of the chosen models there, update them here too.

m_sand <- gam(cbind(sand, total_pts - sand) ~
                year +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_depth, by = year, k = 5, bs = "cr") +
                s(geoscience_detrended, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

m_rock <- gam(cbind(rock, total_pts - rock) ~
                year +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_detrended, by = year, k = 5, bs = "cr") +
                s(geoscience_roughness, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 year +
                 s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                 s(geoscience_depth, by = year, k = 5, bs = "cr") +
                 s(geoscience_detrended, by = year, k = 5, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))

m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
                    year +
                    s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                    s(geoscience_depth, by = year, k = 5, bs = "cr") +
                    s(geoscience_detrended, by = year, k = 5, bs = "cr"),
                  data = habi, method = "REML", family = binomial("logit"))

m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   year +
                   s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                   s(geoscience_depth, by = year, k = 5, bs = "cr") +
                   s(geoscience_roughness, by = year, k = 5, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))

m_reef <- gam(cbind(reef, total_pts - reef) ~
                year +
                s(geoscience_aspect, by = year, k = 5, bs = "cc")  +
                s(geoscience_detrended, by = year, k = 5, bs = "cr") +
                s(geoscience_roughness, by = year, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

final_models_habitat <- list(
  macroalgae            = m_macro,
  sand                  = m_sand,
  seagrasses            = m_seagrass,
  rock                  = m_rock,
  sessile_invertebrates = m_inverts,
  reef                  = m_reef # kept for fish reef-predictor matching, dropped from the report table later
)

# ---- Save into a dedicated appendix-C output area, not the existing folders ----
outdir <- paste0("output/model-output/", park, "/appendix-C/habitat/")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

saveRDS(final_models_habitat, paste0(outdir, name, "_final-models.rds"))
saveRDS(habi,                 paste0(outdir, name, "_habitat-data.rds"))
