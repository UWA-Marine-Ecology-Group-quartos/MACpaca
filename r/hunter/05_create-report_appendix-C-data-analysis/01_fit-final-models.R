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

metadata_bathy_derivatives <- readRDS(
  here("data", park, "tidy", paste0(name, "_metadata-bathymetry-derivatives.rds"))
) %>%
  clean_names()

habi <- readRDS(here("data", park, "tidy", paste0(name, "_benthos-count.RDS"))) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::filter(geoscience_roughness < 10) %>%   # matches the outlier filter in 05
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

stopifnot(!any(is.na(habi$year)))

# ---- FINAL MODELS (verbatim from 05_model-data_benthos.R) -------------------

# Sand
m_sand <- gam(cbind(sand, total_pts - sand) ~
                s(geoscience_aspect, k = 3, bs = "cc") +
                s(geoscience_depth, k = 3, bs = "cr") +
                s(geoscience_detrended, k = 3, bs = "cr") +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Inverts
m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   s(geoscience_aspect, k = 3, bs = "cc") +
                   s(geoscience_depth, k = 3, bs = "cr") +
                   s(geoscience_roughness, k = 3, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))

# Reef
m_reef <- gam(cbind(reef, total_pts - reef) ~
                s(geoscience_aspect, k = 3, bs = "cc") +
                s(geoscience_depth, k = 3, bs = "cr") +
                s(geoscience_roughness, k = 3, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Names are the RESPONSE strings used by the FSS loop and the CSVs, not the
# object names - the loader keys off these. Note `seagrasses` is plural, as it
# is in 03_create-metrics_benthos.R.
final_models_habitat <- list(
  sand                  = m_sand,
  #macroalgae            = m_macro,
  #seagrasses            = m_seagrass,
  #rock                  = m_rock,
  sessile_invertebrates = m_inverts,
  reef                  = m_reef
)

saveRDS(final_models_habitat, file.path(outdir, paste0(name, "_final-models_habitat.rds")))
saveRDS(habi,                 file.path(outdir, paste0(name, "_habitat-data.rds")))

# =============================================================================
# 2. FISH - mirrors 06_model-data_fish.R
# =============================================================================

tidy_maxn <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-count.rds"))) %>%
  dplyr::filter(geoscience_roughness < 7) %>%   # matches the outlier filter in 06
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

tidy_b20 <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-b20.rds"))) %>%
  dplyr::filter(geoscience_roughness < 7) %>%   # matches the outlier filter in 06
  dplyr::mutate(year = factor(as.character(year), levels = year_levels)) %>%
  glimpse()

fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  dplyr::mutate(year   = factor(as.character(year), levels = year_levels),
                status = factor(as.character(status))) %>%
  glimpse()

stopifnot(!any(is.na(fabund$year)))
stopifnot(!any(is.na(fabund$status)))

# `reef` is a covariate in three of the four final models, so it has to be
# present in BOTH tidy syntheses before they are stacked - a missing column in
# tidy-b20 would silently become NA rows and drop the whole b20 fit.
stopifnot(all(c("reef") %in% names(tidy_maxn)))
stopifnot(all(c("reef") %in% names(tidy_b20)))

# ---- FINAL MODELS (verbatim from 06_model-data_fish.R) ----------------------

#Total abundance
m_abundance <- gam(count ~
                     s(geoscience_aspect, k = 3, bs = "cc") +
                     s(geoscience_depth, k = 3, bs = "cr"),
                   data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                   family = poisson)


# Species richness
m_richness <- gam(count ~
                    s(geoscience_aspect, k = 3, bs = "cc") +
                    s(geoscience_depth, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = nb)


# CTI
m_cti <- gam(count ~
               s(geoscience_roughness, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = gaussian(link = "identity"))


# B20
m_b20 <- gam(count ~
               s(geoscience_aspect, k = 3, bs = "cc") +
               s(geoscience_roughness, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "b20"),
             family = tw())

final_models_fish <- list(
  species_richness = m_richness,
  total_abundance  = m_abundance,
  b20              = m_b20,
  cti              = m_cti
)

saveRDS(final_models_fish, file.path(outdir, paste0(name, "_final-models_fish.rds")))
saveRDS(fabund,            file.path(outdir, paste0(name, "_fish-data.rds")))

message("Final models written to: ", outdir)
