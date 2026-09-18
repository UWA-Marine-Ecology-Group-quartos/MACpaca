###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Tidy benthos and fish syntheses (written by 03_ and 04_)
# Task:    Re-fit the final hand-picked GAMs so Appendix C1 has model objects
# Author:  Annika Leunig
# Date:    September 2026
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
# Ningaloo pools benthos years (combine_benthos: true) - campaigns sample
# different spatial areas each year, so year is never offered as a candidate
# and none of the final habitat models below carry a year term.

metadata_bathy_derivatives <- readRDS(
  here("data", park, "tidy", paste0(name, "_metadata-bathymetry-derivatives.rds"))
) %>%
  clean_names()

habi <- readRDS(here("data", park, "tidy", paste0(name, "_benthos-count.RDS"))) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness)) %>%
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

# ---- FINAL MODELS (verbatim from 05_model-data_benthos.R) -------------------
# Macroalgae, rock and seagrasses never entered the FSS loop (>80% zeros) -
# only sand, sessile invertebrates and reef are modelled.

# Sand
m_sand <- gam(cbind(sand, total_pts - sand) ~
                s(geoscience_aspect, k = 5, bs = "cc")  +
                s(geoscience_depth, k = 5, bs = "cr") +
                s(geoscience_detrended, k = 5, bs = "cr") +
                s(geoscience_roughness, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Inverts
m_inverts <- gam(cbind(sessile_invertebrates, total_pts - sessile_invertebrates) ~
                   s(geoscience_aspect, k = 5, bs = "cc")  +
                   s(geoscience_depth, k = 5, bs = "cr") +
                   s(geoscience_detrended, k = 5, bs = "cr") +
                   s(geoscience_roughness, k = 5, bs = "cr"),
                 data = habi, method = "REML", family = binomial("logit"))

# Reef
m_reef <- gam(cbind(reef, total_pts - reef) ~
                s(geoscience_aspect, k = 5, bs = "cc")  +
                s(geoscience_depth, k = 5, bs = "cr") +
                s(geoscience_detrended, k = 5, bs = "cr") +
                s(geoscience_roughness, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Guard
if (combine_benthos &&
    any(vapply(list(m_sand, m_inverts, m_reef),
               function(m) "year" %in% all.vars(formula(m)), logical(1)))) {
  stop("combine_benthos is TRUE but a final habitat model still contains a year term.")
}

final_models_habitat <- list(
  sand                  = m_sand,
  sessile_invertebrates = m_inverts,
  reef                  = m_reef
)

saveRDS(final_models_habitat, file.path(outdir, paste0(name, "_final-models_habitat.rds")))
saveRDS(habi,                 file.path(outdir, paste0(name, "_habitat-data.rds")))

# =============================================================================
# 2. FISH - mirrors 06_model-data_fish.R
# =============================================================================
# Fish years are also pooled (see 06_model-data_fish.R) - only `status` is
# forced into every model via null.terms, there is no year term anywhere below.

tidy_maxn <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-count.rds"))) %>%
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

tidy_b20 <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-b20.rds"))) %>%
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

stopifnot(!any(is.na(fabund$status)))

# `reef` is a covariate in three of the four final models, so it has to be
# present in BOTH tidy syntheses before they are stacked - a missing column in
# tidy-b20 would silently become NA rows and drop the whole b20 fit.
stopifnot(all(c("reef") %in% names(tidy_maxn)))
stopifnot(all(c("reef") %in% names(tidy_b20)))

# ---- FINAL MODELS (verbatim from 06_model-data_fish.R) ----------------------

# Total abundance
m_abundance <- gam(count ~ status +
                     s(geoscience_depth, k = 3, bs = "cr") +
                     s(reef, k = 3, bs = "cr"),
                   data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                   family = poisson)

# Species richness
m_richness <- gam(count ~ status +
                    s(geoscience_aspect, k = 3, bs = "cc") +
                    s(geoscience_depth, k = 3, bs = "cr") +
                    s(reef, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = gaussian(link = "identity"))

# CTI
m_cti <- gam(count ~ status +
               s(geoscience_depth, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = gaussian(link = "identity"))

# B20
m_b20 <- gam(count ~ status +
               s(geoscience_aspect, k = 3, bs = "cc") +
               s(geoscience_depth, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "b20"),
             family = tw())

# Guard
if (any(vapply(list(m_abundance, m_richness, m_cti, m_b20),
               function(m) "year" %in% all.vars(formula(m)), logical(1)))) {
  stop("A final fish model contains a year term - fish years are pooled here.")
}
if (!all(vapply(list(m_abundance, m_richness, m_cti, m_b20),
                function(m) "status" %in% all.vars(formula(m)), logical(1)))) {
  stop("A final fish model is missing the forced status term.")
}

final_models_fish <- list(
  species_richness = m_richness,
  total_abundance  = m_abundance,
  b20              = m_b20,
  cti              = m_cti
)

saveRDS(final_models_fish, file.path(outdir, paste0(name, "_final-models_fish.rds")))
saveRDS(fabund,            file.path(outdir, paste0(name, "_fish-data.rds")))

message("Final models written to: ", outdir)
