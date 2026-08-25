###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Tidy benthos and fish syntheses (written by 03_ and 04_)
# Task:    Re-fit the final hand-picked GAMs so Appendix C has model objects
# Author:  Annika Leunig
# Date:    August 2026
###
# =============================================================================


rm(list = ls())

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
# NOTE the roughness outlier filter is commented out in 05 for this park, so it
# is commented out here too. If you switch it back on in 05, switch it back on
# here or the refit will not be the model reported in Appendix A.

metadata_bathy_derivatives <- readRDS(
  here("data", park, "tidy", paste0(name, "_metadata-bathymetry-derivatives.rds"))
) %>%
  clean_names()

habi_raw <- readRDS(here("data", park, "tidy", paste0(name, "_benthos-count.RDS"))) %>%
  left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(geoscience_roughness))
# dplyr::filter(geoscience_roughness < 4)   # matches the outlier filter in 05

# `years` in 00_config.yml is the FISH year list - 06_model-data_fish.R expands
# it with map_dfr() to build the prediction frame, so a benthos-only year in
# there would produce NA fish rows. Benthos can therefore legitimately cover
# more years than the config lists (BOSS-only campaigns, for instance).
#
# That is harmless for the fit while combine_benthos is TRUE, because no habitat
# model contains `year` and gam() never drops those rows. It is NOT harmless for
# the report caption, which would otherwise claim the pooling covers fewer years
# than it does - so the true set is taken from the data and saved for the qmd.
benthos_years <- sort(unique(as.character(habi_raw$year)))
saveRDS(benthos_years, file.path(outdir, paste0(name, "_benthos-years.rds")))

unlisted_years <- setdiff(benthos_years, year_levels)
if (length(unlisted_years)) {
  message("Benthos years not in 00_config.yml `years`: ",
          paste(unlisted_years, collapse = ", "),
          " (config: ", paste(year_levels, collapse = ", "), ").\n",
          "  Expected when a campaign contributes benthos but no fish. Pooled ",
          "into the habitat fit regardless; reported in the figure caption.")
}

habi <- habi_raw %>%
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

# ---- FINAL MODELS (verbatim from 05_model-data_benthos.R) -------------------

# Sand
m_sand <- gam(cbind(sand, total_pts - sand) ~
                s(geoscience_aspect, k = 5, bs = "cc")  +
                s(geoscience_depth, k = 5, bs = "cr") +
                s(geoscience_detrended, k = 5, bs = "cr") +
                s(geoscience_roughness, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Rock
m_rock <- gam(cbind(rock, total_pts - rock) ~
                s(geoscience_aspect, k = 5, bs = "cc")  +
                s(geoscience_depth, k = 5, bs = "cr") +
                s(geoscience_detrended, k = 5, bs = "cr") +
                s(geoscience_roughness, k = 5, bs = "cr"),
              data = habi, method = "REML", family = binomial("logit"))

# Macroalgae - roughness dropped, dAICc 1.76 behind and no gain in r2
m_macro <- gam(cbind(macroalgae, total_pts - macroalgae) ~
                 s(geoscience_aspect, k = 5, bs = "cc")  +
                 s(geoscience_depth, k = 5, bs = "cr") +
                 s(geoscience_detrended, k = 5, bs = "cr"),
               data = habi, method = "REML", family = binomial("logit"))

# Seagrass
m_seagrass <- gam(cbind(seagrasses, total_pts - seagrasses) ~
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

# Same guard as the bottom of 05 - a stray year term here would never match an
# FSS candidate row, because the habitat FSS loop had no factor variables.
if (combine_benthos &&
    any(vapply(list(m_sand, m_rock, m_macro, m_seagrass, m_inverts, m_reef),
               function(m) "year" %in% all.vars(formula(m)), logical(1)))) {
  stop("combine_benthos is TRUE but a final habitat model still contains a year term.")
}

# Names are the RESPONSE strings used by the FSS loop and the CSVs, not the
# object names - the loader keys off these. Note `seagrasses` is plural, as it
# is in 03_create-metrics_benthos.R.
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

# =============================================================================
# 2. FISH - mirrors 06_model-data_fish.R
# =============================================================================
# The roughness filter is commented out in 06 as well - left commented here to
# match.

tidy_maxn <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-count.rds"))) %>%
  # dplyr::filter(geoscience_roughness < 4) %>%   # matches the outlier filter in 06
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

tidy_b20 <- readRDS(here("data", park, "tidy", paste0(name, "_tidy-b20.rds"))) %>%
  # dplyr::filter(geoscience_roughness < 4) %>%   # matches the outlier filter in 06
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  dplyr::mutate(year   = droplevels(factor(as.character(year), levels = year_levels)),
                status = droplevels(factor(as.character(status)))) %>%
  glimpse()

stopifnot(!any(is.na(fabund$year)))
stopifnot(!any(is.na(fabund$status)))

# `status` is a parametric term in two of the four final models, so it needs
# both levels or the contrast fails - same check 06 prints before the FSS loop.
stopifnot(nlevels(fabund$status) == 2)

# `reef` is a covariate in two of the four final models, so it has to be
# present in BOTH tidy syntheses before they are stacked - a missing column in
# tidy-b20 would silently become NA rows and drop the whole b20 fit.
stopifnot(all(c("reef") %in% names(tidy_maxn)))
stopifnot(all(c("reef") %in% names(tidy_b20)))

# ---- FINAL MODELS (verbatim from 06_model-data_fish.R) ----------------------

# Total abundance
m_abundance <- gam(count ~ year +
                     s(geoscience_aspect, k = 3, bs = "cc") +
                     s(geoscience_depth, k = 3, bs = "cr"),
                   data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                   family = poisson)

# Species richness - aspect dropped, dAICc 1.39 behind and r2 unchanged
m_richness <- gam(count ~ status +
                    s(reef, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = gaussian(link = "identity"))

# CTI - six models within delta AICc <= 2, this is the two-predictor one
m_cti <- gam(count ~ year +
               s(geoscience_depth, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = gaussian(link = "identity"))

# B20 - aspect dropped, dAICc 0.01 behind and r2 unchanged
m_b20 <- gam(count ~ year + status +
               s(reef, k = 3, bs = "cr"),
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
