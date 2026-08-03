###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Folder:  create-appendix-C  (self-contained, does not modify 03-06)
# Task:    Re-fit the final hand-picked fish GAMs so Appendix C has its own
#          model objects. Reads only the tidy RDS 03_create-metrics_benthos.R
#          (b20) and 04_create-metrics_fish.R (maxn) already saved to disk -
#          does not touch either script or 06_model-data_fish.R.
# Author:  Claude Spencer & Henry Evans
###

rm(list = ls())

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

library(tidyverse)
library(mgcv)

# ---- Load the already-saved tidy data - no re-derivation needed ----------
tidy_maxn <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-count.rds")) %>%
  dplyr::filter(geoscience_roughness < 4) # TODO keep in sync with 06_model-data_fish.R if that filter ever changes

tidy_b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-b20.rds")) %>%
  dplyr::filter(geoscience_roughness < 4) # TODO keep in sync with 06_model-data_fish.R if that filter ever changes

fabund <- bind_rows(tidy_maxn, tidy_b20) %>%
  glimpse()

# ---- Final hand-picked models -------------------------------------------
# TODO these formulas are DUPLICATED from 06_model-data_fish.R (not sourced
# from it, per your request not to touch that script). Keep in sync if the
# chosen models there change.

m_abundance <- gam(count ~ year + status +
                     s(reef, by = year, k = 3, bs = "cr"),
                   data = fabund %>% dplyr::filter(response %in% "total_abundance"),
                   family = poisson)

# TODO m_richness formula was truncated in the copy of 06_model-data_fish.R I
# reviewed - confirm this matches lines 217-222 of your real script exactly.
m_richness <- gam(count ~ year + status +
                    s(geoscience_depth, by = year, k = 3, bs = "cr") +
                    s(reef, by = year, k = 3, bs = "cr"),
                  data = fabund %>% dplyr::filter(response %in% "species_richness"),
                  family = gaussian(link = "identity"))

m_cti <- gam(count ~ year + status +
               s(geoscience_depth, by = year, k = 3, bs = "cr") +
               s(geoscience_detrended, by = year, k = 3, bs = "cr") +
               s(reef, by = year, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "cti"),
             family = gaussian(link = "identity"))

m_b20 <- gam(count ~ year + status +
               s(geoscience_depth, by = year, k = 3, bs = "cr") +
               s(geoscience_detrended, by = year, k = 3, bs = "cr"),
             data = fabund %>% dplyr::filter(response %in% "b20"),
             family = tw())

final_models_fish <- list(
  total_abundance  = m_abundance,
  species_richness = m_richness,
  cti              = m_cti,
  b20              = m_b20
)

# ---- Save into a dedicated appendix-C output area, not the existing folders ----
outdir <- paste0("output/model-output/", park, "/appendix-C/fish/")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

saveRDS(final_models_fish, paste0(outdir, name, "_final-models.rds"))
saveRDS(fabund,            paste0(outdir, name, "_fish-data.rds"))
