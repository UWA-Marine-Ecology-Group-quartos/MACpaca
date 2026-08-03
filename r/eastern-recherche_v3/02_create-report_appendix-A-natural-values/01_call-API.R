###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Marine Park monitoring data syntheses
# Task:    Call GlobalArchive API to download data syntheses
# Author:  Claude Spencer
# Date:    June 2024
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

# Load libraries needed -----

# TODO Run these once or as required:
# remotes::install_github("GlobalArchiveManual/CheckEM")
# CheckEM::ga_api_set_token()

library(tidyverse)
library(CheckEM)

options(timeout=600) # increase if more time needed for large data downloads

# Load the saved token
token <- readRDS("secrets/api_token.RDS")

# Load the metadata, count and length ----
CheckEM::ga_api_all_data(synthesis_id = "70", # AL changed
                         token = token,
                         dir = paste0("data/", park, "/raw/"), # Check the directory
                         include_zeros = TRUE)

metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  mutate(year = as.factor(year(date_time)),
         status = as.factor(status)) %>%
  glimpse()

metadata <- metadata %>%
  mutate(method = case_when(
    str_detect(campaignid, "BOSS") ~ "BOSS",
    str_detect(campaignid, "BRUV") ~ "stereo-BRUVs",
    TRUE ~ NA_character_
  ))

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# Tidy and join habitat with metadata
benthos_summarised <- readRDS(paste0("data/", park, "/raw/benthos_summarised.RDS"))
tidy_habitat <- benthos_summarised %>%
  left_join(metadata) %>%
  glimpse()


# Read and process BOSS habitat data (Salisbury & Investigator MBH) ----
# Source: CoralNet dot point export - not in GlobalArchive
boss_meta <- read_csv(paste0("data/", park, "/raw/Salisbury_Investigator_MBH_BOSS_habitat_Metadata.csv")) %>%
  rename_with(tolower) %>%
  select(sample, date, depth, location,
         longitude_dd = longitude, latitude_dd = latitude) %>%   # status dropped
  mutate(
    sample     = str_replace_all(sample, "_", "-"),
    date_time  = dmy(date),
    year       = as.factor(year(date_time)),
    status     = as.factor("Fished"),                            # <- set here
    campaignid = "2022-11_Salisbury-Investigator_BOSS"
  )

# CATAMI → habitat class lookup
catami_lookup <- tribble(
  ~catami_l2_l3,                                  ~habitat_class,
  "Substrate > Unconsolidated (soft)",             "unconsolidated",
  "Macroalgae > Large canopy-forming",             "macroalgae",
  "Macroalgae > Erect fine branching",             "macroalgae",
  "Macroalgae > Erect coarse branching",           "macroalgae",
  "Macroalgae > Encrusting",                       "macroalgae",
  "Macroalgae > Filamentous / filiform",           "macroalgae",
  "Macroalgae > Articulated calcareous",           "macroalgae",
  "Matrix > Bryozoa / Cnidaria / Sponge Matrix",  "sessile_invertebrates",
  "Matrix > Bryozoa / Cnidaria Matrix",            "sessile_invertebrates",
  "Sponges > Erect forms",                         "sessile_invertebrates",
  "Sponges > Massive forms",                       "sessile_invertebrates",
  "Sponges > Cup-likes",                           "sessile_invertebrates",
  "Sponges > Crusts",                              "sessile_invertebrates",
  "Cnidaria > Corals",                             "sessile_invertebrates",
  "Cnidaria > Hydroids",                           "sessile_invertebrates",
  "Bryozoa > Soft",                                "sessile_invertebrates",
  "Bryozoa > Hard",                                "sessile_invertebrates",
  "Ascidians > Stalked",                           "sessile_invertebrates",
  "Ascidians > Unstalked",                         "sessile_invertebrates",
  "Worms > Polychaetes",                           "sessile_invertebrates",
  "Worms",                                         "sessile_invertebrates",
  "Echinoderms > Sea stars",                       "sessile_invertebrates",
  "Seagrasses > Strap-like leaves",                "seagrasses",
  "Unscorable",                                    "unscorable",
  "Fishes > Bony fishes",                          "unscorable",
  "Bioturbation > Resting traces",                 "unscorable",
  "Molluscs > Cephalopods",                        "unscorable"
)

boss_benthos_summarised <- read_tsv(
  paste0("data/", park, "/raw/Salisbury_Investigator_MBH_BOSS_habitat_Dot Point Measurements.txt"),
  skip = 4
) %>%
  rename_with(tolower) %>%
  rename(catami_l2_l3 = `catami_l2_l3`) %>%
  mutate(sample = str_replace_all(str_remove(filename, "\\.jpg$"), "_", "-")) %>%
  filter(!is.na(catami_l2_l3)) %>%
  left_join(catami_lookup, by = "catami_l2_l3") %>%
  group_by(sample) %>%
  mutate(total_points_annotated = n()) %>%
  group_by(sample, total_points_annotated, habitat_class) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percent = count / total_points_annotated) %>%
  pivot_wider(
    names_from  = habitat_class,
    values_from = c(count, percent),
    values_fill = 0
  ) %>%
  rename_with(~ str_replace(., "^count_", ""), starts_with("count_")) %>%
  rename_with(~ paste0(str_remove(., "^percent_"), "_percent"), starts_with("percent_")) %>%
  # Ensure all expected columns exist even if a class has zero observations
  mutate(
    consolidated           = 0,
    consolidated_percent   = 0,
    across(c(macroalgae, sessile_invertebrates, unconsolidated,
             unscorable, seagrasses), ~ replace_na(., 0)),
    across(c(macroalgae_percent, sessile_invertebrates_percent, unconsolidated_percent,
             unscorable_percent, seagrasses_percent), ~ replace_na(., 0)),
    sample_url = NA_character_,
    campaignid = "2022-11_Salisbury-Investigator_BOSS"
  ) %>%
  # Fix INC- prefix mismatch (CoralNet used INC, metadata uses INV)
  mutate(sample = str_replace(sample, "^INC-", "INV-"))

# Join with metadata and filter to Salisbury only for ERMP
boss_tidy <- boss_benthos_summarised %>%
  left_join(boss_meta, by = c("sample", "campaignid")) %>%
  filter(location == "Salisbury MBH")

# Bind into tidy_habitat and resave ----
tidy_habitat <- bind_rows(tidy_habitat, boss_tidy)

saveRDS(tidy_habitat, paste0("data/", park, "/raw/", name, "_benthos.RDS"))

metadata <- bind_rows(
  metadata,
  boss_tidy %>%
    distinct(campaignid, sample, longitude_dd, latitude_dd, depth, status, year, date_time) %>%
    mutate(method = "BOSS")   # match whatever GA uses
) %>%
  distinct(campaignid, sample, .keep_all = TRUE)   # <- only new line

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

# deployments <- metadata %>%
#   distinct(campaignid, sample) %>%
#   bind_rows(boss_tidy %>% distinct(campaignid, sample)) %>%
#   count(campaignid, name = "deployments") %>%
#   arrange(campaignid) %>%
#   bind_rows(summarise(., campaignid = "TOTAL", deployments = sum(deployments)))
#
# deployments
