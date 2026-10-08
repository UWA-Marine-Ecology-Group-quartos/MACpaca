###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Western rock lobster pot synthesis, Abrolhos (Yamatji Shallow Bank)
# Task:    Call GlobalArchive API to download the lobster synthesis
# Author:
# Date:    October 2026
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

dir.create(paste0("data/", park, "/raw/"), recursive = TRUE, showWarnings = FALSE)

# Load the metadata and length ----
# ga_api_all_data() drops the stage column (sex), so the metadata and length
# endpoints are called directly instead
metadata <- CheckEM::ga_api_metadata(token = token,
                                     synthesis_id = "99") %>% # Lobster pots
  glimpse()

saveRDS(metadata, paste0("data/", park, "/raw/metadata.RDS"))

length <- CheckEM::ga_api_length(token = token,
                                 synthesis_id = "99") %>%
  glimpse()

saveRDS(length, paste0("data/", park, "/raw/length.RDS"))

# TODO check the years, status and stage are what you expect
metadata %>%
  count(campaignid, status) %>%
  print(n = Inf)

length %>%
  count(stage) %>%
  print(n = Inf)

