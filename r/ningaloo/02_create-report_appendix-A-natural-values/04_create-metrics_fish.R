###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Fish data synthesis
# Task:    Combine and format fish data for full subsets modelling
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
###

# Clear the environment
rm(list = ls())

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# Load necessary libraries
library(CheckEM)
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(vegan)
library(purrr)

metadata_bathy_derivatives <- readRDS(paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")) %>%
  clean_names() %>%
  glimpse()

metadata <- readRDS(paste0("data/", park, "/raw/metadata.RDS"))

# This is formatted habitat from 03_create-metrics_habitat
benthos <- readRDS(paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")) %>%
  CheckEM::clean_names() %>%
  dplyr::select(campaignid, sample, year, status, reef, total_pts) %>%
  dplyr::mutate(reef = reef/total_pts) %>% # Model reef as proportion for fish prediction
  glimpse()

count <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, count) %>%
  dplyr::mutate(scientific_name = paste(family, genus, species, sep = " ")) %>%
  glimpse()

spp_list <- count %>%
  dplyr::distinct(scientific_name, .keep_all = T) %>%
  dplyr::select(family, genus, species, scientific_name)

write.csv(spp_list, file = paste0("data/", park, "/tidy/", name, "_species_list.csv"))

ta.sr <- count %>%
  dplyr::select(-c(family, genus, species)) %>%
  pivot_wider(names_from = "scientific_name", values_from = count) %>%
  dplyr::mutate(
    total_abundance = rowSums(across(-c(campaignid, sample)), na.rm = TRUE),
    species_richness = rowSums(across(-c(campaignid, sample)) > 0)
  ) %>%
  dplyr::select(campaignid, sample, total_abundance, species_richness) %>%
  pivot_longer(cols = c("total_abundance", "species_richness"), names_to = "response", values_to = "count") %>%
  glimpse()

# -----------------------------
# Species Accumulation
# -----------------------------
# Prepare species matrix
# -----------------------------
count.wide <- count %>%
  dplyr::select(-c(family, genus, species)) %>%
  dplyr::group_by(campaignid, sample, scientific_name) %>%
  dplyr::summarise(
    count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = scientific_name,
    values_from = count,
    values_fill = 0
  ) %>%
  mutate(
    Year = case_when(
      grepl("^2014", campaignid) ~ "2014", # TODO adjust to years in data
      grepl("^2024", campaignid) ~ "2024",
      TRUE ~ campaignid
    )
  ) %>%
  left_join(
    metadata %>% dplyr::select(sample, status),
    by = "sample"
  ) %>%
  dplyr::filter(!is.na(status))

# -----------------------------
# Function to generate SAC data
# -----------------------------

make_sac_df <- function(df, year_name, status_name) {

  species_mat <- df %>%
    dplyr::select(-campaignid, -sample, -Year, -status)

  species_pa <- vegan::decostand(species_mat, method = "pa")

  sac_random_pa <- vegan::specaccum(
    species_pa,
    method = "random",
    permutations = 999
  )

  df_random_pa <- data.frame(
    x = sac_random_pa$sites,
    richness = sac_random_pa$richness,
    sd = sac_random_pa$sd,
    curve = "Sample-based detection/non-detection",
    Year = year_name,
    status = status_name
  )

  sac_rare <- vegan::specaccum(
    species_mat,
    method = "rarefaction"
  )

  df_rare <- data.frame(
    x = sac_rare$individuals,
    richness = sac_rare$richness,
    sd = sac_rare$sd,
    curve = "Individual-based rarefaction",
    Year = year_name,
    status = status_name
  )

  bind_rows(df_random_pa, df_rare)
}

# -----------------------------
# Generate SACs by Year and status
# -----------------------------

sac_df <- count.wide %>%
  dplyr::group_split(Year, status) %>%
  purrr::map_dfr(function(x) {
    make_sac_df(
      x,
      unique(x$Year),
      unique(x$status)
    )
  })

saveRDS(sac_df, file = paste0("data/", park, "/tidy/", name, "_species-accumulation.rds"))

cti <- CheckEM::create_cti(data = count) %>%
  dplyr::rename(count = cti) %>%
  dplyr::mutate(response = "cti") %>%
  glimpse()

tidy_maxn <- bind_rows(ta.sr, cti) %>% # TODO check which samples are removed in this chunk
  dplyr::select(-c(log_count, w_sti)) %>%
  dplyr::left_join(metadata) %>% # To join samples without valid bathymetry derivatives
  dplyr::left_join(benthos) %>%
  dplyr::left_join(metadata_bathy_derivatives) %>%
  dplyr::filter(!is.na(reef),
                !is.na(geoscience_aspect)) %>% # Not valid values for modelling so will remove them now
  glimpse()

saveRDS(tidy_maxn, file = paste0("data/", park, "/tidy/", name, "_tidy-count.rds"))

# Create df for calculating B20
b20_length <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, count) %>%
  mutate(length_cm = length_mm / 10) %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " ")) %>%
  glimpse()

# 1) Calculate mass from lengths
biomass <- b20_length %>%
  mutate(
    adj_length = case_when(
      count == 0 ~ NA_real_,  # length irrelevant for absences
      fb_length_weight_measure %in% "FL" ~ length_cm,
      fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "FL → TL" ~ (length_cm * fb_b_ll) + fb_a_ll,
      fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "TL → FL" ~ (length_cm - fb_a_ll) / fb_b_ll,
      TRUE ~ NA_real_
    ),
    mass_g = case_when(
      count == 0 ~ 0,  # absences are zero biomass
      !is.na(adj_length) & !is.na(fb_a) & !is.na(fb_b) ~ (adj_length ^ fb_b) * fb_a * count,
      TRUE ~ NA_real_  # present but cannot compute biomass -> NA
    )
  ) %>%
  left_join(metadata, by = c("campaignid","sample")) %>%
  left_join(metadata_bathy_derivatives,
            by = c("campaignid","sample","longitude_dd","latitude_dd","status","year"))

# 2) Define inclusion + b20-specific biomass
b20_mass <- biomass %>%
  mutate(
    include_b20 = class == "Actinopterygii" &
      (count == 0 | (length_cm >= 20 & length_cm <= 800)),

    b20_mass_g = case_when(
      count == 0 ~ 0,                        # absence stays zero
      !include_b20 ~ 0,                      # excluded taxa/sizes contribute 0 to B20
      include_b20 & !is.na(mass_g) ~ mass_g, # included + computable biomass
      include_b20 & is.na(mass_g) ~ NA_real_ # included but missing -> NA (flag)
    )
  )

b20_mass_check <- b20_mass %>%
  select(sample, year, scientific_name, b20_mass_g, length_cm)

sp_watercol <- b20_mass %>%
  distinct(scientific_name, rls_water_column)

# 3) Per sample × species B20 biomass
b20_by_sample <- b20_mass %>%
  group_by(year, sample, scientific_name) %>%
  summarise(
    present_n = sum(count, na.rm = TRUE),

    # If species never present on that BRUV => 0
    b20_sample = if (sum(count, na.rm = TRUE) == 0) 0
    # If present, but every INCLUDED present record has NA biomass => NA
    else if (all(is.na(b20_mass_g[count > 0 & include_b20]))) NA_real_
    # Otherwise sum included biomass (excluded rows are already 0)
    else sum(b20_mass_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sp_watercol, by = "scientific_name")


# 4) Ensure every BRUV × species exists (zeros for absences)
all_samples <- metadata %>%
  distinct(year, sample)

b20_by_sample_complete <- b20_by_sample %>%
  right_join(all_samples, by = c("year","sample")) %>%
  tidyr::complete(nesting(year, sample), scientific_name,
                  fill = list(b20_sample = 0, present_n = 0)) %>%
  left_join(metadata %>% select(year, sample, status), by = c("year", "sample"))

# 5) Species summaries per year
b20_species_by_status <- b20_by_sample_complete %>%
  group_by(year, scientific_name, status) %>%
  summarise(
    b20 = mean(b20_sample, na.rm = TRUE),
    sd  = sd(b20_sample, na.rm = TRUE),
    n   = sum(!is.na(b20_sample)),
    se  = sd / sqrt(n),
    .groups = "drop"
  )

b20_species_combined <- b20_by_sample_complete %>%
  group_by(year, scientific_name) %>%
  summarise(
    b20 = mean(b20_sample, na.rm = TRUE),
    sd  = sd(b20_sample, na.rm = TRUE),
    n   = sum(!is.na(b20_sample)),
    se  = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(status = "Combined")

b20_species <- bind_rows(b20_species_by_status, b20_species_combined) %>%
  left_join(sp_watercol, by = "scientific_name")

saveRDS(b20_species, file = paste0("data/", park, "/tidy/", name, "_b20-species.rds"))


# -------------------------------------------------------------------------
# Commonwealth-only copy of metadata
# -------------------------------------------------------------------------

marine_parks_amp <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

metadata_amp <- metadata %>%
  distinct(campaignid, sample, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_join(
    marine_parks_amp %>% dplyr::select(name, epbc),
    join = st_within,
    left = FALSE
  ) %>%
  st_drop_geometry()

# optional quick check
metadata_amp %>%
  count(year, status)

# -------------------------------------------------------------------------
# Create df for calculating B20 (Commonwealth only)
# -------------------------------------------------------------------------

b20_length_amp <- readRDS(paste0("data/", park, "/raw/_length-with-zeros.RDS")) %>%
  dplyr::select(campaignid, sample, family, genus, species, length_mm, count) %>%
  mutate(length_cm = length_mm / 10) %>%
  left_join(CheckEM::australia_life_history) %>%
  dplyr::mutate(scientific_name = paste(genus, species, sep = " "))

# 1) Calculate mass from lengths
biomass_amp <- b20_length_amp %>%
  inner_join(metadata_amp, by = c("campaignid", "sample")) %>%
  left_join(
    metadata_bathy_derivatives,
    by = c("campaignid", "sample", "longitude_dd", "latitude_dd", "status", "year")
  ) %>%
  mutate(
    adj_length = case_when(
      count == 0 ~ NA_real_,  # length irrelevant for absences
      fb_length_weight_measure %in% "FL" ~ length_cm,
      fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "FL → TL" ~ (length_cm * fb_b_ll) + fb_a_ll,
      fb_length_weight_measure %in% "TL" & fb_ll_equation_type %in% "TL → FL" ~ (length_cm - fb_a_ll) / fb_b_ll,
      TRUE ~ NA_real_
    ),
    mass_g = case_when(
      count == 0 ~ 0,  # absences are zero biomass
      !is.na(adj_length) & !is.na(fb_a) & !is.na(fb_b) ~ (adj_length ^ fb_b) * fb_a * count,
      TRUE ~ NA_real_  # present but cannot compute biomass -> NA
    )
  )

# 2) Define inclusion + b20-specific biomass
b20_mass_amp <- biomass_amp %>%
  mutate(
    include_b20 = class == "Actinopterygii" &
      (count == 0 | (length_cm >= 20 & length_cm <= 800)),

    b20_mass_g = case_when(
      count == 0 ~ 0,                        # absence stays zero
      !include_b20 ~ 0,                      # excluded taxa/sizes contribute 0 to B20
      include_b20 & !is.na(mass_g) ~ mass_g, # included + computable biomass
      include_b20 & is.na(mass_g) ~ NA_real_ # included but missing -> NA (flag)
    )
  )

b20_mass_check_amp <- b20_mass_amp %>%
  select(sample, year, scientific_name, b20_mass_g, length_cm)

sp_watercol_amp <- b20_mass_amp %>%
  distinct(scientific_name, rls_water_column)

# 3) Per sample × species B20 biomass
b20_by_sample_amp <- b20_mass_amp %>%
  group_by(year, sample, scientific_name) %>%
  summarise(
    present_n = sum(count, na.rm = TRUE),

    # If species never present on that BRUV => 0
    b20_sample = if (sum(count, na.rm = TRUE) == 0) 0
    # If present, but every INCLUDED present record has NA biomass => NA
    else if (all(is.na(b20_mass_g[count > 0 & include_b20]))) NA_real_
    # Otherwise sum included biomass (excluded rows are already 0)
    else sum(b20_mass_g, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(sp_watercol_amp, by = "scientific_name")

# 4) Ensure every BRUV × species exists (zeros for absences)
all_samples_amp <- metadata_amp %>%
  distinct(year, sample)

b20_by_sample_complete_amp <- b20_by_sample_amp %>%
  right_join(all_samples_amp, by = c("year","sample")) %>%
  tidyr::complete(
    nesting(year, sample), scientific_name,
    fill = list(b20_sample = 0, present_n = 0)
  ) %>%
  left_join(
    metadata_amp %>% distinct(year, sample, status),
    by = c("year", "sample")
  )

# 5) Species summaries per year
b20_species_by_status_amp <- b20_by_sample_complete_amp %>%
  group_by(year, scientific_name, status) %>%
  summarise(
    b20 = mean(b20_sample, na.rm = TRUE),
    sd  = sd(b20_sample, na.rm = TRUE),
    n   = sum(!is.na(b20_sample)),
    se  = sd / sqrt(n),
    .groups = "drop"
  )

b20_species_combined_amp <- b20_by_sample_complete_amp %>%
  group_by(year, scientific_name) %>%
  summarise(
    b20 = mean(b20_sample, na.rm = TRUE),
    sd  = sd(b20_sample, na.rm = TRUE),
    n   = sum(!is.na(b20_sample)),
    se  = sd / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(status = "Combined")

b20_species_amp <- bind_rows(b20_species_by_status_amp, b20_species_combined_amp) %>%
  left_join(sp_watercol_amp, by = "scientific_name")

saveRDS(
  b20_species_amp,
  file = paste0("data/", park, "/tidy/", name, "_b20-species_amp.rds")
)

## The below is to work out which species are missing fishbase data
# message(paste(length(which(!is.na(biomass$length_cm))), "measured lengths in data"))
# message(paste(length(which(!is.na(biomass$adj_length))), "adjusted lengths in data"))
# message(paste(length(which(!is.na(biomass$length_cm))) - length(which(!is.na(biomass$adj_length))),
#               "measured lengths not converted to adjusted (missing)"))
#
# message(paste(length(which(!is.na(biomass$length_cm) &
#                              is.na(biomass$fb_length_weight_measure))), "because fb_length_weight_measure is NA"))
# message(paste(length(which(!is.na(biomass$length_cm) &
#                              is.na(biomass$fb_ll_equation_type) &
#                              biomass$fb_length_weight_measure == "TL")),
#               "because fb_length_weight_measure = TL (good) but fb_ll_equation_type is missing"))
# message(paste(length(which(biomass$fb_length_weight_measure == "SL" & !is.na(biomass$length_cm))),
#               "because fb_length_weight_measure is SL (not FL or TL)"))
#
# message(paste("These 3x reasons added =", length(which(!is.na(biomass$length_cm) &
#                                                          is.na(biomass$fb_length_weight_measure))) +
#                 length(which(!is.na(biomass$length_cm) &
#                                is.na(biomass$fb_ll_equation_type) &
#                                biomass$fb_length_weight_measure == "TL")) +
#                 length(which(biomass$fb_length_weight_measure == "SL" & !is.na(biomass$length_cm))),
#               "accounting for all missing adjusted lengths"))
#
# missing_info <- biomass %>%
#   dplyr::filter(class %in% "Actinopterygii") %>%
#   dplyr::filter(length_cm >= 20) %>%
#   filter(is.na(adj_length)) %>%
#   distinct(scientific_name, australian_common_name, .keep_all = TRUE) %>%
#   select(family, genus, species, australian_common_name, fb_length_weight_measure,
#          fb_a, fb_b, fb_ll_equation_type)
# write.csv(missing_info, file = paste0("data/", park, "/tidy/", name, "_b20_missing_info.csv"))

b20_metadata <- biomass %>%
  distinct(year, sample) %>%
  glimpse()

# Calculate B20* for each sample
b20_tidy <- biomass %>% # TODO this needs tweaking, not working 100% because some lengths have NA mass (fix in biomass)
  mutate(
    include_b20 = class == "Actinopterygii" &
      # B20 size rule, but keep absences
      (count == 0 | (length_cm >= 20 & length_cm <= 800)),

    b20_mass_g = case_when(
      count == 0 ~ 0,                        # absences
      !include_b20 ~ 0,                      # excluded rows contribute zero to B20
      include_b20 & !is.na(mass_g) ~ mass_g, # included + computable
      include_b20 & is.na(mass_g) ~ NA_real_ # included but missing -> NA (flag)
    )
  ) %>%
  group_by(year, sample) %>%
  summarise(
    # if you want a diagnostic:
    n_present = sum(count > 0, na.rm = TRUE),
    n_present_included = sum(count > 0 & include_b20, na.rm = TRUE),
    n_missing_mass_included = sum(count > 0 & include_b20 & is.na(b20_mass_g), na.rm = TRUE),

    # sample-level B20 biomass
    count = if (sum(count, na.rm = TRUE) == 0) 0
    else if (all(is.na(b20_mass_g[count > 0 & include_b20]))) NA_real_
    else sum(b20_mass_g, na.rm = TRUE),

    .groups = "drop"
  ) %>%
  right_join(b20_metadata, by = c("year","sample")) %>%   # keep all BRUVs
  mutate(
    count = ifelse(is.na(count), 0, count),
    response = "b20"
  ) %>%
  left_join(metadata, by = c("year","sample")) %>%
  left_join(metadata_bathy_derivatives,
            by = c("campaignid","sample","longitude_dd","latitude_dd","status","year")) %>%
  left_join(benthos, by = c("campaignid","sample","status","year")) %>%
  filter(!is.na(reef),
         !is.na(geoscience_aspect)) %>%
  glimpse()

# Check number of samples that are > 0
nrow(filter(b20_tidy, count > 0))/nrow(b20_tidy)

saveRDS(b20_tidy, file = paste0("data/", park, "/tidy/", name, "_tidy-b20.rds"))
