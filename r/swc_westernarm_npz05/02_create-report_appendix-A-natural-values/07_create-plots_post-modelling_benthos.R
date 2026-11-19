###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Habitat data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling habitat figures for marine park reporting
# Author:  Claude Spencer & Henry Evans
# Date:    July 2026
###

# Clear your environment
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
years <- config$years
combine_benthos <- config$combine_benthos


benthos_label <- if (combine_benthos) paste(years, collapse = "_") else NA
pred.labels <- if (combine_benthos) benthos_label else years

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggnewscale)
library(scales)
library(tidyterra)
library(patchwork)
library(scatterpie)
library(CheckEM)
library(grid)
library(viridis)
library(geos)

# Load functions
file.sources <- list.files(pattern = "*.R", path = paste0("r/", park, "/functions/"), full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(114.20, 115.40, -34.60, -33.90)   # TODO must be wider than every plot window, and must reach the shoreline or ausc/cwatr crop to nothing

# Load necessary spatial files
ausc <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(e) %>%
  st_transform(4326)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Ngari Capes", "Geographe", "South-west Corner")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

npz <- marine_parks[marine_parks$zone %in% "National Park Zone", ]
wasanc <- marine_parks[marine_parks$zone %in% "Sanctuary Zone", ]

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

cwatr_offset <- st_as_sf(geos_offset_curve(as_geos_geometry(cwatr), distance = 0.003))
st_crs(cwatr_offset) <- 4326

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

# Map pretty habitat names to raster layer prefixes in dat
habitat_lookup <- c(
  "Sand" = "sand",
  "Seagrass" = "seagrass",
  "Macroalgae" = "macro",
  "Sessile invertebrates" = "inverts",
  "Rock" = "rock"
)

# Optional habitat colours for other functions if needed
hab_cols <- c(
  "Sand" = "wheat",
  "Macroalgae" = "darkgoldenrod4",
  "Seagrass" = "forestgreen",
  "Rock" = "grey40",
  "Sessile invertebrates" = "plum"
)

# TODO Plot extent
prediction_limits <- c(114.44, 114.99, -34.17, -34.03)

# Read all years once

if (combine_benthos) {
  dat_list <- setNames(
    list(readRDS(paste0("output/model-output/", park, "/habitat/",
                        name, "_predicted-habitat_", benthos_label, ".rds"))),
    benthos_label)
} else {
  dat_list <- setNames(vector("list", length(years)), years)

  for (yr in years) {
    message("Reading year: ", yr)

    dat_list[[as.character(yr)]] <- readRDS(
      paste0(
        "output/model-output/", park, "/habitat/",
        name, "_predicted-habitat_", yr, ".rds"
      )
    )
  }
}

# -------------------------------------------------------------------
# PART 1: Multi-year plots (categorical + dominant benthos)
# -------------------------------------------------------------------
# One faceted figure per plot type covering every year, rather than one
# figure per year. The multi functions take dat_list directly and do their
# own as.data.frame() and normalise_se() internally.

p_cat <- categoricalhabitat_plot_multi(
  dat_list          = dat_list,
  prediction_limits = prediction_limits,
  habitat_lookup    = habitat_lookup
)

print(p_cat)

# TODO Sized for a 2 x 2 grid of years - revisit if the year count changes
ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-habitat-categorical_",
    paste(pred.labels, collapse = "-"), ".png"
  ),
  plot = p_cat,
  height = 9,
  width = 8,
  dpi = 300,
  units = "in",
  bg = "white"
)

saveRDS(p_cat,
        paste0(
          "plots/", park, "/habitat/", name,
          "_predicted-habitat-categorical_",
          paste(pred.labels, collapse = "-"), ".rds"
        )
)

p_dom <- dominantbenthos_plot_multi(
  dat_list          = dat_list,
  prediction_limits = prediction_limits,
  habitat_lookup    = habitat_lookup
)

print(p_dom)

# TODO dominantbenthos_plot_multi stacks the years down the page with a
# prediction column and an SE column, so this is sized tall. With four years
# it will not fit an A4 portrait page at full size - check the rendered PDF.
ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-dominant-habitat_",
    paste(pred.labels, collapse = "-"), ".png"
  ),
  plot = p_dom,
  height = 13,
  width = 7,
  dpi = 300,
  units = "in",
  bg = "white"
)

saveRDS(p_dom,
        paste0(
          "plots/", park, "/habitat/", name,
          "_predicted-dominant-habitat_",
          paste(pred.labels, collapse = "-"), ".rds"
        )
)


## Predicted reef
p_reef <- predictedreef_plot_multi(
  dat_list          = dat_list,
  prediction_limits = prediction_limits
)

print(p_reef)

ggsave(
  filename = paste0(
    "plots/", park, "/habitat/", name,
    "_predicted-reef-and-se_",
    paste(years, collapse = "-"), ".png"
  ),
  plot = p_reef, height = 9, width = 9, dpi = 300, units = "in", bg = "white"
)

saveRDS(p_reef,
        paste0("plots/", park, "/habitat/", name,
               "_predicted-reef-and-se_", paste(years, collapse = "-"), ".rds"))

# -------------------------------------------------------------------
# PART 3: Multi-year individual habitat plots
# -------------------------------------------------------------------
# NOTE for some reason this overwrites  swc_westernarm_predicted-dominant-habitat_2020_2023_2024_2025.png
# so rerun that bit
for (habitat_name in names(habitat_lookup)) {

  message("Building individual habitat plot for: ", habitat_name)

  layer_stub <- habitat_lookup[[habitat_name]]

  p_hab <- individualbenthic_plot(
    habitat_name = habitat_name,
    layer_stub = layer_stub,
    dat_list = dat_list,
    prediction_limits = prediction_limits,
    pred_limits = NULL,   # use c(0, 1) for a fixed probability scale across taxa
    se_limits = NULL      # auto-scale within habitat across years
  )

  print(p_hab)

  out_name <- habitat_name %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "-")

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name,
      "_predicted-individual-habitat_", out_name, "_",
      paste(pred.labels, collapse = "-"), ".png"
    ),
    plot = p_hab,
    height = 7,
    width = 10,
    dpi = 300,
    units = "in",
    bg = "white"
  )

  saveRDS(p_hab,
          paste0(
            "plots/", park, "/habitat/", name,
            "_predicted-individual-habitat_", out_name, "_",
            paste(pred.labels, collapse = "-"), ".rds"
          ))
}

# -------------------------------------------------------------------
# PART 4: Control plots by taxa, facetted by depth class
# -------------------------------------------------------------------
if (!combine_benthos) {

  # Create the data (makes a dataframe for each ecosystem depth contour)
  control_all <- purrr::map(years, \(yy) {
    dat_yy <- readRDS(
      paste0(
        "output/model-output/", park, "/habitat/",
        name, "_predicted-habitat_", yy, ".rds"
      )
    )
    controldata_benthos(dat = dat_yy, year = yy, amp_abbrv = "SWCMP", state_abbrv = "NCMP")
  })

  park_dat.shallow <- purrr::map_dfr(control_all, "shallow") %>%
    dplyr::mutate(depth_class = "Shallow (0 - 30 m)")

  park_dat.meso <- purrr::map_dfr(control_all, "meso") %>%
    dplyr::mutate(depth_class = "Mesophotic (30 - 70 m)")

  park_dat.rari <- purrr::map_dfr(control_all, "rari") %>%
    dplyr::mutate(depth_class = "Rariphotic (70 - 200 m)")

  park_dat.control <- dplyr::bind_rows(
    park_dat.shallow,
    park_dat.meso,
    park_dat.rari
  ) %>%
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      depth_class = factor(
        depth_class,
        levels = c(
          "Shallow (0 - 30 m)",
          "Mesophotic (30 - 70 m)",
          "Rariphotic (70 - 200 m)"
        )
      )
    )

  # Taxa to plot
  taxa_lookup <- c(
    "seagrass"   = "Seagrass",
    "macroalgae" = "Macroalgae",
    "rock"       = "Rock",
    "sand"       = "Sand",
    "inverts"    = "Sessile invertebrates"
  )

  for (taxa_code in names(taxa_lookup)) {

    message("Building control plot for taxon: ", taxa_lookup[[taxa_code]])

    p_taxa <- controlplot_benthos(
      data = park_dat.control,
      taxa = taxa_code,
      amp_abbrv = "SWCMP",
      state_abbrv = "NCMP",
      taxa_label = taxa_lookup[[taxa_code]]
    )

    if (!is.null(p_taxa)) {

      print(p_taxa)

      out_name <- taxa_lookup[[taxa_code]] %>%
        stringr::str_to_lower() %>%
        stringr::str_replace_all("\\s+", "-")

      ggsave(
        filename = paste0(
          "plots/", park, "/habitat/", name, "_control-plot_", out_name, ".png"
        ),
        plot = p_taxa,
        height = 4,
        width = 6,
        dpi = 300,
        units = "in",
        bg = "white"
      )

      saveRDS(p_taxa,
              paste0(
                "plots/", park, "/habitat/", name,
                "_control-plot_", out_name, ".rds"
              ))
    }
  }

}

# ---- Scatterpie data prep ----

# TODO Set the extent of the study
e <- ext(114.20, 115.40, -34.60, -33.90)   # TODO must be wider than every plot window, and must reach the shoreline or ausc/cwatr crop to nothing

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

metadata_bathy_derivatives <- readRDS(
  paste0("data/", park, "/tidy/", name, "_metadata-bathymetry-derivatives.rds")
) %>%
  clean_names()

benthos <- readRDS(
  paste0("data/", park, "/tidy/", name, "_benthos-count.RDS")
) %>%
  dplyr::rename(
    Macroalgae = macroalgae,
    Seagrass = seagrasses,
    Sand = sand,
    Rock = rock,
    "Sessile invertebrates" = sessile_invertebrates
  ) %>%
  left_join(metadata_bathy_derivatives, by = c("campaignid", "sample", "year", "status")) %>%
  arrange(desc(Sand))

hab_fills <- scale_fill_manual(
  name = "",
  limits = c("Rock", "Sessile invertebrates", "Macroalgae", "Seagrass", "Sand"),
  values = c(
    "Rock" = "grey40",
    "Sessile invertebrates" = "plum",
    "Macroalgae" = "darkgoldenrod4",
    "Seagrass" = "forestgreen",
    "Sand" = "wheat"
  )
)

wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
  # "Marine Nature Reserve" = "#bfd054"
),
name = "State Marine Parks")

depth_fills <- scale_fill_manual(
  values = c("#a7cfe0", "#9acbec", "#98c4f7", "#a3bbff", "#81a1fc"),
  guide = "none"
)

# Frame the scatterpies on the samples rather than a hardcoded box. Computed
# once here and passed to both the per-year and multi-year plots so every
# panel sits on the same window.
# TODO Widen site_pad if the coastline falls outside the frame
site_pad <- 0.05

site_limits <- benthos %>%
  dplyr::filter(is.finite(longitude_dd), is.finite(latitude_dd)) %>%
  dplyr::summarise(
    xmin = min(longitude_dd) - site_pad,
    xmax = max(longitude_dd) + site_pad,
    ymin = min(latitude_dd)  - site_pad,
    ymax = max(latitude_dd)  + site_pad
  ) %>%
  unlist(use.names = FALSE)

# TODO Check the frame the scatterpies will be drawn on
message("Scatterpie extent: ", paste(round(site_limits, 3), collapse = ", "))

if (combine_benthos) {

  # Pooled run: all years' sites sit in different spatial areas, so plot
  # them together on one unfacetted map rather than per-year + multi-year.
  message("Building pooled scatterpie for: ", benthos_label)

  benthos_pooled <- benthos %>%
    dplyr::filter(
      is.finite(longitude_dd),
      is.finite(latitude_dd)
    ) %>%
    dplyr::arrange(desc(Sand))

  p_scatterpie <- scatterpie_plot_single(
    benthos_year = benthos_pooled,
    site_limits = site_limits,
    pie_radius = 0.008
  )

  print(p_scatterpie)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name, "_scatterpie_", benthos_label, ".png"
    ),
    plot = p_scatterpie,
    height = 7,
    width = 10,
    dpi = 300,
    bg = "white"
  )

  saveRDS(p_scatterpie,
          paste0(
            "plots/", park, "/habitat/", name,
            "_scatterpie_", benthos_label, ".rds"
          ))

} else {

  for (yr in years) {

    message("Year: ", yr)

    benthos_year <- benthos %>%
      dplyr::filter(as.character(year) == as.character(yr)) %>%
      dplyr::filter(
        is.finite(longitude_dd),
        is.finite(latitude_dd)
      ) %>%
      dplyr::arrange(desc(Sand))

    p_scatterpie <- scatterpie_plot_single(
      benthos_year = benthos_year,
      site_limits = site_limits,
      pie_radius = 0.008
    )

    print(p_scatterpie)

    ggsave(
      filename = paste0(
        "plots/", park, "/habitat/", name, "_scatterpie_", yr, ".png"
      ),
      plot = p_scatterpie,
      height = 7,
      width = 10,
      dpi = 300,
      bg = "white"
    )

    saveRDS(p_scatterpie,
            paste0(
              "plots/", park, "/habitat/", name,
              "_scatterpie_", yr, ".rds"
            ))
  }

  p_scatterpie_multi <- scatterpie_plot_multi(
    benthos = benthos,
    years = years,
    site_limits = site_limits,
    pie_radius = 0.008
  )

  print(p_scatterpie_multi)

  ggsave(
    filename = paste0(
      "plots/", park, "/habitat/", name, "_scatterpie_",
      paste(years, collapse = "-"), ".png"
    ),
    plot = p_scatterpie_multi,
    height = 7,
    width = 10,
    dpi = 300,
    bg = "white"
  )

  saveRDS(p_scatterpie_multi,
          paste0(
            "plots/", park, "/habitat/", name,
            "_scatterpie_",
            paste(years, collapse = "-"), ".rds"
          ))
}

