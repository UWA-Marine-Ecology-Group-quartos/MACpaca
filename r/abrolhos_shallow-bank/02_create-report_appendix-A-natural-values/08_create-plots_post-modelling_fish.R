###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Fish data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling fish figures for marine park reporting
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

# Fish models pool the survey years, so there is a single prediction surface
# saved under this label by 06_model-data_fish.R
fish_label <- paste(years, collapse = "_")

# Load libraries
library(tidyverse)
library(terra)
library(sf)
library(ggplot2)
library(ggnewscale)
library(scales)
library(viridis)
library(patchwork)
library(tidyterra)
library(tidytext)
library(ggtext)
library(CheckEM)

# Load functions
file.sources <- list.files(pattern = "*.R", path = paste0("r/", park, "/functions/"), full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(113.15, 113.65, -28.25, -27.85)

# Load necessary spatial files
sf_use_s2(FALSE)

# Australian outline and state and commonwealth marine parks
marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Abrolhos")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

# State sanctuary zones. Abrolhos AMP is Commonwealth-only, so this is empty and
# the plotting functions skip the state layers. Set to a filtered sf object for
# parks that do have state reserves.
wasanc <- NULL

# Australian outline
aus <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc <- aus %>%
  st_crop(e) %>%
  st_transform(4326)

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

# Load the bathymetry data (GA 250m resolution)
bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)

names(bathy)[3] <- "Depth"

# Spatial predictions limits
prediction_limits <- c(113.15, 113.65, -28.25, -27.85)

# Pretty fish metric names mapped to raster layer stubs
fish_metric_lookup <- c(
  "Whole assemblage" = "richness",
  "CTI" = "cti",
  "Large Reef Fish Index*" = "b20",
  "Total abundance" = "abundance"
)

# Read the single pooled prediction surface
dat <- readRDS(
  paste0("output/model-output/", park, "/fish/",
         name, "_predicted-fish_", fish_label, ".rds")
)

if (!inherits(dat, "SpatRaster")) dat <- terra::rast(dat)
terra::crs(dat) <- "EPSG:4326"

dat_list <- setNames(list(dat), fish_label)

# =============================================================================
# SST PROCESSING - run once to create the SST time series used by the Reef
# Fish Thermal Index (CTI) control plot.
#
# TODO Download the IMOS 6-day SST product (more recent coverage than the
# 1-month product used in Appendix B - 03_create-report_appendix-B-pressures/
# 01_spatial-layers.R): "IMOS - SRS - SST - L3S - Single Sensor - 6 day - day
# and night time - Australia" from the AODN portal, and save it as:
#   data/<park>/spatial/oceanography/SST_recent.nc
#
# NOTE Kelvin is converted to Celsius ONCE, at extraction below. Do not subtract
# 273.15 again in the climatology loop or the time-series summary.
# =============================================================================

library(RNetCDF)
library(lubridate)

nc_sst <- open.nc(paste0("data/", park, "/spatial/oceanography/SST_recent.nc"))
print.nc(nc_sst)

# Extract raw arrays
sst_var <- var.get.nc(nc_sst, "sea_surface_temperature")
lat     <- var.get.nc(nc_sst, "lat")
lon     <- var.get.nc(nc_sst, "lon")
time_nc <- var.get.nc(nc_sst, "time")

# Convert time to dates
dates_sst <- as.Date(utcal.nc("seconds since 1981-01-01 00:00:00", time_nc, type = "c"))

close.nc(nc_sst) # close before raster operations to avoid GDAL errors

# Convert Kelvin to Celsius (ONCE) and fix dimension order
# [lon, lat, time] -> [lat, lon, time]
sst_var       <- sst_var - 273.15
sst_corrected <- aperm(sst_var, c(2, 1, 3))

# Create raster stack
rast_sst <- terra::rast(sst_corrected,
                        extent = terra::ext(min(lon), max(lon), min(lat), max(lat)),
                        crs    = "EPSG:4326")

# Assign dates, crop and trim to study extent
names(rast_sst) <- as.character(dates_sst)
time(rast_sst)  <- dates_sst
rast_sst        <- terra::crop(rast_sst, e) %>% terra::trim()

# Check orientation - if upside down run: rast_sst <- terra::flip(rast_sst, "vertical")
plot(rast_sst[[1]])

# ---- Keep only years with all 12 months represented ----
# Partial years bias both the climatology and the annual means in the CTI
# control plot, so they are dropped here.
layer_dates   <- time(rast_sst)
months_per_yr <- tapply(month(layer_dates), year(layer_dates),
                        function(m) length(unique(m)))
complete_years <- as.numeric(names(months_per_yr)[months_per_yr == 12])

message("Dropping incomplete SST years: ",
        paste(setdiff(unique(year(layer_dates)), complete_years), collapse = ", "))

if (length(complete_years) == 0) stop("No years with 12 months of SST data")

rast_sst <- terra::subset(rast_sst, year(layer_dates) %in% complete_years)

# Build monthly climatology
sst_list <- list()
for (month in sort(unique(month(time(rast_sst))))) {
  monthly_rast <- subset(rast_sst, month(time(rast_sst)) == month) %>%
    mean(na.rm = TRUE)
  names(monthly_rast) <- month.abb[month]
  sst_list[[month.abb[month]]] <- monthly_rast
}
sst <- rast(sst_list)

saveRDS(sst, paste0("data/", park, "/spatial/oceanography/", name, "_SST_raster-recent.rds"))

# Build monthly time-series summary
sst_tsdf <- terra::global(rast_sst, fun = "mean", na.rm = TRUE) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_sst, fun = "sd", na.rm = TRUE)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(
    sst = mean(mean, na.rm = TRUE),   # already Celsius (converted at extraction)
    sd  = mean(sd,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(season = case_when(
    month %in% c("04", "05", "06") ~ "Autumn",
    month %in% c("07", "08", "09") ~ "Winter",
    month %in% c("10", "11", "12") ~ "Spring",
    month %in% c("01", "02", "03") ~ "Summer"
  )) %>%
  glimpse()

saveRDS(sst_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_SST_time-series-recent.rds"))

boxplot(sst_tsdf$sst ~ sst_tsdf$month)

# -------------------------------------------------------------------
# Fish metric plots
# -------------------------------------------------------------------
for (metric_name in names(fish_metric_lookup)) {

  message("Building fish metric plot for: ", metric_name)

  layer_stub <- fish_metric_lookup[[metric_name]]

  # Only build plot if the prediction and SE layers both exist
  has_all_layers <- all(unlist(lapply(dat_list, function(x) {
    c(
      paste0("p_", layer_stub, ".fit") %in% names(x),
      paste0("p_", layer_stub, ".se.fit") %in% names(x)
    )
  })))

  if (!has_all_layers) {
    message("Skipping ", metric_name, ": missing .fit or .se.fit layer")
    next
  }

  p_metric <- fishmetric_plot(
    metric_name = metric_name,
    layer_stub = layer_stub,
    dat_list = dat_list,
    prediction_limits = prediction_limits,
    pred_limits = NULL,   # set numeric vector if you want fixed limits
    se_limits = NULL,     # auto-scale within metric
    wasanc = wasanc
  )

  print(p_metric)

  out_name <- metric_name %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "-") %>%
    str_replace_all("(^-|-$)", "")

  ggsave(
    filename = paste0(
      "plots/", park, "/fish/", name,
      "_predicted-individual-fish-metric_", out_name, "_",
      fish_label, ".png"
    ),
    plot = p_metric,
    height = 5,
    width = 9,
    dpi = 300,
    units = "in",
    bg = "white"
  )

  saveRDS(p_metric,
          paste0("plots/", park, "/fish/", name,
                 "_predicted-individual-fish-metric_", out_name, "_",
                 fish_label, ".rds")
  )
}

# -------------------------------------------------------------------
# Control plots by metric, facetted by depth class
# -------------------------------------------------------------------
# Predictions are pooled across survey years, so there is a single surface. It
# is labelled with the most recent survey year for the control plot x-axis.

control_all <- list(
  controldata_fish(
    dat = dat_list[[fish_label]],
    year = as.numeric(max(years)),
    amp_abbrv = "ABMP"   # TODO park abbreviation
  )
)

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
    depth_class = factor(
      depth_class,
      levels = c(
        "Shallow (0 - 30 m)",
        "Mesophotic (30 - 70 m)",
        "Rariphotic (70 - 200 m)"
      )
    )
  )

metric_lookup <- c(
  "richness"  = "Species richness (per BRUV)",
  "cti"       = "Community Thermal Index (\u00B0C)",
  "b20"       = "Large reef fish index* (biomass g per BRUV)",
  "abundance" = "Total abundance (per BRUV)"
)

for (metric_code in names(metric_lookup)) {

  message("Building control plot for metric: ", metric_lookup[[metric_code]])

  p_metric <- controlplot_fish(
    data = park_dat.control,
    metric = metric_code,
    amp_abbrv = "ABMP",   # TODO park abbreviation
    metric_label = metric_lookup[[metric_code]]
  )

  if (!is.null(p_metric)) {

    print(p_metric)

    out_name <- metric_lookup[[metric_code]] %>%
      stringr::str_to_lower() %>%
      stringr::str_replace_all("\u00b0", "") %>%
      stringr::str_replace_all("\\*", "") %>%
      stringr::str_replace_all("[()]", "") %>%
      stringr::str_replace_all("[[:space:]]+", "-")

    ggsave(
      filename = paste0(
        "plots/", park, "/fish/", name, "_control-plot_", out_name, ".png"
      ),
      plot = p_metric,
      height = 8,
      width = 6,
      dpi = 300,
      units = "in",
      bg = "white"
    )

    saveRDS(p_metric,
            paste0("plots/", park, "/fish/", name, "_control-plot_", out_name, ".rds")
    )
  }
}


# Stacked plots

theme_collapse <- theme(
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white", linewidth = 0.25),
  plot.margin = grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text <- theme(
  strip.text.x = element_text(size = 5, angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x = element_text(vjust = -0.0, size = 10),
  axis.title.y = element_text(vjust = 0.0, size = 10),
  axis.text.x = element_text(size = 8),
  axis.text.y = element_text(size = 8),
  legend.title = element_text(family = "TN", size = 8),
  legend.text = element_text(family = "TN", size = 8))

# read in STI
sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct() %>%
  glimpse()

# Create DF filter for Commonwealth waters only
metadata_amp <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  distinct(campaignid, sample, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_join(
    marine_parks_amp %>% dplyr::select(name, epbc),
    join = st_within,
    left = FALSE
  ) %>%
  st_drop_geometry()

# TODO check sampling effort by year and status before interpreting the SACs
metadata_amp %>% count(year, status)

# -----------------------------
# Species Accumulation Curves
# -----------------------------

sac_df <- readRDS(paste0("data/", park, "/tidy/", name, "_species-accumulation.rds"))

base_theme <- theme_bw(base_size = 13)

sac_sample <- ggplot(
  sac_df %>%
    filter(curve == "Sample-based detection/non-detection"),
  aes(
    x = x,
    y = richness,
    colour = status,
    fill = status,
    linetype = Year
  )
) +
  geom_ribbon(
    aes(
      ymin = richness - sd,
      ymax = richness + sd
    ),
    alpha = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    values = setNames(
      c("22", "solid"),
      as.character(years)
    )
  ) +
  scale_colour_manual(name = "Status",
                      values = c(
                        "No-Take" = "#7bbc63",
                        "Fished" = "#b9e6fb"
                      )
  ) +
  scale_fill_manual(name = "Status",
                    values = c(
                      "No-Take" = "#7bbc63",
                      "Fished" = "#b9e6fb"
                    )
  ) +
  labs(
    x = "Number of BRUV deployments",
    y = "Species richness"
  ) +
  base_theme

sac_sample

ggsave(
  paste0("plots/", park, "/fish/", name, "_SAC-sample.png"),
  plot = sac_sample,
  height = 4,
  width = 7,
  dpi = 300,
  units = "in",
  bg = "white"
)

saveRDS(sac_sample,
        paste0("plots/", park, "/fish/", name, "_SAC-sample.rds")
)

sac_individual <- ggplot(
  sac_df %>%
    filter(curve == "Individual-based rarefaction"),
  aes(
    x = x,
    y = richness,
    colour = status,
    fill = status,
    linetype = Year
  )
) +
  geom_ribbon(
    aes(
      ymin = richness - sd,
      ymax = richness + sd
    ),
    alpha = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    values = setNames(
      c("22", "solid"),
      as.character(years)
    )
  ) +
  scale_colour_manual(name = "Status",
                      values = c(
                        "No-Take" = "#7bbc63",
                        "Fished" = "#b9e6fb"
                      )
  ) +
  scale_fill_manual(name = "Status",
                    values = c(
                      "No-Take" = "#7bbc63",
                      "Fished" = "#b9e6fb"
                    )
  ) +
  labs(
    x = "Cumulative MaxN individuals",
    y = "Species richness"
  ) +
  base_theme

sac_plot <- sac_sample / sac_individual +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "a",
    tag_suffix = ")"
  ) &
  theme(
    legend.position = "right",
    plot.tag = element_text(face = "bold", size = 14)
  )

sac_plot

ggsave(
  paste0("plots/", park, "/fish/", name, "_SAC-faceted.png"),
  plot = sac_plot,
  height = 8,
  width = 7,
  dpi = 300,
  units = "in",
  bg = "white"
)

saveRDS(sac_plot,
        paste0("plots/", park, "/fish/", name, "_SAC-faceted.rds")
)

# Read in maxn (Commonwealth only)
maxn <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  semi_join(metadata_amp, by = c("campaignid", "sample")) %>%
  mutate(year = year(date_time)) %>%
  left_join(sti, by = c("family", "genus", "species")) %>%
  select(
    year, sample, scientific_name, family, genus, species, count,
    rls_thermal_niche
  ) %>%
  glimpse()

length(unique(maxn$sample)) * length(unique(maxn$scientific_name))

# workout mean maxn for each species ---
maxn.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(sti, by = "scientific") %>%
  glimpse()

if (length(years) > 1) {
  species_by_year <- split(maxn.10$scientific, maxn.10$year)
  species_year_count <- table(unlist(lapply(species_by_year, unique)))
  unique_species <- names(species_year_count[species_year_count == 1])
} else {
  unique_species <- character(0)
}

bar_maxn <- ggplot(
  maxn.10 %>%
    mutate(scientific_label = if_else(scientific %in% unique_species,
                                      paste0("**", scientific, "**"),
                                      scientific)),
  aes(x = reorder_within(scientific_label, maxn, year), y = maxn)
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(aes(ymin = pmax(maxn - se, 0), ymax = maxn + se), width = 0.2) +
  coord_flip() +
  facet_wrap(~year, nrow = 1, scales = "free_y") +
  scale_x_reordered() +
  labs(
    x = "Species",
    y = expression(Average~abundance~(MaxN~per~BRUV))) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown(),
        panel.grid.major.x = element_line(color = "grey90"))

bar_maxn

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.png"),
       plot = bar_maxn, height = 4, width = 11, dpi = 300, units = "in", bg = "white")

saveRDS(bar_maxn,
        paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.rds")
)


# Thermal Index stacked plot
cti.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop") %>%
  left_join(sti, by = "scientific") %>%
  filter(!is.na(rls_thermal_niche)) %>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  glimpse()

if (length(years) > 1) {
  species_by_year_cti <- split(cti.10$scientific, cti.10$year)
  species_year_count_cti <- table(unlist(lapply(species_by_year_cti, unique)))
  unique_species_cti <- names(species_year_count_cti[species_year_count_cti == 1])
} else {
  unique_species_cti <- character(0)
}

log1p10_trans <- trans_new(
  name = "log10p1",
  transform = function(x) log10(x + 1),
  inverse   = function(x) 10^x - 1
)

# choose the centering statistic
mid_niche <- median(cti.10$rls_thermal_niche, na.rm = TRUE)

# global limits across both facets/years
niche_limits <- range(cti.10$rls_thermal_niche, na.rm = TRUE)

bar_cti <- ggplot(
  cti.10 %>%
    mutate(
      scientific_label = if_else(scientific %in% unique_species_cti,
                                 paste0("**", scientific, "**"),
                                 scientific),
      niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01)
    ),
  aes(
    x = reorder_within(scientific_label, rls_thermal_niche, year),
    y = maxn,
    fill = rls_thermal_niche
  )
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(
      ymin = pmax(maxn - se, 0),
      ymax = maxn + se
    ),
    width = 0.2
  ) +
  geom_text(aes(y = 23, label = niche_lab), hjust = 0, size = 3) +
  coord_flip(clip = "off") +
  facet_wrap(~year, nrow = 1, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(
    trans = log1p10_trans,
    expand = expansion(mult = c(0, 0.15)),
    breaks = c(0, 5, 10, 20),
    labels = scales::label_number()
  ) +
  # centre GREY at the mean thermal niche
  scale_fill_gradientn(
    colours = c("#2b83ba", "grey", "#d7191c"),
    values  = scales::rescale(c(niche_limits[1],
                                mid_niche,
                                niche_limits[2])),
    limits = niche_limits,
    na.value = "grey80"
  ) +
  guides(fill = "none") +
  labs(
    x = "Species",
    y = expression(Log[10]~(Average~abundance~+~1))
  ) +
  theme_bw() +
  theme_collapse +
  theme(axis.text.y = element_markdown(),
        panel.grid.major.x = element_line(color = "grey90"))

bar_cti

ggsave(paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.png"),
       plot = bar_cti, height = 4, width = 11, dpi = 300, units = "in", bg = "white")

saveRDS(bar_cti,
        paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.rds")
)

# B20 ---------------------------------------------------------------------

# read in b20 species summaries (already mean + sd per year x species)
b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_b20-species_amp.rds"))

# top 10 b20 per year using combined values only
b20.10 <- b20 %>%
  filter(status == "Combined") %>%
  group_by(year) %>%
  slice_max(order_by = b20, n = 10, with_ties = FALSE) %>%
  ungroup()

# species unique to either year's top 10 (for bold labels)
if (length(years) > 1) {
  species_by_year_b20 <- split(b20.10$scientific_name, b20.10$year)
  species_year_count_b20 <- table(unlist(lapply(species_by_year_b20, unique)))
  unique_species_b20 <- names(species_year_count_b20[species_year_count_b20 == 1])
} else {
  unique_species_b20 <- character(0)
}

# common plot function
plot_b20_bars <- function(plot_data, fill_values, fill_breaks) {
  ggplot(
    plot_data %>%
      mutate(
        scientific_label = if_else(
          scientific_name %in% unique_species_b20,
          paste0("**", scientific_name, "**"),
          scientific_name
        )
      ),
    aes(
      x = reorder_within(scientific_label, b20, year),
      y = b20,
      fill = status
    )
  ) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.7,
      colour = "black",
      linewidth = 0.25
    ) +
    geom_errorbar(
      aes(ymin = pmax(b20 - se, 0), ymax = b20 + se),
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    coord_flip() +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(base = 10),
      breaks = c(0, 1, 10, 100, 1000),
      labels = scales::label_number()
    ) +
    facet_wrap(~year, nrow = 1, scales = "free_y") +
    scale_x_reordered() +
    scale_fill_manual(
      values = fill_values,
      breaks = fill_breaks
    ) +
    labs(
      x = "Species",
      y = expression(Average~biomass~(B20~per~BRUV)),
      fill = "Status"
    ) +
    theme_bw() +
    theme_collapse +
    theme(
      axis.text.y = element_markdown(),
      panel.grid.major.x = element_line(color = "grey90")
    )
}

# -------------------------------------------------------------------------
# Plot 1: both years split into Fished / No-Take
# -------------------------------------------------------------------------

b20_plot_split <- b20 %>%
  filter(status != "Combined") %>%
  semi_join(b20.10, by = c("year", "scientific_name")) %>%
  mutate(
    status = if_else(status %in% "Fished", "Open", status),
    status = factor(status, levels = c("Open", "No-Take"))
  )

bar_b20 <- plot_b20_bars(
  plot_data   = b20_plot_split,
  fill_values = c("Open" = "white", "No-Take" = "grey40"),
  fill_breaks = c("No-Take", "Open")
)

bar_b20

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot.png"),
  plot   = bar_b20,
  height = 4,
  width  = 11,
  dpi    = 300,
  units  = "in",
  bg     = "white"
)

# -------------------------------------------------------------------------
# Plot 2: first survey year Combined, most recent split into Fished / No-Take
# Use this when the earlier year has too few deployments in one status to
# support a split (2021 Fished n = 5 at this park).
# TODO check and edit status
# -------------------------------------------------------------------------

b20_plot_mixed <- b20 %>%
  semi_join(b20.10, by = c("year", "scientific_name")) %>%
  filter(
    (year == years[1] & status == "Combined") |
      (year == years[length(years)] & status %in% c("Fished", "No-Take"))
  ) %>%
  mutate(
    status = if_else(status %in% c("Combined", "Fished"), "Open", status),
    status = factor(status, levels = c("Open", "No-Take"))
  )

bar_b20_v2 <- plot_b20_bars(
  plot_data   = b20_plot_mixed,
  fill_values = c(
    "Open"   = "white",
    "No-Take"  = "grey40"
  ),
  fill_breaks = c("No-Take", "Open")
)

bar_b20_v2

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot_mixed.png"),
  plot   = bar_b20_v2,
  height = 4,
  width  = 11,
  dpi    = 300,
  units  = "in",
  bg     = "white"
)

saveRDS(bar_b20_v2,
        paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot_mixed.rds")
)

# -------------------------------------------------------------------
# Bubble plots
# -------------------------------------------------------------------

tidy_count <- readRDS(paste0("data/", park, "/tidy/", name, "_tidy-count.rds")) %>%
  semi_join(metadata_amp, by = c("campaignid", "sample"))

bubble_combined <- bubble_plots(
  dat                 = tidy_count,
  ausc                = ausc,
  cwatr               = cwatr,
  marine_parks_amp    = marine_parks_amp,
  wasanc              = wasanc,
  prediction_limits   = prediction_limits,
  # TODO Bubble plots are zoomed in tighter than the prediction surface
  bubble_limits       = c(113.3, 113.6, -28.2, -27.95),
  size_range          = c(1, 9),
  n_size_breaks       = 6
)

bubble_combined

ggsave(
  paste0("plots/", park, "/fish/", name, "_bubbleplot_richness-abundance.png"),
  plot   = bubble_combined,
  height = 9,
  width  = 8,
  dpi    = 300,
  units  = "in",
  bg     = "white"
)

saveRDS(bubble_combined,
        paste0("plots/", park, "/fish/", name, "_bubbleplot_richness-abundance.rds"))
