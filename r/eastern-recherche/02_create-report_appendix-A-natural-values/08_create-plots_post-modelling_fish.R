###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Fish data synthesis & habitat models derived from FSSgam
# Task:    Create post-modelling fish figures for marine park reporting
# Author:  Claude Spencer
# Date:    June 2024
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

name  <- config$name
park  <- config$park
years <- config$years

# Load libraries ----
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
library(lubridate)
library(CheckEM)
library(RNetCDF)

# Load functions ----
file.sources <- list.files(pattern = "*.R", path = paste0("r/", park, "/functions/"), full.names = TRUE)
sapply(file.sources, source, .GlobalEnv)

# controlplot_fish() defined inline so the CTI panel carries the monthly SST
# overlay (solid black line + grey SD ribbon). Defined after the source() loop,
# so it overrides any controlplot_fish() in functions/.
controlplot_fish <- function(data, metric, amp_abbrv, state_abbrv,
                             metric_label = NULL,
                             depth_levels = c("Shallow (0 - 30 m)",
                                              "Mesophotic (30 - 70 m)",
                                              "Rariphotic (70 - 200 m)")) {

  mean_col <- metric
  se_col   <- paste0(metric, "_se")

  if (is.null(metric_label)) {
    metric_label <- dplyr::case_when(
      metric == "richness"  ~ "Species richness (per BRUV)",
      metric == "cti"       ~ "Community Thermal Index (\u00B0C)",
      metric == "b20"       ~ "Large reef fish index* (biomass g per BRUV)",
      metric == "abundance" ~ "Total abundance (per BRUV)",
      TRUE ~ stringr::str_to_title(metric)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(year),
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(
        zone_new,
        levels = c(
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(amp_abbrv, "other zones"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones")
        )
      )
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", metric_label)
    return(NULL)
  }

  fill_vals <- setNames(
    c("#fff8a3", "#7bbc63", "#b9e6fb", "#bfd054", "#bddde1"),
    c(
      paste(amp_abbrv, "HPZ"),
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones"),
      paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones")
    )
  )

  shape_vals <- setNames(
    c(21, 21, 21, 25, 25),
    c(
      paste(amp_abbrv, "HPZ"),
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones"),
      paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones")
    )
  )

  # Year axis derived from the data (no hard-coded survey years) ----
  yr_breaks <- sort(unique(plot_dat$year))

  if (metric == "cti") {

    # SST series supplied as <name>_SST_time-series.rds
    # (columns: year, month, sst, sd, season). Aggregated to an annual mean below
    # (matching the original code) and plotted as a line on the year x-axis.
    # NOTE: the supplied sst column is offset by -273.15; +273.15 returns it to
    # degrees C so it is comparable to CTI on the shared y-axis. Set to 0 to use
    # the raw stored values.
    sst_offset <- 273.15

    sst <- readRDS(
      paste0("data/", park, "/spatial/oceanography/",
             name, "_SST_time-series.rds")
    ) %>%
      dplyr::mutate(
        year = as.numeric(year),
        sst  = sst + sst_offset
      ) %>%
      dplyr::filter(!is.na(sst), year >= 2016, year <= 2026) %>%
      # annual mean SST (matches the original code) - avoids the busy monthly
      # seasonal sawtooth. For a monthly line, drop this group_by/summarise and
      # plot on a decimal-year x instead.
      dplyr::group_by(year) %>%
      dplyr::summarise(
        sst = mean(sst, na.rm = TRUE),
        sd  = mean(sd,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(year)

    # fixed x range 2016-2026
    x_lims <- c(2016, 2026)

    p <- ggplot() +
      # grey error mask (annual SD) - drawn first so it sits behind the line
      geom_ribbon(
        data = sst,
        aes(x = year, ymin = sst - sd, ymax = sst + sd),
        fill = "grey60",
        alpha = 0.35
      ) +
      # solid black annual SST line
      geom_line(
        data = sst,
        aes(x = year, y = sst),
        colour = "black",
        linewidth = 0.6
      ) +
      geom_errorbar(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          ymin = .data[[mean_col]] - .data[[se_col]],
          ymax = .data[[mean_col]] + .data[[se_col]],
          fill = zone_new,
          shape = zone_new
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          fill = zone_new,
          shape = zone_new
        ),
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = 2018,
        linetype = "dashed",
        color = "grey50",
        linewidth = 0.5,
        alpha = 0.7
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2018, 2022, 2025)) +
      coord_cartesian(xlim = x_lims) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = TRUE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = TRUE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )

  } else {

    p <- ggplot(
      data = plot_dat,
      aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
    ) +
      geom_errorbar(
        aes(
          ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
          ymax = .data[[mean_col]] + .data[[se_col]]
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = 2018,
        linetype = "dashed",
        color = "grey50",
        linewidth = 0.5,
        alpha = 0.7
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2018, 2022, 2025)) +
      coord_cartesian(xlim = c(2016, 2026), ylim = c(0, NA)) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = TRUE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = TRUE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )
  }

  return(p)
}

# TODO Set cropping extent - larger than most zoomed out plot
e <- ext(123.1, 124.0, -34.7, -33.9)

# Load necessary spatial files ----
sf_use_s2(FALSE)

marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) # TODO select relevant parks

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  st_transform(4326)

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  st_transform(4326)

aus   <- st_read("data/south-west network/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
ausc  <- aus %>%
  st_crop(e) %>%
  st_transform(4326)

cwatr <- st_read("data/south-west network/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_crop(e) %>%
  st_transform(4326)

bathy <- rast("data/south-west network/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif") %>%
  crop(e) %>%
  clamp(upper = 0, lower = -250, values = FALSE) %>%
  trim() %>%
  as.data.frame(xy = TRUE, na.rm = TRUE)
names(bathy)[3] <- "Depth"

# Spatial prediction limits
prediction_limits <- c(123.1, 124.0, -34.7, -33.9)

# Pretty fish metric names mapped to raster layer stubs ----
fish_metric_lookup <- c(
  "Whole assemblage"       = "richness",
  "CTI"                    = "cti",
  "Large Reef Fish Index*" = "b20",
  "Total abundance"        = "abundance"
)

# Read all years once ----
dat_list <- setNames(vector("list", length(years)), years)

for (yr in years) {
  message("Reading year: ", yr)

  dat <- readRDS(
    paste0(
      "output/model-output/", park, "/fish/",
      name, "_predicted-fish_", yr, ".rds"
    )
  )

  if (!inherits(dat, "SpatRaster")) dat <- terra::rast(dat)
  terra::crs(dat) <- "EPSG:4326"

  dat_list[[as.character(yr)]] <- dat
}


# =============================================================================
# SST PROCESSING — run once to create saved SST files used by controlplot_fish
# =============================================================================

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

# Convert Kelvin to Celsius and fix dimension order [lon, lat, time] -> [lat, lon, time]
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

# Remove 2025 data before any further processing
rast_sst <- rast_sst[[year(time(rast_sst)) < 2025]]

plot(rast_sst)
# Check orientation — if upside down run: rast_sst <- terra::flip(rast_sst, "vertical")
plot(rast_sst[[1]])

winter_sst_ts <- rast_sst[[which(month(dates_sst) %in% c(7, 8, 9))]]

# Build monthly climatology
sst_list <- list()
for (month in sort(unique(month(time(rast_sst))))) {
  monthly_rast <- subset(rast_sst, month(time(rast_sst)) == month) %>%
    mean(na.rm = TRUE) %>%
    app(fun = function(i) { i - 273.15 })
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
    sst = mean(mean, na.rm = TRUE) - 273.15, # Apply -273.15 offset (controlplot_fish adds it back)
    sd  = mean(sd,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  dplyr::mutate(season = case_when(
    month %in% c("04", "05", "06") ~ "Autumn",
    month %in% c("07", "08", "09") ~ "Winter",
    month %in% c("10", "11", "12") ~ "Spring",
    month %in% c("01", "02", "03") ~ "Summer"
  )) %>%
  glimpse()

saveRDS(sst_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_SST_time-series-recent.rds"))

boxplot(sst_tsdf$sst ~ sst_tsdf$month)

# =============================================================================
# PART 1: Fish metric spatial plots (prediction + SE side by side, per year)
# =============================================================================

for (metric_name in names(fish_metric_lookup)) {

  message("Building fish metric plot for: ", metric_name)

  layer_stub <- fish_metric_lookup[[metric_name]]

  # Only build plot if every year has both prediction and SE layers
  has_all_layers <- all(unlist(lapply(dat_list, function(x) {
    c(
      paste0("p_", layer_stub, ".fit")    %in% names(x),
      paste0("p_", layer_stub, ".se.fit") %in% names(x)
    )
  })))

  if (!has_all_layers) {
    message("Skipping ", metric_name, ": missing .fit or .se.fit layer in one or more years")
    next
  }

  p_metric <- fishmetric_plot(
    metric_name       = metric_name,
    layer_stub        = layer_stub,
    dat_list          = dat_list,
    prediction_limits = prediction_limits,
    pred_limits       = NULL,   # set numeric vector if you want fixed limits
    se_limits         = NULL    # auto-scale within metric across years
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
      paste(years, collapse = "-"), ".png"
    ),
    plot   = p_metric,
    height = 5,
    width  = 8,
    dpi    = 900,
    units  = "in",
    bg     = "white"
  )

  saveRDS(
    p_metric,
    paste0(
      "plots/", park, "/fish/", name,
      "_predicted-individual-fish-metric_", out_name, "_",
      paste(years, collapse = "-"), ".rds"
    )
  )
}

# =============================================================================
# PART 2: Control plots by metric, facetted by depth class
# =============================================================================
controlplot_fish <- function(data, metric, amp_abbrv, state_abbrv,
                             metric_label = NULL,
                             depth_levels = c("Shallow (0 - 30 m)",
                                              "Mesophotic (30 - 70 m)",
                                              "Rariphotic (70 - 200 m)")) {

  mean_col <- metric
  se_col   <- paste0(metric, "_se")

  if (is.null(metric_label)) {
    metric_label <- dplyr::case_when(
      metric == "richness"  ~ "Species richness (per BRUV)",
      metric == "cti"       ~ "Community Thermal Index (\u00B0C)",
      metric == "b20"       ~ "Large reef fish index* (biomass g per BRUV)",
      metric == "abundance" ~ "Total abundance (per BRUV)",
      TRUE ~ stringr::str_to_title(metric)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(year),
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(
        zone_new,
        levels = c(
          paste(amp_abbrv, "HPZ"),
          paste(amp_abbrv, "NPZ (IUCN II)"),
          paste(amp_abbrv, "other zones"),
          paste(state_abbrv, "SZ (IUCN II)"),
          paste(state_abbrv, "other zones")
        )
      )
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", metric_label)
    return(NULL)
  }

  fill_vals <- setNames(
    c("#fff8a3", "#7bbc63", "#b9e6fb", "#bfd054", "#bddde1"),
    c(
      paste(amp_abbrv, "HPZ"),
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones"),
      paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones")
    )
  )

  shape_vals <- setNames(
    c(21, 21, 21, 25, 25),
    c(
      paste(amp_abbrv, "HPZ"),
      paste(amp_abbrv, "NPZ (IUCN II)"),
      paste(amp_abbrv, "other zones"),
      paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones")
    )
  )

  # Year axis derived from the data (no hard-coded survey years) ----
  yr_breaks <- sort(unique(plot_dat$year))

  if (metric == "cti") {

    # SST series supplied as <name>_SST_time-series.rds
    # (columns: year, month, sst, sd, season). Aggregated to an annual mean below
    # (matching the original code) and plotted as a line on the year x-axis.
    # NOTE: the supplied sst column is offset by -273.15; +273.15 returns it to
    # degrees C so it is comparable to CTI on the shared y-axis. Set to 0 to use
    # the raw stored values.
    sst_offset <- 273.15

    sst <- readRDS(
      paste0("data/", park, "/spatial/oceanography/",
             name, "_SST_time-series-recent.rds")
    ) %>%
      dplyr::mutate(
        year = as.numeric(year),
        sst  = sst + sst_offset
      ) %>%
      dplyr::filter(!is.na(sst), year >= 2016, year <= 2026) %>%
      # annual mean SST (matches the original code) - avoids the busy monthly
      # seasonal sawtooth. For a monthly line, drop this group_by/summarise and
      # plot on a decimal-year x instead.
      dplyr::group_by(year) %>%
      dplyr::summarise(
        sst = mean(sst, na.rm = TRUE),
        sd  = mean(sd,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(year)

    # fixed x range 2016-2026
    x_lims <- c(2016, 2026)

    p <- ggplot() +
      # grey error mask (annual SD) - drawn first so it sits behind the line
      geom_ribbon(
        data = sst,
        aes(x = year, ymin = sst - sd, ymax = sst + sd),
        fill = "grey60",
        alpha = 0.35
      ) +
      # solid black annual SST line
      geom_line(
        data = sst,
        aes(x = year, y = sst),
        colour = "black",
        linewidth = 0.6
      ) +
      geom_errorbar(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          ymin = .data[[mean_col]] - .data[[se_col]],
          ymax = .data[[mean_col]] + .data[[se_col]],
          fill = zone_new,
          shape = zone_new
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          fill = zone_new,
          shape = zone_new
        ),
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = 2018,
        linetype = "dashed",
        color = "grey50",
        linewidth = 0.5,
        alpha = 0.7
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2018, 2022, 2025)) +
      coord_cartesian(xlim = x_lims) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = TRUE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = TRUE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )

  } else {

    p <- ggplot(
      data = plot_dat,
      aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
    ) +
      geom_errorbar(
        aes(
          ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
          ymax = .data[[mean_col]] + .data[[se_col]]
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      geom_vline(
        xintercept = 2018,
        linetype = "dashed",
        color = "grey50",
        linewidth = 0.5,
        alpha = 0.7
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2018, 2022, 2025)) +
      coord_cartesian(xlim = c(2016, 2026), ylim = c(0, NA)) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = TRUE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = TRUE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )
  }

  return(p)
}


control_all <- purrr::map(years, \(yy) {
  dat_yy <- readRDS(
    paste0(
      "output/model-output/", park, "/fish/",
      name, "_predicted-fish_", yy, ".rds"
    )
  )

  if (!inherits(dat_yy, "SpatRaster")) dat_yy <- terra::rast(dat_yy)
  terra::crs(dat_yy) <- "EPSG:4326"

  controldata_fish(dat = dat_yy, year = yy, amp_abbrv = "ERMP", state_abbrv = "NCMP") # TODO park abbreviations
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
  # Eastern Recherche has a single relevant zone — keep it labelled "ERMP other
  # zones" so it maps to the blue filled-circle symbol (#b9e6fb, shape 21) in
  # controlplot_fish(); drop = TRUE then hides every other zone from the legend.
  dplyr::mutate(
    zone_new = dplyr::case_when(
      stringr::str_detect(zone_new, "NPZ")         ~ "ERMP NPZ (IUCN II)",
      stringr::str_detect(zone_new, "other zones") ~ "ERMP other zones",
      TRUE ~ zone_new
    )
  ) %>%
  dplyr::filter(zone_new == "ERMP other zones") %>%
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
    data         = park_dat.control,
    metric       = metric_code,
    amp_abbrv    = "ERMP", # TODO park abbreviation
    state_abbrv  = "NCMP",
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
      plot   = p_metric,
      height = 4,
      width  = 6,
      dpi    = 300,
      units  = "in",
      bg     = "white"
    )

    saveRDS(
      p_metric,
      paste0("plots/", park, "/fish/", name, "_control-plot_", out_name, ".rds")
    )
  }
}

# =============================================================================
# PART 3: Stacked plot themes
# =============================================================================

theme_collapse <- theme(
  panel.grid.major = element_line(colour = "white"),
  panel.grid.minor = element_line(colour = "white", size = 0.25),
  plot.margin = grid::unit(c(0, 0, 0, 0), "in")
)

theme.larger.text <- theme(
  strip.text.x = element_text(size = 5, angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x = element_text(vjust = -0.0, size = 10),
  axis.title.y = element_text(vjust = 0.0, size = 10),
  axis.text.x  = element_text(size = 8),
  axis.text.y  = element_text(size = 8),
  legend.title = element_text(size = 8),
  legend.text  = element_text(size = 8)
)

# STI lookup ----
sti <- CheckEM::australia_life_history %>%
  clean_names() %>%
  dplyr::select(family, genus, species, rls_thermal_niche) %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::distinct() %>%
  glimpse()

# Commonwealth waters metadata ----
marine_parks_amp <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
  dplyr::filter(name %in% c("Eastern Recherche")) %>% # TODO select relevant parks
  dplyr::filter(epbc == "Commonwealth") %>%
  st_transform(4326)

metadata_amp <- readRDS(paste0("data/", park, "/raw/metadata.RDS")) %>%
  distinct(campaignid, sample, .keep_all = TRUE) %>%
  st_as_sf(coords = c("longitude_dd", "latitude_dd"), crs = 4326, remove = FALSE) %>%
  st_join(
    marine_parks_amp %>% dplyr::select(name, epbc),
    join = st_within,
    left = FALSE
  ) %>%
  st_drop_geometry()

# =============================================================================
# PART 4: Species Accumulation Curves (facetted by year, split by status)
# =============================================================================

sac_df <- readRDS(paste0("data/", park, "/tidy/", name, "_species-accumulation.rds"))

base_theme <- theme_bw(base_size = 13)

sac_sample <- ggplot(
  sac_df %>%
    filter(curve == "Sample-based detection/non-detection"),
  aes(
    x        = x,
    y        = richness,
    colour   = status,
    fill     = status,
    linetype = Year
  )
) +
  geom_ribbon(
    aes(ymin = richness - sd, ymax = richness + sd),
    alpha  = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    values = setNames(
      c("22", "solid"),
      as.character(years)
    )
  ) +
  scale_colour_manual(
    name   = "Status",
    values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")
  ) +
  scale_fill_manual(
    name   = "Status",
    values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")
  ) +
  labs(x = "Number of BRUV deployments", y = "Species richness") +
  base_theme

sac_sample

ggsave(
  paste0("plots/", park, "/fish/", name, "_SAC-sample.png"),
  plot   = sac_sample,
  height = 4,
  width  = 7,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

saveRDS(sac_sample, paste0("plots/", park, "/fish/", name, "_SAC-sample.rds"))

sac_individual <- ggplot(
  sac_df %>%
    filter(curve == "Individual-based rarefaction"),
  aes(
    x        = x,
    y        = richness,
    colour   = status,
    fill     = status,
    linetype = Year
  )
) +
  geom_ribbon(
    aes(ymin = richness - sd, ymax = richness + sd),
    alpha  = 0.18,
    colour = NA
  ) +
  geom_line(linewidth = 1.2) +
  scale_linetype_manual(
    values = setNames(
      c("22", "solid"),
      as.character(years)
    )
  ) +
  scale_colour_manual(
    name   = "Status",
    values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")
  ) +
  scale_fill_manual(
    name   = "Status",
    values = c("No-Take" = "#7bbc63", "Fished" = "#b9e6fb")
  ) +
  labs(x = "Cumulative MaxN individuals", y = "Species richness") +
  base_theme

sac_plot <- sac_sample / sac_individual +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(legend.position = "right", plot.tag = element_text(face = "bold", size = 14))

sac_plot

ggsave(
  paste0("plots/", park, "/fish/", name, "_SAC-faceted.png"),
  plot   = sac_plot,
  height = 8,
  width  = 7,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

# =============================================================================
# PART 5: Top 10 abundance bar plot (facetted by year)
# =============================================================================

maxn <- readRDS(paste0("data/", park, "/raw/_count-with-zeros.RDS")) %>%
  semi_join(metadata_amp, by = c("campaignid", "sample")) %>%
  mutate(year = year(date_time)) %>%
  left_join(sti, by = c("family", "genus", "species")) %>%
  select(year, sample, scientific_name, family, genus, species, count, rls_thermal_niche) %>%
  glimpse()

length(unique(maxn$sample)) * length(unique(maxn$scientific_name))

# Mean MaxN per species — top 10 per year
maxn.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(sti)

# Species appearing in only one year's top 10 — bolded
spy1 <- maxn.10 %>% filter(year == years[1]) %>% pull(scientific)
spy2 <- maxn.10 %>% filter(year == years[2]) %>% pull(scientific)
unique_species <- union(setdiff(spy1, spy2), setdiff(spy2, spy1))

bar_maxn <- ggplot(
  maxn.10 %>%
    mutate(scientific_label = if_else(
      scientific %in% unique_species,
      paste0("**", scientific, "**"),
      scientific
    )),
  aes(x = reorder_within(scientific_label, maxn, year), y = maxn)
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(aes(ymin = pmax(maxn - se, 0), ymax = maxn + se), width = 0.2) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  labs(
    x = "Species",
    y = expression(Average~abundance~(MaxN~per~BRUV))
  ) +
  theme_bw() +
  theme_collapse +
  theme(
    axis.text.y = element_markdown(),
    panel.grid.major.x = element_line(color = "grey90")
  )

bar_maxn

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.png"),
  plot   = bar_maxn,
  height = 4,
  width  = 9,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

saveRDS(bar_maxn, paste0("plots/", park, "/fish/", name, "_top_maxn_bar_plot.rds"))

# =============================================================================
# PART 6: CTI bar plot (facetted by year)
# =============================================================================

cti.10 <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(year, scientific) %>%
  summarise(
    maxn = mean(count, na.rm = TRUE),
    se   = sd(count, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  ) %>%
  left_join(sti) %>%
  filter(!is.na(rls_thermal_niche)) %>%
  group_by(year) %>%
  slice_max(order_by = maxn, n = 10, with_ties = FALSE) %>%
  ungroup()

# Species appearing in only one year's top 10 — bolded
sp.cti.y1 <- cti.10 %>% filter(year == years[1]) %>% pull(scientific)
sp.cti.y2 <- cti.10 %>% filter(year == years[2]) %>% pull(scientific)

unique_species_cti <- union(setdiff(sp.cti.y1, sp.cti.y2), setdiff(sp.cti.y2, sp.cti.y1))

log1p10_trans <- trans_new(
  name      = "log10p1",
  transform = function(x) log10(x + 1),
  inverse   = function(x) 10^x - 1
)

mid_niche    <- median(cti.10$rls_thermal_niche, na.rm = TRUE)
niche_limits <- range(cti.10$rls_thermal_niche, na.rm = TRUE)

bar_cti <- ggplot(
  cti.10 %>%
    mutate(
      scientific_label = if_else(
        scientific %in% unique_species_cti,
        paste0("**", scientific, "**"),
        scientific
      ),
      niche_lab = scales::number(rls_thermal_niche, accuracy = 0.01)
    ),
  aes(
    x    = reorder_within(scientific_label, rls_thermal_niche, year),
    y    = maxn,
    fill = rls_thermal_niche
  )
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = pmax(maxn - se, 0), ymax = maxn + se),
    width = 0.2
  ) +
  geom_text(aes(y = 23, label = niche_lab), hjust = 0, size = 3) +
  coord_flip(clip = "off") +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(
    trans   = log1p10_trans,
    expand  = expansion(mult = c(0, 0.15)),
    breaks  = c(0, 5, 10, 20),
    labels  = scales::label_number()
  ) +
  scale_fill_gradientn(
    colours  = c("#2b83ba", "grey", "#d7191c"),
    values   = scales::rescale(c(niche_limits[1], mid_niche, niche_limits[2])),
    limits   = niche_limits,
    na.value = "grey80"
  ) +
  guides(fill = "none") +
  labs(
    x = "Species",
    y = expression(Log[10]~(Average~abundance~+~1))
  ) +
  theme_bw() +
  theme_collapse +
  theme(
    axis.text.y = element_markdown(),
    panel.grid.major.x = element_line(color = "grey90")
  )

bar_cti

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.png"),
  plot   = bar_cti,
  height = 4,
  width  = 9,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

saveRDS(bar_cti, paste0("plots/", park, "/fish/", name, "_top_maxn_cti_bar_plot.rds"))

# =============================================================================
# PART 7: B20 bar plot (top 10 per year)
# =============================================================================
b20 <- readRDS(paste0("data/", park, "/tidy/", name, "_b20-species_amp.rds"))

b20.10 <- b20 %>%
  filter(status == "Combined") %>%
  group_by(year) %>%
  slice_max(order_by = b20, n = 10, with_ties = FALSE) %>%
  ungroup()

# Species appearing in only one year's top 10 — bolded
spy1_b20 <- b20.10 %>% filter(year == years[1]) %>% pull(scientific_name)
spy2_b20 <- b20.10 %>% filter(year == years[2]) %>% pull(scientific_name)
unique_species_b20 <- union(setdiff(spy1_b20, spy2_b20), setdiff(spy2_b20, spy1_b20))

bar_b20 <- ggplot(
  b20.10 %>%
    mutate(scientific_label = if_else(
      scientific_name %in% unique_species_b20,
      paste0("**", scientific_name, "**"),
      scientific_name
    )),
  aes(x = reorder_within(scientific_label, b20, year), y = b20)
) +
  geom_col(colour = "black", linewidth = 0.25) +
  geom_errorbar(
    aes(ymin = pmax(b20 - se, 0), ymax = b20 + se),
    width = 0.2
  ) +
  coord_flip() +
  facet_wrap(~year, scales = "free_y") +
  scale_x_reordered() +
  scale_y_continuous(
    trans   = scales::pseudo_log_trans(base = 10),
    breaks  = c(0, 1, 10, 100, 1000),
    labels  = scales::label_number()
  ) +
  labs(
    x = "Species",
    y = expression(Average~biomass~(B20~per~BRUV))
  ) +
  theme_bw() +
  theme_collapse +
  theme(
    axis.text.y = element_markdown(),
    panel.grid.major.x = element_line(color = "grey90")
  )

bar_b20

ggsave(
  paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot.png"),
  plot   = bar_b20,
  height = 4,
  width  = 9,
  dpi    = 600,
  units  = "in",
  bg     = "white"
)

saveRDS(bar_b20, paste0("plots/", park, "/fish/", name, "_top_b20_bar_plot.rds"))

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
  prediction_limits   = prediction_limits
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

