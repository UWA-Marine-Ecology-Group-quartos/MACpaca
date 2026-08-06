###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Spatial covariates
# Task:    Format spatial covariates
# Author:  Annika Leunig and Claude Spencer
# Date:    June 2026
###

###
# TODO need to download the following from AODN portal
# Download the entire time period for just your park extent (if you do all of aus it will take a long time to download)
#
# AODN portal link: https://portal.aodn.org.au/search
# SST:  IMOS - Satellite Remote Sensing - SST - L3S - Single Sensor - 1 month - day and night time - Australia
# SLA: IMOS - OceanCurrent - Gridded sea level anomaly - Near real time
# Acidification: Ocean acidification historical reconstruction
# DHW can be downloaded from here: https://coastwatch.pfeg.noaa.gov/erddap/griddap/NOAA_DHW.html
#
# TODO move all those files to data/ 'the MP you are working on' /spatial/oceanography/
###

# Clear the environment
rm(list = ls())

# # Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# Load libraries
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyverse)
library(RNetCDF)
library(rerddap)

# TODO Set the extent of the study
e <- ext(115.04, 115.60, -33.67, -33.346)

# Oceanography/Pressures
# ── Sea Surface Temperature ───────────────────────────────────────────────────────────────────────
# SST data can sometimes be read strangely so this way prevents it from switching long and lat columns
nc_sst <- open.nc(paste0("data/", park, "/spatial/oceanography/SST.nc"))
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
                        crs = "EPSG:4326")

# Assign dates, crop and trim to study extent
names(rast_sst) <- as.character(dates_sst)
time(rast_sst)  <- dates_sst
rast_sst        <- terra::crop(rast_sst, e) %>% terra::trim()

# Check orientation - if upside down run: rast_sst <- terra::flip(rast_sst, "vertical")
plot(rast_sst[[1]])

winter_sst_ts <- rast_sst[[which(month(dates_sst) %in% c(7, 8, 9))]]

# Build monthly climatology
# Average across years for each month and convert from Kelvin to Celsius
sst_list <- list()
for (month in sort(unique(month(time(rast_sst))))) {
  monthly_rast <- subset(rast_sst, month(time(rast_sst)) == month) %>%
    mean(na.rm = TRUE) %>%
    app(fun = function(i) {i - 273.15})
  names(monthly_rast) <- month.abb[month]
  sst_list[[month.abb[month]]] <- monthly_rast
}
sst <- rast(sst_list)

# Save as rds
saveRDS(sst, paste0("data/", park, "/spatial/oceanography/", name, "_SST_raster.rds"))

# Build a monthly time-series summary
sst_tsdf <- terra::global(rast_sst, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_sst, fun = "sd", na.rm = T)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(sst = mean(mean, na.rm = T) - 273.15, # Convert kelvin to celsius
            sd = mean(sd, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(season = case_when(month %in% c("04", "05", "06") ~ "Autumn", # seasons are based on SST not euro seasons
                                   month %in% c("07", "08", "09") ~ "Winter",
                                   month %in% c("10", "11", "12") ~ "Spring",
                                   month %in% c("01", "02", "03") ~ "Summer")) %>%
  glimpse()

# Save time series
saveRDS(sst_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_SST_time-series.rds"))

# Check data
boxplot(sst_tsdf$sst ~ sst_tsdf$month)

# ── Sea Level anomaly ─────────────────────────────────────────────────────────────────────────────
# Load data
nc_sla <- open.nc(paste0("data/", park, "/spatial/oceanography/SLA.nc"),
                  write = TRUE)
print.nc(nc_sla)

# Convert time to dates
time_nc <- var.get.nc(nc_sla, 'TIME')
time_nc_sla <- utcal.nc("days since 1985-01-01 00:00:00 UTC", time_nc, type = "c")
dates_sla <- as.Date(time_nc_sla)
close.nc(nc_sla)

rast_sla <- terra::rast(paste0("data/", park, "/spatial/oceanography/SLA.nc"),
                        subds = "GSLA")
time(rast_sla) <- dates_sla
names(rast_sla) <- dates_sla
plot(rast_sla)
sla <- mean(rast_sla, na.rm = T)

# Build monthly climatology
# Average across years for each month and convert from Kelvin to Celsius
sla_list <- list()
for (month in sort(unique(month(time(rast_sla))))) {
  monthly_rast <- subset(rast_sla, month(time(rast_sla)) == month) %>%
    mean(na.rm = TRUE)
  names(monthly_rast) <- month.abb[month]
  sla_list[[month.abb[month]]] <- monthly_rast
}
sla <- rast(sla_list)

saveRDS(sla, paste0("data/", park, "/spatial/oceanography/", name, "_SLA_raster.rds"))

sla_tsdf <- terra::global(rast_sla, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_sla, fun = "sd", na.rm = T)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(sla = mean(mean, na.rm = T),
            sd = mean(sd, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
                                   month %in% c("06", "07", "08") ~ "Winter",
                                   month %in% c("09", "10", "11") ~ "Spring",
                                   month %in% c("12", "01", "02") ~ "Summer")) %>%
  glimpse()

#Save
saveRDS(sla_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_SLA_time-series.rds"))

# ── Degree Heating Weeks ──────────────────────────────────────────────────────────────────────────
# Load data
nc_dhw <- open.nc(paste0("data/", park, "/spatial/oceanography/DHW.nc"),
                  write = TRUE)
print.nc(nc_dhw)

# Convert time to dates
time_nc <- var.get.nc(nc_dhw, 'time')
time_nc_dhw <- utcal.nc("seconds since 1970-01-01T00:00:00Z", time_nc, type = "c")
dates_dhw <- as.Date(time_nc_dhw)
close.nc(nc_dhw)

rast_dhw <- terra::rast(paste0("data/", park, "/spatial/oceanography/DHW.nc"),
                        subds = "CRW_DHW")
time(rast_dhw) <- dates_dhw
names(rast_dhw) <- dates_dhw
plot(rast_dhw)

# Highest periods
dhw.2011 <- subset(rast_dhw, year(time(rast_dhw)) == 2011 & month(time(rast_dhw)) == 5) %>%
  mean(na.rm = T)
names(dhw.2011) <- "May 2011"
plot(dhw.2011)
dhw.2012 <- subset(rast_dhw, year(time(rast_dhw)) == 2012 & month(time(rast_dhw)) == 4) %>%
  mean(na.rm = T)
names(dhw.2012) <- "April 2012"
plot(dhw.2012)

dhw.2025 <- subset(rast_dhw, year(time(rast_dhw)) == 2025 & month(time(rast_dhw)) == 4) %>%
  mean(na.rm = T)
names(dhw.2025) <- "April 2025"
plot(dhw.2025)

dhw <- rast(list(dhw.2011, dhw.2025))
plot(dhw)

saveRDS(dhw, paste0("data/", park, "/spatial/oceanography/", name, "_DHW_raster.rds"))

# Create time series and save
dhw_tsdf <- terra::global(rast_dhw, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_dhw, fun = "sd", na.rm = T)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(dhw = mean(mean, na.rm = T),
            sd = mean(sd, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
                                   month %in% c("06", "07", "08") ~ "Winter",
                                   month %in% c("09", "10", "11") ~ "Spring",
                                   month %in% c("12", "01", "02") ~ "Summer")) %>%
  glimpse()

saveRDS(dhw_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_DHW_time-series.rds"))

# ── Acidification ─────────────────────────────────────────────────────────────────────────────────
nc_acid <- open.nc(paste0("data/", park, "/spatial/oceanography/Acidification.nc"),
                   write = TRUE)
print.nc(nc_acid)

# Convert time to dates
time_nc <- var.get.nc(nc_acid, 'TIME')
time_nc_acid <- utcal.nc("months since 1800-01-01 00:00:00", time_nc, type = "c")
dates_acid <- as.Date(time_nc_acid)
close.nc(nc_acid)

rast_acid <- terra::rast(paste0("data/", park, "/spatial/oceanography/Acidification.nc"),
                         subds = "pH_T")
time(rast_acid) <- dates_acid
names(rast_acid) <- dates_acid
plot(rast_acid)

# Create time series
acid_tsdf <- terra::global(rast_acid, fun = "mean", na.rm = T) %>%
  tibble::rownames_to_column() %>%
  cbind(terra::global(rast_acid, fun = "sd", na.rm = T)) %>%
  tidyr::separate(rowname, into = c("year", "month", "day"), sep = "-") %>%
  dplyr::group_by(year, month) %>%
  summarise(acidification = mean(mean, na.rm = T),
            sd = mean(sd, na.rm = T)) %>%
  ungroup() %>%
  dplyr::mutate(season = case_when(month %in% c("03", "04", "05") ~ "Autumn",
                                   month %in% c("06", "07", "08") ~ "Winter",
                                   month %in% c("09", "10", "11") ~ "Spring",
                                   month %in% c("12", "01", "02") ~ "Summer")) %>%
  glimpse()

# Save
saveRDS(acid_tsdf, paste0("data/", park, "/spatial/oceanography/", name, "_Acidification_time-series.rds"))

