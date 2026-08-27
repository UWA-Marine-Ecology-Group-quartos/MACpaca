pressure_data <- function() {
  acid_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
                            name, "_Acidification_time-series.rds")) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(!year %in% c("1870", "2013")) %>% # These 2 years have inaccurate averages as they are only on 6 months
    dplyr::group_by(year) %>%
    summarise(acidification = mean(acidification, na.rm = T), sd = mean(sd, na.rm = T)) %>%
    ungroup() %>%
    glimpse()
  assign("acid_ts", acid_ts, envir = .GlobalEnv)

  sla_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
                           name, "_SLA_time-series.rds")) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(season %in% c("Summer", "Winter")) %>%
    dplyr::group_by(year, season) %>%
    summarise(sla = mean(sla, na.rm = T), sd = mean(sd, na.rm = T)) %>%
    ungroup() %>%
    glimpse()
  assign("sla_ts", sla_ts, envir = .GlobalEnv)

  sst_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
                           name, "_SST_time-series.rds")) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(season %in% c("Summer", "Winter")) %>%
    dplyr::group_by(year, season) %>%
    summarise(sst = mean(sst, na.rm = T), sd = mean(sd, na.rm = T)) %>%
    ungroup() %>%
    glimpse()
  assign("sst_ts", sst_ts, envir = .GlobalEnv)

  dhw_ts <- readRDS(paste0("data/", park, "/spatial/oceanography/",
                           name, "_DHW_time-series.rds")) %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::group_by(year) %>%
    summarise(dhw = mean(dhw, na.rm = T), sd = mean(sd, na.rm = T)) %>%
    ungroup() %>%
    glimpse()
  assign("dhw_ts", dhw_ts, envir = .GlobalEnv)
}
