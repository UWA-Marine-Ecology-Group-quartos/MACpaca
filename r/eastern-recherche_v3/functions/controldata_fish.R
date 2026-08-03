controldata_fish <- function(dat, year, amp_abbrv, state_abbrv) {

  marine_parks <- st_read("data/south-west network/spatial/shapefiles/western-australia_marine-parks-all.shp") %>%
    CheckEM::clean_names() %>%
    dplyr::mutate(zone_new = case_when(
      str_detect(zone, "Other State Marine Park Zone")  ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Habitat Protection Zone") & str_detect(epbc, "State")         ~ paste(state_abbrv, "HPZ"),
      str_detect(zone, "Habitat Protection Zone") & str_detect(epbc, "Commonwealth")  ~ paste(amp_abbrv,  "HPZ"),
      str_detect(zone, "Sanctuary Zone")                                                ~ paste(state_abbrv, "SZ (IUCN II)"),
      str_detect(zone, "National Park Zone")                                            ~ paste(amp_abbrv,  "NPZ (IUCN II)"),
      str_detect(zone, "Special Purpose Zone") & str_detect(epbc, "State")             ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Special Purpose Zone") & str_detect(epbc, "Commonwealth")      ~ paste(amp_abbrv,  "other zones"),
      str_detect(zone, "Multiple Use Zone") & str_detect(epbc, "State")                ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Multiple Use Zone") & str_detect(epbc, "Commonwealth")         ~ paste(amp_abbrv,  "other zones"),
      str_detect(zone, "Recreational Use Zone") & str_detect(epbc, "State")            ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Recreational Use Zone") & str_detect(epbc, "Commonwealth")     ~ paste(amp_abbrv,  "other zones"),
      str_detect(zone, "General Use Zone")                                              ~ paste(state_abbrv, "other zones"),
      str_detect(zone, "Reef Observation Area")                                         ~ paste(state_abbrv, "other zones"),
      TRUE ~ NA_character_
    )) %>%
    dplyr::mutate(status = ifelse(str_detect(zone_new, "SZ|NPZ"), "No-Take", "Fished"))

  preds <- readRDS(paste0("data/", park, "/spatial/rasters/",
                          name, "_bathymetry-derivatives.rds")) %>%
    crop(dat)

  tempdat_v <- terra::vect(as.data.frame(dat, xy = TRUE), geom = c("x", "y"), crs = "epsg:4326")
  tempdat <- cbind(
    as.data.frame(dat, xy = TRUE),
    terra::extract(preds[[1]], tempdat_v, ID = FALSE)
  )

  depth_qs <- c(-2000, -200, -70, -30, 0)
  class_values <- 4:1
  reclass_matrix <- cbind(depth_qs[-length(depth_qs)], depth_qs[-1], class_values)

  edc <- classify(preds$geoscience_depth, rcl = reclass_matrix) %>%
    as.polygons() %>%
    st_as_sf()

  areas <- st_intersection(edc, marine_parks) %>%
    dplyr::mutate(area = st_area(.)) %>%
    dplyr::filter(area > units::set_units(625000, "m^2")) %>%
    dplyr::mutate(
      depth_contour = case_when(
        geoscience_depth == 1 ~ "shallow",
        geoscience_depth == 2 ~ "mesophotic",
        geoscience_depth == 3 ~ "rariphotic",
        geoscience_depth == 4 ~ "deep"
      ),
      filter = "no"
    ) %>%
    dplyr::select(zone, depth_contour, filter) %>%
    as.data.frame() %>%
    dplyr::select(-geometry)

  areas_shallow <- dplyr::filter(areas, depth_contour %in% "shallow") %>%
    dplyr::distinct(zone, filter, .keep_all = TRUE)

  areas_meso <- dplyr::filter(areas, depth_contour %in% "mesophotic") %>%
    dplyr::distinct(zone, filter, .keep_all = TRUE)

  areas_rari <- dplyr::filter(areas, depth_contour %in% "rariphotic") %>%
    dplyr::distinct(zone, filter, .keep_all = TRUE)

  replacement_se <- c(
    "cti_se"       = "p_cti.se.fit",
    "richness_se"  = "p_richness.se.fit",
    "abundance_se" = "p_abundance.se.fit",
    "b20_se"       = "p_b20.se.fit"
  )

  replacement_mean <- c(
    "cti"       = "p_cti.fit",
    "richness"  = "p_richness.fit",
    "abundance" = "p_abundance.fit",
    "b20"       = "p_b20.fit"
  )

  out <- list(shallow = NULL, meso = NULL, rari = NULL)

  # SHALLOW (0-30 m)
  if (any(tempdat$geoscience_depth < 0 & tempdat$geoscience_depth > -30, na.rm = TRUE)) {

    shallow <- preds[[1]] %>% clamp(upper = 0, lower = -30, values = FALSE)
    dat.shallow <- dat %>% terra::mask(shallow)

    errors.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti_se", "richness_se", "abundance_se", "b20_se")))

    means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti", "richness", "abundance", "b20")))

    out$shallow <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.shallow, by = "ID") %>%
      dplyr::left_join(means.shallow,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_shallow,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, status, year,
                    cti, cti_se,
                    richness, richness_se,
                    abundance, abundance_se,
                    b20, b20_se) %>%
      dplyr::filter(!is.na(b20)) %>%
      dplyr::group_by(zone_new, status, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # MESOPHOTIC (30-70 m)
  if (any(tempdat$geoscience_depth < -30 & tempdat$geoscience_depth > -70, na.rm = TRUE)) {

    meso <- preds[[1]] %>% clamp(upper = -30, lower = -70, values = FALSE)
    dat.meso <- dat %>% terra::mask(meso)

    errors.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti_se", "richness_se", "abundance_se", "b20_se")))

    means.meso <- terra::extract(dat.meso, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti", "richness", "abundance", "b20")))

    out$meso <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.meso, by = "ID") %>%
      dplyr::left_join(means.meso,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_meso,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, status, year,
                    cti, cti_se,
                    richness, richness_se,
                    abundance, abundance_se,
                    b20, b20_se) %>%
      dplyr::filter(!is.na(b20)) %>%
      dplyr::group_by(zone_new, status, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  # RARIPHOTIC (70-200 m)
  if (any(tempdat$geoscience_depth < -70 & tempdat$geoscience_depth > -200, na.rm = TRUE)) {

    rari <- preds[[1]] %>% clamp(upper = -70, lower = -200, values = FALSE)
    dat.rari <- dat %>% terra::mask(rari)

    errors.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::ends_with(".se.fit"), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_se)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti_se", "richness_se", "abundance_se", "b20_se")))

    means.rari <- terra::extract(dat.rari, marine_parks) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::matches("^p_.*(?<!\\.se)\\.fit$", perl = TRUE), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(ID = as.character(ID), year = year) %>%
      dplyr::rename(dplyr::any_of(replacement_mean)) %>%
      dplyr::select(ID, year, dplyr::any_of(c("cti", "richness", "abundance", "b20")))

    out$rari <- as.data.frame(marine_parks) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename(ID = rowname) %>%
      dplyr::left_join(errors.rari, by = "ID") %>%
      dplyr::left_join(means.rari,  by = c("ID", "year")) %>%
      dplyr::left_join(areas_rari,  by = "zone") %>%
      dplyr::filter(filter == "no") %>%
      dplyr::select(zone_new, status, year,
                    cti, cti_se,
                    richness, richness_se,
                    abundance, abundance_se,
                    b20, b20_se) %>%
      dplyr::filter(!is.na(b20)) %>%
      dplyr::group_by(zone_new, status, year) %>%
      dplyr::summarise(
        dplyr::across(dplyr::everything(), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )
  }

  out
}
