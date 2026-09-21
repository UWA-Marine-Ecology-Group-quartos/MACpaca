###
# Project: NESP 5.6 - South-west Corner Report
# Task:    Survey-effort pie chart overlays (national + network level),
#          one set per method family (BRUV, UVC, ROV, drop camera, BOSS)
# Author:  Abbey Gibbons
# Date:    September 2026
#
# Outputs (plots/network/spatial/AMP_audit/):
#   national/  one pie per marine network
#   network/   one pie per marine park (or arm/split), for each of the 6 networks
#
# Data sources (two shapefiles, on purpose):
#   - CAPAD Marine 2024 (Australia-wide): national map, the 5 non-SWC network
#     maps, and all pie centroids.
#   - south-and-western-australia_marine-parks-all.shp (SA/WA only): SWC map
#     only. It is the only source of the state-park overlay and the Special
#     Purpose Zone (Mining Exclusion) stripe, so the other 5 networks have
#     neither.
#
# If the CSV changes, check:
#   - name_fixes (Section 2). CSV parks with no CAPAD match are printed and
#     written to unmatched_amp_names.csv; IOT misses raise a warning.
#   - Extents for the 5 non-SWC networks are auto-computed from each network's
#     bounding box + pad. IOT (Christmas + Cocos, ~900 km apart) needs a
#     larger pad (Section 9).
#   - Network names in the Section 9 loop come from the CSV, not hardcoded.

# ==============================================================================
# 0. SETUP
# ==============================================================================

library(tidyverse)
library(sf)
library(scatterpie)
library(ggnewscale)
library(ggpattern)
library(scales)
library(janitor)
library(ggforce)
library(patchwork)
sf_use_s2(TRUE)
data_dir <- "data/south-west network"
audit_root       <- "plots/network/spatial/AMP_audit"
national_out_dir <- file.path(audit_root, "national")
network_out_dir  <- file.path(audit_root, "network")

# ==============================================================================
# 1. LOAD SPATIAL DATA
# ==============================================================================
# ── Shared: Australia outline ────────────────────────────────────────────────
aus <- st_read(file.path(data_dir, "spatial/shapefiles/STE_2021_AUST_GDA2020.shp")) %>%
  st_make_valid()
# ── 1a. NATIONAL layer (CAPAD Marine 2024) - used for the national plot ONLY ─
capad_raw <- st_read(file.path(data_dir, "spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")) %>%
  janitor::clean_names() %>%
  st_make_valid() %>%
  dplyr::mutate(zone_type = stringr::str_replace_all(zone_type, "\\s*\\([^\\)]+\\)", ""))
name_col_candidates <- c("name", "resname", "res_name", "reserve_name",
                         "park_name", "amp_name")
name_col <- intersect(name_col_candidates, names(capad_raw))[1]
if (is.na(name_col)) {
  stop(
    "Couldn't find a reserve-name column in `capad_raw`. Columns available: ",
    paste(names(capad_raw), collapse = ", "),
    ". Add the correct one to `name_col_candidates` above and rerun."
  )
}
capad_raw$park_name_raw <- capad_raw[[name_col]]
amp_zone_code_map <- c(
  npz = "National Park Zone",
  hpz = "Habitat Protection Zone",
  hpr = "Habitat Protection Zone",
  hpl = "Habitat Protection Zone",
  muz = "Multiple Use Zone",
  ruz = "Recreational Use Zone",
  san = "Sanctuary Zone",
  spz = "Special Purpose Zone",
  spt = "Special Purpose Zone",
  spm = "Special Purpose Zone",
  spn = "Special Purpose Zone"
)
amp_zone_levels_national <- c(
  "Special Purpose Zone", "National Park Zone", "Habitat Protection Zone",
  "Recreational Use Zone", "Multiple Use Zone", "Sanctuary Zone"
)
amp_zone_colours_national <- c(
  "Special Purpose Zone"    = "#6daff4",
  "National Park Zone"      = "#7bbc63",
  "Habitat Protection Zone" = "#fff8a3",
  "Recreational Use Zone"   = "#ffb36b",
  "Multiple Use Zone"       = "#b9e6fb",
  "Sanctuary Zone"          = "#f7c0d8"
)
message("--- CAPAD diagnostic: zone codes with NO entry in amp_zone_code_map ---")
print(
  capad_raw %>% sf::st_drop_geometry() %>%
    dplyr::filter(epbc == "Commonwealth", type == "Australian Marine Park") %>%
    dplyr::mutate(zone_code = stringr::str_sub(res_number, -5, -3)) %>%
    dplyr::filter(!zone_code %in% names(amp_zone_code_map)) %>%
    dplyr::distinct(zone_code)
)
fed_mps_national <- capad_raw %>%
  dplyr::filter(epbc == "Commonwealth", type == "Australian Marine Park") %>%
  dplyr::mutate(zone_code = stringr::str_sub(res_number, -5, -3),
                zone_type = dplyr::recode(zone_code, !!!amp_zone_code_map,
                                          .default = NA_character_)) %>%
  dplyr::filter(!is.na(zone_type))
state_mps_national <- capad_raw %>%
  dplyr::filter(epbc == "State") %>%
  dplyr::mutate(sanctuary = "State Marine Park")
capad_commonwealth <- capad_raw %>%
  dplyr::filter(epbc == "Commonwealth", type == "Australian Marine Park")

# ── 1b. REGIONAL layer (south-west shapefile) - network-level plot ONLY ─────
capad_labels_src <- st_read(file.path(data_dir, "spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp"))
marine_parks <- st_read(file.path(data_dir, "spatial/shapefiles/south-and-western-australia_marine-parks-all.shp"))
marine_parks <- marine_parks %>%
  dplyr::mutate(
    zone = dplyr::if_else(
      zone == "Special Purpose Zone" & stringr::str_detect(zone_type, "Mining Exclusion"),
      "Special Purpose Zone (Mining Exclusion)",
      zone
    )
  )
amp_zone_levels <- c("National Park Zone", "Habitat Protection Zone",
                     "Multiple Use Zone", "Special Purpose Zone",
                     "Special Purpose Zone (Mining Exclusion)")
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  dplyr::mutate(zone = factor(zone, levels = amp_zone_levels),
                pattern_type = dplyr::if_else(
                  zone == "Special Purpose Zone (Mining Exclusion)", "stripe", "none"
                ))
amp_zone_colours <- marine_parks_amp %>%
  st_drop_geometry() %>%
  dplyr::distinct(zone, colour) %>%
  dplyr::filter(!is.na(zone)) %>%
  tibble::deframe()
amp_zone_colours <- amp_zone_colours[amp_zone_levels]
names(amp_zone_colours) <- amp_zone_levels
if (is.na(amp_zone_colours[["Special Purpose Zone (Mining Exclusion)"]])) {
  amp_zone_colours[["Special Purpose Zone (Mining Exclusion)"]] <-
    amp_zone_colours[["Special Purpose Zone"]]
}
marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
  dplyr::mutate(
    zone = dplyr::case_when(
      zone == "Reef Observation Area"   ~ "Sanctuary Zone",
      zone == "National Park Zone"      ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE                              ~ zone
    ),
    colour = dplyr::case_when(
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      zone == "Sanctuary Zone"               ~ "#bfd4a5",
      TRUE                                   ~ colour
    )
  )

# ==============================================================================
# 2. LOAD AND TIDY THE SURVEY-EFFORT SPREADSHEET
# ==============================================================================

survey_raw <- read_csv(file.path(data_dir, "amp_data_sheet_-_data.csv"))
survey <- survey_raw %>%
  rename(network = Network, amp = `Australian Marine Park`,
         platform = Platform, design = `Survey design standards*`,
         sites = Sites) %>%
  filter(!is.na(network), network != "", platform != "", design != "") %>%
  mutate(sites_n = purrr::map_dbl(
    sites, ~ sum(as.numeric(str_split(.x, ",\\s*")[[1]]), na.rm = TRUE)
  )) %>%
  select(network, amp, platform, design, sites_n)
clean_amp_name <- function(x) {
  x %>%
    str_remove(" \\(eastern arm\\)| \\(western arm\\)| \\(offshore\\)| \\(on shelf\\)") %>%
    str_remove(" Marine Park$")
}
name_fixes <- c(
  "Western Erye"        = "Western Eyre",
  "Gulf of Carpenteria" = "Gulf of Carpentaria",
  "Canarvon Canyon"     = "Carnarvon Canyon",
  "Carter Island"       = "Cartier Island",
  "Solitary"            = "Solitary Islands"
)
survey <- survey %>%
  mutate(amp_group = amp,
         amp_clean = clean_amp_name(amp),
         amp_clean = dplyr::recode(amp_clean, !!!name_fixes))
capad_commonwealth <- capad_commonwealth %>%
  dplyr::mutate(amp_clean = clean_amp_name(park_name_raw))
marine_parks_amp <- marine_parks_amp %>%
  dplyr::mutate(amp_clean = clean_amp_name(name))
fed_mps_national <- fed_mps_national %>%
  dplyr::mutate(amp_clean = clean_amp_name(park_name_raw))

# ==============================================================================
# 3. METHOD GROUPINGS (per Tim's comment on Image 2)
# ==============================================================================

PREF_COLOUR <- "#FFA500"
REP_COLOUR  <- "#d7191c"
method_groups <- list(

  bruv = list(
    platforms = c("stereo-BRUV", "mono-BRUV"),
    palette = c("stereo-BRUV.Preferential"   = "#FFA500",
                "stereo-BRUV.Representative" = "#d7191c",
                "mono-BRUV.Preferential"     = "#FFD590",
                "mono-BRUV.Representative"   = "#f4a9a0"),
    label = "BRUV (stereo + mono)"
  ),

  uvc = list(
    platforms = c("RLS-UVC", "AIMS-UVC"),
    palette = c("RLS-UVC.Preferential"    = "#FFA500",
                "RLS-UVC.Representative"  = "#d7191c",
                "AIMS-UVC.Preferential"   = "#FFD590",
                "AIMS-UVC.Representative" = "#f4a9a0"),
    label = "UVC (RLS vs AIMS)",
    network_size = list(min_r = 0.12, max_r = 0.55)
  ),

  rov = list(
    platforms = c("stereo-ROV", "mono-ROV"),
    palette = c("stereo-ROV.Preferential"   = "#FFA500",
                "stereo-ROV.Representative" = "#d7191c",
                "mono-ROV.Preferential"     = "#FFD590",
                "mono-ROV.Representative"   = "#f4a9a0"),
    label = "ROV (stereo + mono)",
    network_size = list(min_r = 0.12, max_r = 0.55)
  ),
  drop_camera = list(
    platforms = c("drop camera (downward facing)"),
    palette = c("drop camera (downward facing).Preferential"   = "#FFA500",
                "drop camera (downward facing).Representative" = "#d7191c"),
    label = "Drop camera (mono, downward)"
  ),

  boss = list(
    platforms = c("stereo-BOSS"),
    palette = c("stereo-BOSS.Preferential"   = PREF_COLOUR,
                "stereo-BOSS.Representative" = REP_COLOUR),
    label = "BOSS / horizontal drop camera"
  )
)

# ==============================================================================
# 4. BUILD PIE DATA FOR ONE METHOD GROUP AT A GIVEN SPATIAL LEVEL
# ==============================================================================

build_pie_data <- function(group_name, level = c("network", "amp")) {
  level <- match.arg(level)
  grp <- method_groups[[group_name]]
  group_col <- if (level == "network") "network" else "amp_group"

  wide <- survey %>%
    filter(platform %in% grp$platforms) %>%
    mutate(category = paste(platform, design, sep = ".")) %>%
    group_by(across(all_of(group_col)), category) %>%
    summarise(sites_n = sum(sites_n), .groups = "drop") %>%
    pivot_wider(names_from = category, values_from = sites_n, values_fill = 0)

  missing_cols <- setdiff(names(grp$palette), names(wide))
  wide[missing_cols] <- 0

  wide %>%
    mutate(total = rowSums(across(all_of(names(grp$palette)))))
}

# ==============================================================================
# 5. SPATIAL CENTRES FOR THE PIES
# ==============================================================================

network_lookup <- survey %>% distinct(network, amp_clean)
match_diag <- network_lookup %>%
  mutate(matched = amp_clean %in% capad_commonwealth$amp_clean)
if (any(!match_diag$matched)) {
  unmatched <- match_diag %>% filter(!matched)
  message("--- National match check: these CSV parks had NO matching CAPAD polygon ---")
  print(unmatched)
  dir.create(national_out_dir, recursive = TRUE, showWarnings = FALSE)
  write_csv(unmatched, file.path(national_out_dir, "unmatched_amp_names.csv"))

  iot_unmatched <- unmatched %>% filter(str_detect(network, regex("Indian Ocean", ignore_case = TRUE)))
  if (nrow(iot_unmatched) > 0) {
    warning(
      "Indian Ocean Territories rows failed to match a CAPAD polygon and will ",
      "NOT appear on the national plot: ",
      paste(iot_unmatched$amp_clean, collapse = "; "),
      ". Check spelling against `park_name_raw` in `capad_commonwealth` and add ",
      "a fix to `name_fixes` in Section 2.",
      call. = FALSE
    )
  }
}
network_centres <- purrr::map_dfr(unique(network_lookup$network), function(net) {
  amps_in_net <- network_lookup$amp_clean[network_lookup$network == net]
  matched <- capad_commonwealth %>% filter(amp_clean %in% amps_in_net)
  if (nrow(matched) == 0) return(tibble(network = net, X = NA_real_, Y = NA_real_))
  geom <- matched %>% st_union() %>% st_centroid() %>% st_coordinates()
  tibble(network = net, X = geom[1, "X"], Y = geom[1, "Y"])
}) %>%
  filter(!is.na(X))
amp_centres <- capad_commonwealth %>%
  st_drop_geometry() %>%
  distinct(amp_clean) %>%
  left_join(
    capad_commonwealth %>% group_by(amp_clean) %>% summarise(geometry = st_union(geometry)) %>%
      st_centroid() %>% mutate(X = st_coordinates(.)[, 1], Y = st_coordinates(.)[, 2]) %>%
      st_drop_geometry(),
    by = "amp_clean"
  )
manual_centre_overrides <- tribble(
  ~amp_group,                                    ~X,     ~Y,
  "South-west Corner Marine Park (western arm)", 113.6, -33.6,
  "South-west Corner Marine Park (eastern arm)", 120.6, -35.2,
  "South-west Corner Marine Park (offshore)",    117.5, -37.5
)
amp_group_centres <- survey %>%
  distinct(amp_group, amp_clean) %>%
  left_join(amp_centres, by = "amp_clean") %>%
  rows_update(manual_centre_overrides, by = "amp_group")
amp_group_centres <- amp_group_centres %>%
  group_by(X, Y) %>%
  mutate(.dupe_n = n(), .dupe_i = row_number()) %>%
  ungroup() %>%
  mutate(
    .jitter_deg = 0.15,
    X = if_else(.dupe_n > 1, X + .jitter_deg * cos(2 * pi * (.dupe_i - 1) / .dupe_n), X),
    Y = if_else(.dupe_n > 1, Y + .jitter_deg * sin(2 * pi * (.dupe_i - 1) / .dupe_n), Y)
  ) %>%
  select(-.dupe_n, -.dupe_i, -.jitter_deg)

# ==============================================================================
# 6. PIE LAYER + SIZE LEGEND (shared by all plot levels)
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

fit_dims <- function(xlim, ylim, target_width = NULL, target_height = NULL,
                     min_dim = 4, max_dim = 13, legend_allowance = 2.2) {
  lat_mid <- mean(ylim)
  aspect  <- (diff(xlim) * cos(lat_mid * pi / 180)) / diff(ylim)  # width/height

  if (!is.null(target_height)) {
    height <- target_height
    width  <- height * aspect + legend_allowance
  } else {
    width  <- target_width %||% 10
    height <- (width - legend_allowance) / aspect
  }

  list(width  = pmin(max_dim, pmax(min_dim, width)),
       height = pmin(max_dim, pmax(min_dim, height)))
}

scale_pie_radii <- function(pie_data, min_r = 0.1, max_r = 1, overlap_margin = 0.92) {

  pie_data <- pie_data %>% filter(total > 0)
  n <- nrow(pie_data)
  max_total <- if (n > 0) max(pie_data$total, na.rm = TRUE) else 0

  pie_data$r <- if (n > 0 && max_total > 0) {
    scales::rescale(sqrt(pie_data$total), to = c(min_r, max_r), from = c(0, sqrt(max_total)))
  } else {
    rep(min_r, n)
  }

  shrink_factor <- 1
  if (n >= 2) {
    d <- as.matrix(dist(pie_data[, c("X", "Y")]))
    diag(d) <- NA
    r_sum <- outer(pie_data$r, pie_data$r, "+")
    overlap_ratio <- r_sum / (d * overlap_margin)
    max_ratio <- suppressWarnings(max(overlap_ratio, na.rm = TRUE))
    if (is.finite(max_ratio) && max_ratio > 1) {
      shrink_factor <- max_ratio
      pie_data$r <- pie_data$r / shrink_factor
    }
  }

  list(data = pie_data, min_r = min_r, max_r = max_r,
       max_total = max_total, shrink_factor = shrink_factor)
}

radius_for_totals <- function(totals, scale_info) {
  if (scale_info$max_total <= 0) return(rep(scale_info$min_r, length(totals)))
  r <- scales::rescale(sqrt(totals), to = c(scale_info$min_r, scale_info$max_r),
                       from = c(0, sqrt(scale_info$max_total)))
  r / scale_info$shrink_factor
}

pie_layer <- function(pie_data, palette) {
  list(
    ggnewscale::new_scale_fill(),
    scatterpie::geom_scatterpie(
      data      = pie_data,
      aes(x = X, y = Y, r = r),
      cols      = names(palette),
      colour    = "black",
      linewidth = 0.15,
      alpha     = 0.65
    ),
    scale_fill_manual(
      name   = "Survey design",
      values = palette,
      labels = function(breaks) gsub("\\.", " ", breaks)
    )
  )
}

# Draws the "100 / 200"-style reference circles directly IN the map's own
# coord_sf() panel, in the same degree units as the real pies' r - this is
# what makes them automatically true-to-scale. `anchor` picks which corner
# of xlim/ylim it sits in - chosen per network in Section 9 to clear real
# park polygons/pies.

nice_ref_totals <- function(max_total, n = 3) {
  if (!is.finite(max_total) || max_total <= 0) return(numeric(0))
  mag    <- floor(log10(max_total))
  ladder <- sort(unique(as.vector(outer(c(1, 2, 5), 10^((mag - 3):(mag + 1))))))
  ladder <- ladder[ladder >= 1]                    # whole sites only
  top    <- max(which(ladder <= max_total))
  ladder[pmax(1, (top - n + 1)):top]
}
pie_size_legend_layer <- function(scale_info, xlim, ylim, ref_totals = NULL,
                                  n_ref = 3,                       # <- new
                                  unit_label = "sites",
                                  anchor = c("bottomleft", "bottomright",
                                             "topleft", "topright"),
                                  pad_frac = 0.04) {
  anchor <- match.arg(anchor)
  if (scale_info$max_total <= 0) return(list())

  if (is.null(ref_totals)) {
    ref_totals <- nice_ref_totals(scale_info$max_total, n = n_ref)
  }

  ref_r <- radius_for_totals(ref_totals, scale_info)   # same units as the real pie r
  gap   <- max(ref_r) * 2.4
  x_pad <- diff(xlim) * pad_frac
  y_pad <- diff(ylim) * pad_frac
  label_gap <- diff(ylim) * 0.012
  title_gap <- diff(ylim) * 0.015

  x0 <- switch(anchor,
               bottomleft = , topleft  = xlim[1] + x_pad + max(ref_r),
               bottomright = , topright = xlim[2] - x_pad - max(ref_r) -
                 gap * (length(ref_totals) - 1))
  y0 <- switch(anchor,
               bottomleft = , bottomright = ylim[1] + y_pad + max(ref_r) * 1.6,
               topleft = , topright        = ylim[2] - y_pad - max(ref_r) * 1.6)

  key <- tibble::tibble(
    total = ref_totals,
    r     = ref_r,
    x     = x0 + seq(0, by = gap, length.out = length(ref_totals)),
    y     = y0
  )

  list(
    ggforce::geom_circle(data = key, aes(x0 = x, y0 = y, r = r),
                         fill = "grey70", colour = "black", linewidth = 0.2,
                         inherit.aes = FALSE),
    geom_text(data = key, aes(x = x, y = y - r - label_gap, label = total),
              size = 3, inherit.aes = FALSE),
    annotate("text", x = x0 - ref_r[1],
             y = y0 + max(ref_r) + title_gap,
             label = paste0("Pie size \u2248 ", unit_label),
             hjust = 0, vjust = 0, size = 3)
  )
}

safe_pie_layer <- function(pie_data, palette, min_r, max_r) {
  tryCatch({
    scaled <- scale_pie_radii(pie_data, min_r = min_r, max_r = max_r)
    list(layer = pie_layer(scaled$data, palette), scaled = scaled)
  }, error = function(e) {
    message("  -> Could not build pie layer (", conditionMessage(e),
            ") - drawing map without pies.")
    list(
      layer  = list(),
      scaled = list(data = tibble::tibble(), min_r = min_r, max_r = max_r,
                    max_total = 0, shrink_factor = 1)
    )
  })
}

# ==============================================================================
# 7. NATIONAL PLOT (Image 1 + 2)
# ==============================================================================

make_national_pie_map <- function(group_name, save_name = NULL,
                                  min_r = 0.4, max_r = 2.3,
                                  size_anchor = "bottomleft",
                                  width = 11, height = 6) {

  grp <- method_groups[[group_name]]
  pie_data <- build_pie_data(group_name, level = "network") %>%
    left_join(network_centres, by = "network") %>%
    filter(!is.na(X))

  pr <- safe_pie_layer(pie_data, grp$palette, min_r = min_r, max_r = max_r)

  nat_xlim <- c(90, 175)
  nat_ylim <- c(-60, -5)

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = state_mps_national, aes(fill = sanctuary), colour = NA) +
    scale_fill_manual(values = c("State Marine Park" = "grey80"),
                      name = "State Marine Parks",
                      guide = guide_legend(order = 2)) +
    ggnewscale::new_scale_fill() +
    geom_sf(data = fed_mps_national, aes(fill = zone_type), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = amp_zone_colours_national, name = "Australian Marine Parks",
                      guide = guide_legend(order = 1)) +
    pr$layer +
    pie_size_legend_layer(pr$scaled, xlim = nat_xlim, ylim = nat_ylim, anchor = size_anchor) +
    coord_sf(xlim = nat_xlim, ylim = nat_ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = "left",
          legend.justification = "top",
          legend.box = "vertical",
          legend.title = element_text(size = 11),
          legend.text  = element_text(size = 9),
          plot.background = element_rect(fill = "white", colour = NA))

  if (!is.null(save_name)) {
    dir.create(national_out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(national_out_dir, paste0(save_name, ".png"))
    tryCatch({
      ggsave(out_path, p, dpi = 600, width = width, height = height, bg = "white")
    }, error = function(e) {
      if (file.exists(out_path)) file.remove(out_path)
      stop(e)
    })
  }
  p
}

# ==============================================================================
# 8. NETWORK-LEVEL PLOT (Image 3) - one pie per marine park / arm / split
# ==============================================================================

make_network_pie_map <- function(group_name, network_name, save_name = NULL,
                                 xlim, ylim, min_r = 0.2, max_r = 1.5,
                                 size_anchor = "bottomleft",
                                 width = 12.5, height = 6.5) {

  grp <- method_groups[[group_name]]
  amps_in_network <- network_lookup$amp_clean[network_lookup$network == network_name]

  pie_data <- build_pie_data(group_name, level = "amp") %>%
    inner_join(amp_group_centres, by = "amp_group") %>%
    filter(amp_clean %in% amps_in_network, !is.na(X))

  pr <- safe_pie_layer(pie_data, grp$palette, min_r = min_r, max_r = max_r)

  net_amp   <- marine_parks_amp   %>% filter(name %in% amps_in_network)
  net_state <- marine_parks_state %>% filter(name %in% amps_in_network)

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf_pattern(data = net_amp,
                    aes(fill = zone, pattern = pattern_type),
                    colour = NA, alpha = 0.9,
                    pattern_fill = "white", pattern_colour = "white",
                    pattern_density = 0.15, pattern_spacing = 0.01,
                    pattern_angle = 45, pattern_size = 0.2,
                    key_glyph = ggpattern::draw_key_polygon_pattern) +
    scale_pattern_manual(values = c(none = "none", stripe = "stripe"), guide = "none") +
    scale_fill_manual(name = "Australian Marine Parks", values = amp_zone_colours,
                      guide = guide_legend(override.aes = list(
                        pattern = ifelse(amp_zone_levels ==
                                           "Special Purpose Zone (Mining Exclusion)",
                                         "stripe", "none")))) +
    new_scale_fill() +
    geom_sf(data = net_state, aes(fill = zone), colour = NA, alpha = 0.5) +
    scale_fill_manual(name = "State Marine Parks",
                      values = with(net_state, setNames(colour, zone))) +
    pr$layer +
    pie_size_legend_layer(pr$scaled, xlim = xlim, ylim = ylim, anchor = size_anchor) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +

    theme(panel.grid = element_blank(),

          axis.title = element_blank(),

          legend.position = "left",

          legend.justification = "top",

          legend.title = element_text(size = 11),

          legend.text  = element_text(size = 9),

          plot.background = element_rect(fill = "white", colour = NA))


  if (!is.null(save_name)) {
    dir.create(network_out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(network_out_dir, paste0(save_name, ".png"))
    tryCatch({
      ggsave(out_path, p, dpi = 600, width = width, height = height, bg = "white")
    }, error = function(e) {
      if (file.exists(out_path)) file.remove(out_path)
      stop(e)
    })
  }
  p
}

# ==============================================================================
# 8b. NETWORK-LEVEL PLOT, NATIONAL VERSION - other 5 networks
# ==============================================================================

make_network_pie_map_national <- function(group_name, network_name, save_name = NULL,
                                          pad = 3, min_r = 0.25, max_r = 2,
                                          size_anchor = "bottomleft",
                                          width = NULL, height = NULL) {

  grp <- method_groups[[group_name]]
  amps_in_network <- network_lookup$amp_clean[network_lookup$network == network_name]

  net_amp <- fed_mps_national %>% dplyr::filter(amp_clean %in% amps_in_network)

  if (nrow(net_amp) == 0) {
    message("Skipping '", network_name, "' (", group_name,
            "): no matching polygons in fed_mps_national - check amp_clean",
            " spelling / name_fixes.")
    return(invisible(NULL))
  }

  pie_data <- build_pie_data(group_name, level = "amp") %>%
    inner_join(amp_group_centres, by = "amp_group") %>%
    filter(amp_clean %in% amps_in_network, !is.na(X))

  pr <- safe_pie_layer(pie_data, grp$palette, min_r = min_r, max_r = max_r)

  bbox <- sf::st_bbox(net_amp)
  xlim <- c(bbox[["xmin"]] - pad, bbox[["xmax"]] + pad)
  ylim <- c(bbox[["ymin"]] - pad, bbox[["ymax"]] + pad)

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = net_amp, aes(fill = zone_type), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = amp_zone_colours_national, name = "Australian Marine Parks") +
    pr$layer +
    pie_size_legend_layer(pr$scaled, xlim = xlim, ylim = ylim, anchor = size_anchor) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = "left",
          legend.justification = "top",
          legend.title = element_text(size = 11),
          legend.text  = element_text(size = 9),
          plot.background = element_rect(fill = "white", colour = NA))

  if (!is.null(save_name)) {
    dir.create(network_out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(network_out_dir, paste0(save_name, ".png"))
    dims <- fit_dims(xlim, ylim, target_width = width, target_height = height)
    tryCatch({
      ggsave(out_path, p, dpi = 600, width = dims$width, height = dims$height, bg = "white")
    }, error = function(e) {
      if (file.exists(out_path)) file.remove(out_path)
      stop(e)
    })
  }
}

# ==============================================================================
# 9. GENERATE THE SET
# ==============================================================================

for (g in names(method_groups)) {
  make_national_pie_map(g, save_name = paste0("national-", g, "-pies"))
}

# SWC only: size key anchored bottom-left.
for (g in names(method_groups)) {
  grp <- method_groups[[g]]
  size_args <- if (!is.null(grp$network_size)) grp$network_size else list()
  do.call(make_network_pie_map, c(
    list(
      group_name   = g,
      network_name = "South-west Marine Parks Network",
      xlim         = c(107, 140),
      ylim         = c(-41, -23),
      save_name    = paste0("swc-", g, "-pies")
    ),
    size_args
  ))
}

# Per-network overrides: anchor placement, extra bbox padding for the
# trickier extents (Indian Ocean Territories in particular), and a lower
# max_r everywhere to bring pie sizes down overall - not just the
# previously-crowded networks. Where a group ALSO has its own
# network_size (uvc, rov), the group-level setting still wins since it's
# more specific.
network_anchor_overrides <- list(
  "North Marine Parks Network"            = "bottomleft",
  "North-west Marine Parks Network"       = "topleft",
  "Coral Sea Marine Park"                 = "bottomleft",
  "Indian Ocean Territories Marine Parks" = "bottomleft",
  "Temperate East Marine Parks Network"   = "bottomright"
)

network_pad_overrides <- list(
  "Indian Ocean Territories Marine Parks" = 5
)

network_size_overrides <- list(
  "North Marine Parks Network"          = list(min_r = 0.12, max_r = 0.85),  # new
  "North-west Marine Parks Network"     = list(min_r = 0.15, max_r = 1.0),
  "Coral Sea Marine Park"               = list(min_r = 0.12, max_r = 0.9),
  "Temperate East Marine Parks Network" = list(min_r = 0.12, max_r = 0.9)
)

for (g in names(method_groups)) {
  other_networks <- setdiff(unique(network_lookup$network), "South-west Marine Parks Network")
  for (net in other_networks) {
    grp <- method_groups[[g]]
    default_size <- list(min_r = 0.2, max_r = 1.5)
    size_args <- modifyList(
      modifyList(default_size, network_size_overrides[[net]] %||% list()),
      grp$network_size %||% list()
    )
    anchor <- network_anchor_overrides[[net]] %||% "bottomleft"
    pad    <- network_pad_overrides[[net]] %||% 1.5

    tryCatch(
      do.call(make_network_pie_map_national, c(
        list(
          group_name   = g,
          network_name = net,
          save_name    = paste0(janitor::make_clean_names(net), "-", g, "-pies"),
          pad          = pad,
          size_anchor  = anchor
        ),
        size_args
      )),
      error = function(e) message("FAILED on network='", net, "', group='", g, "': ", conditionMessage(e))
    )
  }
}
# ==============================================================================
# End of script
# ==============================================================================
