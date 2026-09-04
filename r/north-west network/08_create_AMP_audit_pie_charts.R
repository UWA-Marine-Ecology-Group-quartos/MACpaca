###
# Project: NESP 5.6 Project - North-west Network Report
# Task:    Survey-effort pie chart overlay (network level only, one pie per
#          marine park), adapted from the South-west Corner survey-effort
#          pie chart script.
# Note:    SELF-CONTAINED - loads its own spatial data, does not require
#          any other script to have been run first.
#
# Output:
#   North-west network map, one pie per marine PARK, repeated per survey
#   method family (BRUV, UVC, ROV, drop camera, BOSS). NO national-level
#   plot in this version (per request) - if you want the national overview
#   too, that logic (Sections 1a/7 of the SW script) can be dropped back in.
#
# ------------------------------------------------------------------------
# ADAPTATION NOTES (differences from the SW script this was built from):
#
# 1. SPATIAL DATA (Section 1): the SW script's network-level plot used a
#    SW-only regional shapefile ("south-and-western-australia_marine-
#    parks-all.shp"). For North-west there's an equivalent regional file -
#    "nw-network-australia_marine-parks-all.shp" - referenced in your NW
#    network-zones script. This version loads THAT file, filtered to the
#    same NW park name list your zones script uses (Commonwealth AMPs +
#    WA state marine parks/IPAs), rather than the national CAPAD layer,
#    so zone polygons/colours match what you've already validated for NW.
#
# 2. NO MINING-EXCLUSION STRIPE PATTERN: the SW script splits out a
#    "Special Purpose Zone (Mining Exclusion)" sub-zone and draws it with
#    a hatched pattern via ggpattern. Your NW network-zones script doesn't
#    build that distinction (no equivalent zone_type text handling), so
#    this version does NOT attempt it either - zones render as flat
#    colour only, taken directly from the shapefile's `colour` column,
#    same as your NW zones script does. CHECK whether any NW AMP zones
#    actually carry a Mining Exclusion sub-type in the raw data; if so,
#    the SW script's Section 8 pattern logic can be ported across.
#
# 3. INDIGENOUS PROTECTED AREAS: NW has two state-water IPAs (Yawuru,
#    Nyangumarta Warrarn) coded "Indigenous" rather than "State" in the
#    shapefile - carried over verbatim from your NW zones script's
#    zone/colour recode logic (Section 2 below).
#
# 4. NETWORK NAME STRING: `network_lookup`/`build_pie_data()` filter on
#    whatever string your CSV's `Network` column uses for this network.
#    I've assumed "North-west Marine Parks Network" (parallel to
#    "South-west Marine Parks Network" in the SW script's CSV) - CONFIRM
#    against the actual CSV value; if it's off, every centroid/pie for
#    this network will silently come back empty rather than erroring.
#
# 5. SURVEY CSV PATH: assumed to live alongside the other NW inputs at
#    `data/north-west network/amp_data_sheet_-_data.csv`. If this is
#    actually a single shared master file living elsewhere (e.g. still
#    under `data/south west network/`), update `survey_csv_path` below -
#    left as its own variable for exactly this reason.
#
# 6. PIE CENTROIDS (Section 5): sourced from the national CAPAD layer
#    (`capad_commonwealth`), same generic approach as the SW script's
#    Section 5b/5a - this isn't SW-specific, so it should resolve
#    correctly for NW parks without changes. No manual centroid overrides
#    are pre-added here (the SW script needed 3, for the SWC arms) -
#    add any if the match-diagnostic below flags NW parks with no
#    resolvable centroid, or if pies come out badly placed/overlapping
#    in a way `add_pies()`'s auto-shrink can't fix cleanly.
#
# 7. METHOD GROUPS / COLOURS (Section 3): copied unchanged from the SW
#    script's latest version (platform-hue palettes for bruv/uvc/rov/
#    drop_camera, red/green Preferential-Representative for boss) so
#    colour meaning stays consistent across network reports. Delete or
#    edit if that's not desired.
# ------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(scatterpie)
library(ggnewscale)
library(scales)
library(janitor)
library(ggforce)
library(patchwork)

sf_use_s2(TRUE)

data_dir       <- "data/north-west network"
survey_csv_path <- file.path(data_dir, "amp_data_sheet_-_data.csv")

audit_root      <- "plots/network/spatial/AMP_audit"
network_out_dir <- file.path(audit_root, "network")

NETWORK_NAME <- "North-west Marine Parks Network"

# ==============================================================================
# 1. LOAD SPATIAL DATA
# ==============================================================================

# ── Australia outline ────────────────────────────────────────────────────────
aus <- st_read(file.path(data_dir, "spatial/shapefiles/STE_2021_AUST_GDA2020.shp")) %>%
  st_make_valid()

# ── National CAPAD - used ONLY to source pie-placement centroids ────────────
# (generic across networks, same approach as the SW script - not filtered to
# NW here, just matched later via `amp_clean` per network)
capad_raw <- st_read(file.path(data_dir, "spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")) %>%
  janitor::clean_names() %>%
  st_make_valid()

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

capad_commonwealth <- capad_raw %>%
  dplyr::filter(epbc == "Commonwealth", type == "Australian Marine Park")

# ── NW regional layer - zone polygons for the network-level plot ────────────
# Same file + same park filter list as your NW network-zones script.
nw_park_names <- c(
  # Commonwealth AMPs (North-west Network)
  "Argo-Rowley Terrace", "Ashmore Reef", "Carnarvon Canyon", "Cartier Island",
  "Dampier", "Eighty Mile Beach", "Gascoyne", "Kimberley", "Mermaid Reef",
  "Montebello", "Ningaloo", "Roebuck", "Shark Bay",
  # WA state marine parks (Gascoyne-Pilbara-Kimberley)
  "Hamelin Pool", "Muiron Islands", "Barrow Island", "Thevenard Island",
  "Montebello Islands", "Yawuru Nagulagun / Roebuck Bay", "Yawuru",        # IPA
  "Nyangumarta Warrarn",                                                  # IPA
  "Bardi Jawi Gaarra", "North Kimberley", "Mayala",
  "Lalang-gaddam", "Rowley Shoals", "Scott Reef"
)

marine_parks <- st_read(file.path(data_dir, "spatial/shapefiles/nw-network-australia_marine-parks-all.shp")) %>%
  dplyr::filter(name %in% nw_park_names)

# Australian Marine Parks only (Commonwealth) - flat colour, no stripe pattern
# (see adaptation note 2 above)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# Indigenous Protected Areas (in state waters) - kept distinguishable from
# other state zones, same recode as your NW zones script
ipa_names <- c("Yawuru", "Nyangumarta Warrarn")

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% c("State", "Indigenous")) %>%  # Yawuru & Nyangumarta Warrarn are coded "Indigenous"
  dplyr::mutate(
    zone = dplyr::case_when(
      name %in% ipa_names              ~ "Indigenous Protected Area",
      zone == "Reef Observation Area"  ~ "Sanctuary Zone",
      zone == "National Park Zone"     ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE                             ~ zone
    ),
    colour = dplyr::case_when(
      name %in% ipa_names                    ~ "#FFD8A8",
      zone == "Other State Marine Park Zone" ~ "#f7d0dc",
      TRUE                                   ~ colour
    )
  )

# ==============================================================================
# 2. LOAD AND TIDY THE SURVEY-EFFORT SPREADSHEET
# ==============================================================================
survey_raw <- read_csv(survey_csv_path)

survey <- survey_raw %>%
  rename(network = Network, amp = `Australian Marine Park`,
         platform = Platform, design = `Survey design standards*`,
         sites = Sites) %>%
  filter(!is.na(network), network != "", platform != "", design != "") %>%
  # "Sites" is sometimes a comma-separated list e.g. "100, 200" (multiple
  # deployments/years) - sum to a single site count per row
  mutate(sites_n = purrr::map_dbl(
    sites, ~ sum(as.numeric(str_split(.x, ",\\s*")[[1]]), na.rm = TRUE)
  )) %>%
  select(network, amp, platform, design, sites_n)

# Strip AMP suffixes so names can be matched to shapefile polygons.
clean_amp_name <- function(x) {
  x %>%
    str_remove(" \\(eastern arm\\)| \\(western arm\\)| \\(offshore\\)") %>%
    str_remove(" Marine Park$")
}

# Known spelling mismatches between the CSV and the shapefiles. Carried
# over from the SW script - "Canarvon Canyon" -> "Carnarvon Canyon" and
# "Carter Island" -> "Cartier Island" are NW-network parks, so these are
# directly relevant here. Add more if the match-diagnostic below flags any.
name_fixes <- c(
  "Gulf of Carpenteria" = "Gulf of Carpentaria",  # not NW, harmless to keep
  "Canarvon Canyon"     = "Carnarvon Canyon",
  "Carter Island"       = "Cartier Island"
)

survey <- survey %>%
  mutate(amp_group = amp,                    # raw value - keeps any
         # eastern/western/offshore-style splits if NW ever has them
         amp_clean = clean_amp_name(amp),
         amp_clean = dplyr::recode(amp_clean, !!!name_fixes))

# Apply the same cleaning to both spatial layers' name fields
capad_commonwealth <- capad_commonwealth %>%
  dplyr::mutate(amp_clean = clean_amp_name(park_name_raw))

marine_parks_amp <- marine_parks_amp %>%
  dplyr::mutate(amp_clean = clean_amp_name(name))

# ==============================================================================
# 3. METHOD GROUPINGS
# ==============================================================================
# Each pie = one method family, sliced by platform x design (Preferential/
# Representative). Copied unchanged from the SW script's latest version -
# see adaptation note 7 above.
PREF_COLOUR <- "#FFA500"  # orange - Preferential (boss only)
REP_COLOUR  <- "#d7191c"  # red    - Representative (boss only)

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
    label = "UVC (RLS vs AIMS)"
  ),

  rov = list(
    platforms = c("stereo-ROV", "mono-ROV"),
    palette = c("stereo-ROV.Preferential"   = "#FFA500",
                "stereo-ROV.Representative" = "#d7191c",
                "mono-ROV.Preferential"     = "#FFD590",
                "mono-ROV.Representative"   = "#f4a9a0"),
    label = "ROV (stereo + mono)"
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
# 4. BUILD PIE DATA FOR ONE METHOD GROUP
# ==============================================================================
# `amp_group` column used as the join key so any future eastern/western/
# offshore-style splits (like the SWC arms) are respected automatically.
build_pie_data <- function(group_name) {
  grp <- method_groups[[group_name]]

  wide <- survey %>%
    filter(network == NETWORK_NAME, platform %in% grp$platforms) %>%
    mutate(category = paste(platform, design, sep = ".")) %>%
    group_by(amp_group, category) %>%
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
network_lookup <- survey %>%
  filter(network == NETWORK_NAME) %>%
  distinct(network, amp_clean)

if (nrow(network_lookup) == 0) {
  stop(
    "No rows in `survey` matched network == \"", NETWORK_NAME, "\". ",
    "Check the exact spelling used in the CSV's Network column and update ",
    "`NETWORK_NAME` above (see adaptation note 4)."
  )
}

match_diag <- network_lookup %>%
  mutate(matched = amp_clean %in% capad_commonwealth$amp_clean)
if (any(!match_diag$matched)) {
  message("--- Match check: these CSV parks had NO matching CAPAD polygon ---")
  print(match_diag %>% filter(!matched))
  message("Check spelling against `park_name_raw` in `capad_commonwealth`, and add",
          " any fixes to `name_fixes` in Section 2.")
}

# Centroids sourced from the national CAPAD layer (generic - see note 6)
amp_centres <- capad_commonwealth %>%
  st_drop_geometry() %>%
  distinct(amp_clean) %>%
  left_join(
    capad_commonwealth %>% group_by(amp_clean) %>% summarise(geometry = st_union(geometry)) %>%
      st_centroid() %>% mutate(X = st_coordinates(.)[, 1], Y = st_coordinates(.)[, 2]) %>%
      st_drop_geometry(),
    by = "amp_clean"
  )

amp_group_centres <- survey %>%
  filter(network == NETWORK_NAME) %>%
  distinct(amp_group, amp_clean) %>%
  left_join(amp_centres, by = "amp_clean")

if (any(is.na(amp_group_centres$X))) {
  message("--- These NW parks have no resolvable centroid - pies for them will be dropped ---")
  print(amp_group_centres %>% filter(is.na(X)))
  message("Add a manual override to `amp_group_centres` (rows_update by amp_group), same",
          " pattern as `swc_manual_centres` in the SW script, if needed.")
}

# ==============================================================================
# 6. PIE LAYER + SIZE LEGEND
# ==============================================================================
# Split into three functions so a size legend can reuse the exact radius
# scaling that got applied to the real pies (including the per-map
# no-overlap shrink, which varies map to map):
#   - scale_pie_radii(): sqrt-rescale + no-overlap shrink (same maths as
#     the old add_pies()), returns the scaled data AND the actual final
#     min_r/max_r/shrink factor used - not just the data.
#   - pie_layer(): draws geom_scatterpie from already-scaled data.
#   - pie_size_legend(): builds a small "pie size ~ N sites" key from
#     2-3 reference totals, scaled with the same maths via
#     radius_for_totals().
# Zero-data guard (e.g. drop_camera in this network) is now on
# scale_pie_radii(): it returns max_total = 0, which pie_layer() and
# pie_size_legend() both treat as "draw nothing" rather than erroring.
scale_pie_radii <- function(pie_data, min_r = 0.1, max_r = 1, overlap_margin = 0.92) {

  pie_data <- pie_data %>% filter(total > 0)
  n <- nrow(pie_data)

  if (n == 0) {
    message("  -> No survey data for this method group in this network - skipping pie layer.")
    return(list(data = pie_data, min_r = min_r, max_r = max_r,
                max_total = 0, shrink_factor = 1))
  }

  max_total <- max(pie_data$total, na.rm = TRUE)
  pie_data$r <- scales::rescale(sqrt(pie_data$total), to = c(min_r, max_r), from = c(0, sqrt(max_total)))

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

pie_layer <- function(scale_info, palette) {
  if (scale_info$max_total <= 0 || nrow(scale_info$data) == 0) return(list())

  list(
    ggnewscale::new_scale_fill(),
    scatterpie::geom_scatterpie(
      data      = scale_info$data,
      aes(x = X, y = Y, r = r),
      cols      = names(palette),
      colour    = "black",
      linewidth = 0.15,
      alpha     = 0.65
    ),
    scale_fill_manual(
      name   = "Survey design",
      values = palette,
      labels = gsub("\\.", " ", names(palette))
    )
  )
}

# Small "pie size ~ N sites" key. ref_totals defaults to ~3 nice round
# numbers spanning the data range; pass your own e.g.
# pie_size_legend(scale_info, ref_totals = c(10, 50, 100)) to override.
pie_size_legend <- function(scale_info, ref_totals = NULL, unit_label = "sites") {

  if (scale_info$max_total <= 0) return(patchwork::plot_spacer())

  if (is.null(ref_totals)) {
    brks <- scales::breaks_extended(n = 3)(c(0, scale_info$max_total))
    ref_totals <- sort(unique(round(brks[brks > 0 & brks <= scale_info$max_total])))
    if (length(ref_totals) == 0) ref_totals <- round(scale_info$max_total)
  }

  ref_r <- radius_for_totals(ref_totals, scale_info)
  gap   <- max(ref_r) * 2.4
  key <- tibble::tibble(
    total = ref_totals,
    r     = ref_r,
    x     = seq(0, by = gap, length.out = length(ref_totals)),
    y     = 0
  )

  ggplot(key) +
    ggforce::geom_circle(aes(x0 = x, y0 = y, r = r),
                         fill = "grey70", colour = "black", linewidth = 0.2) +
    geom_text(aes(x = x, y = -max(ref_r) * 1.3, label = total), size = 3) +
    coord_equal(clip = "off") +
    labs(title = paste0("Pie size \u2248 ", unit_label)) +
    theme_void() +
    theme(plot.title      = element_text(size = 8, hjust = 0),
          plot.margin     = margin(4, 4, 4, 4),
          plot.background = element_rect(fill = "white", colour = NA, linewidth = 0.3))
}

# ==============================================================================
# 7. NETWORK-LEVEL PLOT - one pie per marine park
# ==============================================================================
# `xlim`/`ylim` default to the NW zones script's `plot_limits` (109-130 E,
# -26.5 to -12.5 S) - TUNE if pies get cut off or the extent looks wrong for
# a given method group.
make_nw_pie_map <- function(group_name, save_name = NULL,
                            xlim = c(109, 130), ylim = c(-26.5, -11.9),
                            min_r = 0.12, max_r = 1,
                            legend_pos = c(left = 0.01, bottom = 0.02, right = 0.20, top = 0.20),
                            width = 11.5, height = 6.5) {

  grp <- method_groups[[group_name]]

  pie_data <- build_pie_data(group_name) %>%
    inner_join(amp_group_centres, by = "amp_group") %>%
    filter(!is.na(X))

  scaled <- scale_pie_radii(pie_data, min_r = min_r, max_r = max_r)

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks",
                      guide = guide_legend(order = 1),
                      values = with(marine_parks_amp, setNames(colour, zone)),
                      breaks = c("Sanctuary Zone", "National Park Zone", "Habitat Protection Zone",
                                 "Multiple Use Zone", "Special Purpose Zone", "Recreational Use Zone")) +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name = "State and Territory Marine Parks",
                      guide = guide_legend(order = 2),
                      values = with(marine_parks_state, setNames(colour, zone)),
                      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone",
                                 "Special Purpose Zone", "Indigenous Protected Area",
                                 "Other State Marine Park Zone")) +
    pie_layer(scaled, grp$palette) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = "left",
          legend.justification = "top",
          legend.box = "vertical",
          plot.background = element_rect(fill = "white", colour = NA))

  # CHANGED - align_to = "full" anchors to the WHOLE graphic (legend column
  # + map panel together), not just the panel, so this now sits in the
  # blank space below the categorical legends on the left rather than
  # floating over the map.
  p <- p + patchwork::inset_element(
    pie_size_legend(scaled),
    left = legend_pos[["left"]], bottom = legend_pos[["bottom"]],
    right = legend_pos[["right"]], top = legend_pos[["top"]],
    align_to = "full"
  )

  if (!is.null(save_name)) {
    dir.create(network_out_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(network_out_dir, paste0(save_name, ".png")), p,
           dpi = 600, width = width, height = height, bg = "white")
  }
  p
}

# ==============================================================================
# 8. GENERATE THE SET
# ==============================================================================
# CHANGED: added a per-group message so it's visible at a glance in the
# console which method groups had data vs. were skipped (no functional
# effect on the plots themselves - the actual skip logic lives in
# `add_pies()` in Section 6).
for (g in names(method_groups)) {
  message("Building pie map for: ", g)
  make_nw_pie_map(g, save_name = paste0("nw-", g, "-pies"))
}

# ==============================================================================
# End of script
# ==============================================================================

