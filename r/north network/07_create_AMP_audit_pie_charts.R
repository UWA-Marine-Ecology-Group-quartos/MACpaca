###
# Project: NESP 5.6 Project - North Network Report
# Task:    Survey-effort pie chart overlay (network level only, one pie per
#          marine park), adapted from the North-west Network survey-effort
#          pie chart script, using spatial setup from the North network
#          zones script ("North Network Report" / Annika Leunig, July 2026).
# Note:    SELF-CONTAINED - loads its own spatial data, does not require
#          any other script to have been run first.
#
# Output:
#   North network map, one pie per marine PARK, repeated per survey method
#   family (BRUV, UVC, ROV, drop camera, BOSS). NO national-level plot in
#   this version (matching the NW script) - that logic (Sections 1a/7 of
#   the original SW script) can be dropped back in if wanted.
#
# ------------------------------------------------------------------------
# ADAPTATION NOTES (differences from the NW pie script this was built from):
#
# 1. SPATIAL DATA (Section 1): uses "north-network-australia_marine-parks-
#    all.shp" (per your North zones script), filtered to the SAME park
#    name list your zones script uses - Commonwealth AMPs + NT/Qld state
#    marine parks + IPAs - and cropped to the same `e_mpa` extent
#    (126-142.5 E, -18 to -9 S) as the zones script's Section 1.
#
# 2. NO MINING-EXCLUSION STRIPE PATTERN: same as the NW script - your North
#    zones script doesn't build that distinction either, so zones render
#    as flat colour only, taken directly from the shapefile's `colour`
#    column.
#
# 3. INDIGENOUS PROTECTED AREAS: North has FIVE IPAs, carried over verbatim
#    from your North zones script's Section 2 recode logic - "Dhimurru",
#    "Thuwathu/Bujimulla", "Anindilyakwa", "Djelk - Stage 2", and
#    "Crocodile Islands Maringa" (vs NW's two, Yawuru/Nyangumarta Warrarn).
#    Unlike NW, these are coded "State" (not "Indigenous") in the North
#    zones script's own filter (`epbc %in% "State"` catches them directly)
#    - CONFIRM this against the actual shapefile if IPA pies/zones don't
#    render; if they turn out to be coded "Indigenous" here too, add that
#    to the `epbc %in% c(...)` filter in Section 1 below, same as NW.
#
# 4. NETWORK NAME STRING: `network_lookup`/`build_pie_data()` filter on
#    whatever string your CSV's `Network` column uses for this network.
#    I've assumed "North Marine Parks Network" (parallel to "South-west
#    Marine Parks Network" / "North-west Marine Parks Network" in the
#    other scripts) - CONFIRM against the actual CSV value; if it's off,
#    every centroid/pie for this network will silently come back empty
#    rather than erroring (there's a stop() check below that will catch a
#    total mismatch, but not a partial/wrong-but-plausible one).
#
# 5. SURVEY CSV PATH: assumed to live alongside the other North inputs at
#    `data/north network/amp_data_sheet_-_data.csv` (matching the folder
#    name used in your North zones script, "data/north network/..."). If
#    this is actually a single shared master file living elsewhere, update
#    `survey_csv_path` below - left as its own variable for exactly this
#    reason.
#
# 6. CAPAD VERSION FOR CENTROIDS: your North zones script loads CAPAD
#    Marine *2022* for its inset map. For consistency with the SW/NW pie
#    scripts (which both source pie-placement centroids from CAPAD Marine
#    *2024*), this script uses the 2024 file for centroids too - it's only
#    used here to compute park centroids, not for any zone colouring, so
#    the version mismatch shouldn't matter unless park boundaries changed
#    materially between the two releases. Flagging in case that's not the
#    file you want.
#
# 7. PIE CENTROIDS (Section 5): sourced from the national CAPAD layer
#    (`capad_commonwealth`), same generic approach as the SW/NW scripts -
#    not North-specific, so it should resolve correctly for North parks
#    without changes. No manual centroid overrides are pre-added here (the
#    SW script needed 3, for the SWC arms) - add any if the
#    match-diagnostic below flags North parks with no resolvable centroid,
#    or if pies come out badly placed/overlapping in a way `add_pies()`'s
#    auto-shrink can't fix cleanly.
#
# 8. METHOD GROUPS / COLOURS (Section 3): copied unchanged from the NW
#    script (platform-hue palettes for bruv/uvc/rov/drop_camera, red/green
#    Preferential-Representative for boss) so colour meaning stays
#    consistent across network reports.
#
# 9. PIE STYLING (Section 6): pies drawn with a black border and 65%
#    opacity fill (`colour = "black"`, `alpha = 0.65`), matching the
#    style requested/applied in the NW script.
# ------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(scatterpie)
library(ggnewscale)
library(scales)
library(janitor)

sf_use_s2(TRUE)

data_dir       <- "data/north network"
survey_csv_path <- file.path(data_dir, "amp_data_sheet_-_data.csv")

audit_root      <- "plots/network/spatial/AMP_audit"
network_out_dir <- file.path(audit_root, "network")

NETWORK_NAME <- "North Marine Parks Network"

# ==============================================================================
# 1. LOAD SPATIAL DATA
# ==============================================================================

# ── Australia outline ────────────────────────────────────────────────────────
aus <- st_read(file.path(data_dir, "spatial/shapefiles/STE_2021_AUST_GDA2020.shp")) %>%
  st_make_valid()

# ── National CAPAD - used ONLY to source pie-placement centroids ────────────
# (generic across networks, same approach as the SW/NW scripts - not
# filtered to North here, just matched later via `amp_clean` per network)
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

# ── North regional layer - zone polygons for the network-level plot ─────────
# Same file, same park filter list, and same crop extent as your North
# zones script (Section 1).
e_mpa <- st_bbox(c(xmin = 125, xmax = 142.5, ymin = -18, ymax = -9), crs = st_crs(4326))

north_park_names <- c(
  # Commonwealth AMPs (North Network)
  "Arafura", "Arnhem", "Gulf of Carpentaria", "Joseph Bonaparte Gulf",
  "Limmen", "Oceanic Shoals", "Wessel", "West Cape York", "North Kimberley",
  # NT/Qld state marine parks
  "Garig Gunak Barlu", "Limmen Bight", "Eight Mile Creek",
  "Morning Inlet - Bynoe River", "Staaten-Gilbert", "Nassau River",
  "Pine River Bay",
  # Indigenous Protected Areas
  "Dhimurru", "Thuwathu/Bujimulla", "Anindilyakwa", "Djelk - Stage 2",
  "Crocodile Islands Maringa"
)

marine_parks <- st_read(file.path(data_dir, "spatial/shapefiles/north-network-australia_marine-parks-all.shp")) %>%
  dplyr::filter(name %in% north_park_names) %>%
  st_crop(e_mpa)

# Australian Marine Parks only (Commonwealth) - flat colour, no stripe
# pattern (see adaptation note 2 above)
marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth")

# Indigenous Protected Areas (in state waters) - kept distinguishable from
# other state zones, same recode as your North zones script. NOTE: per
# adaptation note 3, these are assumed coded "State" here (not "Indigenous"
# like NW's) - CONFIRM and edit the `epbc %in% c(...)` filter below if not.
ipa_names <- c("Dhimurru", "Thuwathu/Bujimulla", "Anindilyakwa",
               "Djelk - Stage 2", "Crocodile Islands Maringa")

marine_parks_state <- marine_parks %>%
  dplyr::filter(epbc %in% "State") %>%
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
# over from the SW/NW scripts - none of the previously-flagged fixes
# ("Gulf of Carpenteria", "Canarvon Canyon", "Carter Island") are North-
# network parks, so this starts empty. Add any here if the match-
# diagnostic below flags North parks.
name_fixes <- c(
  "Gulf of Carpenteria" = "Gulf of Carpentaria"  # not North-specific, harmless to keep
)

survey <- survey %>%
  mutate(amp_group = amp,                    # raw value - keeps any
         # eastern/western/offshore-style splits if North ever has them
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
# Representative). Copied unchanged from the NW script - see adaptation
# note 8 above.
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

# Centroids sourced from the national CAPAD layer (generic - see note 7)
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
  message("--- These North parks have no resolvable centroid - pies for them will be dropped ---")
  print(amp_group_centres %>% filter(is.na(X)))
  message("Add a manual override to `amp_group_centres` (rows_update by amp_group), same",
          " pattern as `swc_manual_centres` in the SW script, if needed.")
}

# ==============================================================================
# 6. PIE LAYER
# ==============================================================================
# Pie radius encodes total survey effort (rescaled from sqrt(total sites)
# onto [min_r, max_r]), with a no-overlap guarantee: if the rescaled radii
# would make any two pies touch or overlap, ALL radii are shrunk by the same
# proportional factor (preserving relative size differences) until the
# worst-case pair clears `overlap_margin`. `min_r`/`max_r` are therefore
# upper-bound TARGETS - actual sizes may come out smaller if pies are
# tightly clustered. Identical logic to the SW/NW scripts' `add_pies()`.
#
# Guards against zero-row `pie_data` (e.g. a method group with no surveys
# logged yet for this network) - returns an empty layer list instead of
# letting `geom_scatterpie` error on empty data.
#
# Pies drawn with a black border and 65% opacity fill (see adaptation
# note 9 above) - matches the styling applied in the NW script.
add_pies <- function(pie_data, palette, min_r = 0.1, max_r = 1, overlap_margin = 0.92) {

  pie_data <- pie_data %>% filter(total > 0)
  n <- nrow(pie_data)

  if (n == 0) {
    message("  -> No survey data for this method group in this network - skipping pie layer.")
    return(list())
  }

  max_total <- max(pie_data$total, na.rm = TRUE)
  pie_data$r <- scales::rescale(sqrt(pie_data$total), to = c(min_r, max_r), from = c(0, sqrt(max_total)))

  if (n >= 2) {
    d <- as.matrix(dist(pie_data[, c("X", "Y")]))
    diag(d) <- NA
    r_sum <- outer(pie_data$r, pie_data$r, "+")
    overlap_ratio <- r_sum / (d * overlap_margin)
    max_ratio <- suppressWarnings(max(overlap_ratio, na.rm = TRUE))
    if (is.finite(max_ratio) && max_ratio > 1) {
      pie_data$r <- pie_data$r / max_ratio
    }
  }

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
      labels = names(palette)
    )
  )
}

# ==============================================================================
# 7. NETWORK-LEVEL PLOT - one pie per marine park
# ==============================================================================
# `xlim`/`ylim` default to the North zones script's `plot_limits`
# (126-142.5 E, -18 to -9 S) - TUNE if pies get cut off or the extent
# looks wrong for a given method group.
make_north_pie_map <- function(group_name, save_name = NULL,
                               xlim = c(125.5, 142.5), ylim = c(-18, -8.5),
                               min_r = 0.06, max_r = 0.4,  # TUNE - max_r is a ceiling; auto-shrinks to avoid overlap
                               width = 11, height = 5) {

  grp <- method_groups[[group_name]]

  pie_data <- build_pie_data(group_name) %>%
    inner_join(amp_group_centres, by = "amp_group") %>%
    filter(!is.na(X))

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = marine_parks_amp, aes(fill = zone), colour = NA, alpha = 0.8) +
    scale_fill_manual(name = "Australian Marine Parks",
                      guide = guide_legend(order = 1),
                      values = with(marine_parks_amp, setNames(colour, zone)),
                      breaks = c("National Park Zone", "Habitat Protection Zone",
                                 "Multiple Use Zone", "Special Purpose Zone")) +
    new_scale_fill() +
    geom_sf(data = marine_parks_state, aes(fill = zone), colour = NA, alpha = 0.6) +
    scale_fill_manual(name = "State and Territory Marine Parks",
                      guide = guide_legend(order = 2),
                      values = with(marine_parks_state, setNames(colour, zone)),
                      breaks = c("Sanctuary Zone", "General Use Zone", "Recreational Use Zone",
                                 "Special Purpose Zone", "Indigenous Protected Area",
                                 "Other State Marine Park Zone")) +
    add_pies(pie_data, grp$palette, min_r = min_r, max_r = max_r) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = "left",
          plot.background = element_rect(fill = "white", colour = NA))

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
for (g in names(method_groups)) {
  message("Building pie map for: ", g)
  make_north_pie_map(g, save_name = paste0("north-", g, "-pies"))
}

# ==============================================================================
# End of script
# ==============================================================================
