###
# Project: NESP 5.6 Project - South west Corner Report
# Task:    Survey-effort pie chart overlays (national + network level)
# Note:    SELF-CONTAINED - loads its own spatial data, does not require
#          any other script to have been run first.
#
# Outputs:
#   1. National overview map, one pie per marine NETWORK    (Image 1 & 2)
#   2. South-west network map, one pie per marine PARK/arm  (Image 3)
#   Both repeated per survey method family (BRUV, UVC, ROV, drop camera, BOSS)
#
# ------------------------------------------------------------------------
# FIX APPLIED IN THIS VERSION (see original script's own Section 1 note):
# The previous version built BOTH the national plot and the network plot
# from `marine_parks`, loaded from
# "south-and-western-australia_marine-parks-all.shp". That file only
# contains South/WA parks. Your CSV has 6 networks / 48 parks
# (Coral Sea x1, Indian Ocean Territories x2, North x8, North-west x13,
# South-west x16, Temperate East x8) - so every network except South-west
# (and maybe IOT) had NO matching polygon, got an NA centroid, and was
# silently dropped from the "national" map. That's almost certainly why it
# wasn't rendering as requested.
#
# Fix: the national plot now uses `capad` (CAPAD Marine 2024 - already
# loaded in this script, genuinely Australia-wide) for its basemap AND for
# matching CSV parks to polygons/centroids. The regional
# "south-and-western-australia..." file is still used, unchanged, for the
# South-west network-level plot only.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE (this version):
#   1. Added 4 more spelling fixes to `name_fixes` (Section 2), found via
#      the match-diagnostics message: Gulf of Carpenteria, Canarvon Canyon,
#      Carter Island, and Solitary all had no matching CAPAD polygon.
#      NOTE: "Solitary" -> "Solitary Islands" is a guess based on partial
#      string match against CAPAD's `park_name_raw` - double check the raw
#      CSV value for that row actually reads "Solitary" and not something
#      else `clean_amp_name()` is mangling.
#   2. `add_pies()` now draws every pie at the SAME fixed radius `r`
#      instead of rescaling radius by sqrt(total sites). This means pie
#      size no longer encodes total survey effort - only the slice
#      proportions do. `r` is a new tunable argument on both
#      `make_national_pie_map()` and `make_network_pie_map()`, replacing
#      the old `min_r`/`max_r` pair. Starting values are guesses - tune
#      per plot extent (national spans 57 degrees longitude, SWC network
#      spans 15 degrees, so they need very different `r`).
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 2 (this version):
#   1. National pies left unchanged (r = 2.2, as before).
#   2. South-west network pies bumped from r = 0.22 to r = 0.32 - still a
#      TUNE value, adjust once you see the render.
#   3. Added maps for the other 5 marine park networks (North, North-west,
#      Temperate East, Coral Sea, Indian Ocean Territories). IMPORTANT
#      CAVEAT: the original network-level function's zone detail (with the
#      mining-exclusion stripe pattern) comes from the REGIONAL shapefile
#      ("south-and-western-australia_marine-parks-all.shp"), which per the
#      script's own original note only covers South-west/WA. There is no
#      equivalent regional file here for the other networks, so:
#        - South-west still uses `make_network_pie_map()` (regional file,
#          full zone/pattern/state-park detail) - UNCHANGED behaviour.
#        - The other networks use a new, simpler function,
#          `make_network_pie_map_national()`, built on the NATIONAL CAPAD
#          layer (`fed_mps_national`, already loaded for the national
#          plot) filtered to each network's parks. This gets you AMP zone
#          colour + pies, but NO state-park overlay and NO mining-
#          exclusion stripe pattern, since those can't be reliably
#          attributed outside the SW regional file.
#   4. Map extent (xlim/ylim) for each of the 5 new networks is computed
#      AUTOMATICALLY from that network's own polygon bounding box (+ pad
#      degrees), rather than hand-picked - since I don't have real extents
#      for those regions to hand-tune. CHECK EACH ONE VISUALLY, especially
#      Indian Ocean Territories: it covers Christmas Island and the Cocos
#      (Keeling) Islands, ~900km apart over open ocean, so its auto bbox
#      will likely be a very wide, mostly-empty map - you may want to
#      override its xlim/ylim manually, or split it into two panels.
#   5. Network names for the 5 new networks are NOT hardcoded - the loop
#      in Section 9 just uses whatever network names actually appear in
#      `network_lookup$network` (minus South-west). I don't have your
#      CSV's exact spelling for "Coral Sea ..." / "Indian Ocean
#      Territories ..." to hand, so this avoids guessing wrong.
#   6. `amp_group_centres` (Section 5b) now sources pie-placement centroids
#      from the NATIONAL `capad_commonwealth` layer instead of the
#      regional `marine_parks_amp` layer, since the regional layer has no
#      polygons at all for parks outside South-west/WA. This should give
#      identical centroids to before for South-west parks (same geometry,
#      same name matching) - flagging in case you spot any drift.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 3 (this version):
#   1. Pie radius is now a MAXIMUM/target, auto-capped in add_pies() so
#      pies never overlap - see the comment above add_pies() (Section 6).
#      This means the `r` argument on each make_*() call below can be set
#      generously high and the function will shrink it (uniformly, for
#      every pie on that map) only as much as needed to keep pies from
#      touching. Bumped the requested `r` on all three map functions
#      accordingly - these are now ceilings, not fixed sizes.
#   2. Recoloured every method_groups palette (Section 3): colour now
#      encodes DESIGN ONLY (red = Preferential, green = Representative),
#      the same two colours across every platform in every group. This
#      means platform (e.g. stereo-BRUV vs mono-BRUV) is no longer
#      distinguishable by colour within a pie, only the design split is.
#      Legend (Section 6) collapsed to 2 entries per group accordingly.
#      NOTE: red/green together is a common colourblind-unfriendly pairing
#      (deuteranopia/protanopia) - flagging in case that matters for this
#      report's audience.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 4 (this version):
#   Reverted the red/green recolour for bruv, uvc, rov, and drop_camera
#   back to their ORIGINAL platform-distinguishing colours (hue = platform,
#   shade = design). Only `boss` keeps red = Preferential / green =
#   Representative - it's the one group where that 2-colour scheme loses
#   no information, since it only has a single platform (stereo-BOSS).
#   add_pies()'s legend logic (Section 6) now auto-detects, per group,
#   whether the palette resolves to 2 distinct colours (collapsed
#   "Preferential"/"Representative" legend) or more (full platform.design
#   legend) - no group-specific special-casing needed.
# ------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(scatterpie)
library(ggnewscale)
library(ggpattern)
library(scales)
library(janitor)

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
# Same processing approach as your australia-overview.R template: strip the
# parenthetical suffix off zone_type, derive a simple state-park/sanctuary
# split, and keep only the AMP zone vocabulary for Commonwealth zones.
capad_raw <- st_read(file.path(data_dir, "spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp")) %>%
  janitor::clean_names() %>%
  st_make_valid() %>%
  dplyr::mutate(zone_type = stringr::str_replace_all(zone_type, "\\s*\\([^\\)]+\\)", ""))

# Auto-detect the reserve-name column - stops with the real column list
# printed if none of these match, instead of guessing wrong silently.
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

# NOTE on this 2024 CAPAD export: `zone_type` is empty for every row (checked
# directly - see diagnostic further down). The zone instead has to be decoded
# from the last 3 letters of the 5-character zone code at the end of
# `res_number` (e.g. "swswcspm03" -> "spm", "telhihpz07" -> "hpz") - your
# original template already extracts this same 5-character code for zone
# labels (`str_sub(RES_NUMBER, -5, -1)`), this just goes one step further and
# maps the 3-letter prefix to a zone name. `epbc == "Commonwealth"` alone
# also isn't enough to isolate AMP zones - it also catches legacy
# Commonwealth Marine Reserves / Indigenous Protected Areas, etc. - so
# `type == "Australian Marine Park"` is now required too.
#
# BEST-GUESS mapping below, collapsed to the same 6-category vocabulary your
# original template uses (parenthetical sub-types like Trawl/Mining
# Exclusion collapse into their base zone, same as before). Confident on
# npz/muz/san/ruz; less sure what the r/l suffix on "hp" or the "n" suffix
# on "sp" specifically denote - CONFIRM/EDIT this table if you know
# otherwise. Any code NOT in this table gets flagged by the diagnostic below
# instead of silently disappearing.
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

# --- DIAGNOSTIC (safe to delete once the national plot looks right) -------
message("--- CAPAD diagnostic: zone codes with NO entry in amp_zone_code_map ---")
print(
  capad_raw %>% sf::st_drop_geometry() %>%
    dplyr::filter(epbc == "Commonwealth", type == "Australian Marine Park") %>%
    dplyr::mutate(zone_code = stringr::str_sub(res_number, -5, -3)) %>%
    dplyr::filter(!zone_code %in% names(amp_zone_code_map)) %>%
    dplyr::distinct(zone_code)
)
# ----------------------------------------------------------------------------

# All AMP zones - only "Australian Marine Park" reserves, Commonwealth only.
# NOTE: State marine park "Sanctuary Zone" can't be reliably derived the same
# way - state reserves don't carry these letter-coded res_numbers, and
# zone_type is empty everywhere in this file - so every State reserve here
# renders as a single "State Marine Park" colour rather than guessing at a
# sanctuary split.
fed_mps_national <- capad_raw %>%
  dplyr::filter(epbc == "Commonwealth", type == "Australian Marine Park") %>%
  dplyr::mutate(zone_code = stringr::str_sub(res_number, -5, -3),
                zone_type = dplyr::recode(zone_code, !!!amp_zone_code_map,
                                          .default = NA_character_)) %>%
  dplyr::filter(!is.na(zone_type))

state_mps_national <- capad_raw %>%
  dplyr::filter(epbc == "State") %>%
  dplyr::mutate(sanctuary = "State Marine Park")

# Only "Australian Marine Park" Commonwealth reserves - used to dissolve
# each park's full extent down to one centroid per park, for pie placement.
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

# Known spelling mismatches between the CSV and the shapefiles - add more
# here if the match-diagnostics message below flags any.
# The four entries below were added after the match-diagnostics run flagged
# them as unmatched against `capad_commonwealth$park_name_raw`:
#   "Gulf of Carpenteria" -> "Gulf of Carpentaria" (transposed letters)
#   "Canarvon Canyon"     -> "Carnarvon Canyon"     (missing "r")
#   "Carter Island"       -> "Cartier Island"       (transposed letters)
#   "Solitary"            -> "Solitary Islands"     (CSV value appears
#                             truncated relative to CAPAD's full name -
#                             confirm the raw CSV cell actually reads
#                             "Solitary" before trusting this one)
name_fixes <- c(
  "Western Erye"        = "Western Eyre",
  "Gulf of Carpenteria" = "Gulf of Carpentaria",
  "Canarvon Canyon"     = "Carnarvon Canyon",
  "Carter Island"       = "Cartier Island",
  "Solitary"            = "Solitary Islands"
)

survey <- survey %>%
  mutate(amp_group = amp,                    # raw value - keeps the
         # (eastern/western arm/offshore) split Tim asked for
         amp_clean = clean_amp_name(amp),
         amp_clean = dplyr::recode(amp_clean, !!!name_fixes))

# Apply the same cleaning to both spatial layers' name fields
capad_commonwealth <- capad_commonwealth %>%
  dplyr::mutate(amp_clean = clean_amp_name(park_name_raw))

marine_parks_amp <- marine_parks_amp %>%
  dplyr::mutate(amp_clean = clean_amp_name(name))

# amp_clean added to fed_mps_national (national CAPAD, all networks) too,
# so it can be filtered per-network in make_network_pie_map_national()
# (Section 8b) for the 5 networks outside South-west.
fed_mps_national <- fed_mps_national %>%
  dplyr::mutate(amp_clean = clean_amp_name(park_name_raw))

# ==============================================================================
# 3. METHOD GROUPINGS (per Tim's comment on Image 2)
# ==============================================================================
# Each pie = one method family, sliced by platform x design (Preferential/
# Representative).
#
# Colour scheme differs by group now (per your last request):
#   - bruv, uvc, rov: back to the ORIGINAL scheme - hue = platform
#     (red-family vs blue-family), shade = design (dark = Preferential,
#     light = Representative). 4 distinct colours, 4 legend entries.
#   - drop_camera: also back to its original single red-family
#     dark/light pair (it only has one platform, so this was never
#     ambiguous either way).
#   - boss: KEPT on the red = Preferential / green = Representative
#     scheme, since it only has one platform (stereo-BOSS) - collapsing
#     to 2 colours here loses no platform information.
# The legend logic in add_pies() (Section 6) auto-detects which case
# applies: if a group's palette only has 2 distinct colours (like boss),
# the legend shows just "Preferential"/"Representative"; otherwise it
# shows the full platform.design breakdown, same as before.
PREF_COLOUR <- "#FFA500"  # red   - Preferential (boss only)
REP_COLOUR  <- "#d7191c"  # green - Representative (boss only)

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
# 4. BUILD PIE DATA FOR ONE METHOD GROUP AT A GIVEN SPATIAL LEVEL
# ==============================================================================
# level = "network" (national plot) or "amp" (network-level plot, using the
# amp_group column so the SWC western/eastern/offshore split is respected)
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
# 5a. Network centres (national plot) - dissolve every NATIONAL CAPAD polygon
#     that belongs to each network (via the CSV's network<->park mapping)
#     and take the centroid of the union.
network_lookup <- survey %>% distinct(network, amp_clean)

match_diag <- network_lookup %>%
  mutate(matched = amp_clean %in% capad_commonwealth$amp_clean)
if (any(!match_diag$matched)) {
  message("--- National match check: these CSV parks had NO matching CAPAD polygon ---")
  print(match_diag %>% filter(!matched))
  message("Check spelling against `park_name_raw` in `capad_commonwealth`, and add",
          " any fixes to `name_fixes` in Section 2.")
}

network_centres <- purrr::map_dfr(unique(network_lookup$network), function(net) {
  amps_in_net <- network_lookup$amp_clean[network_lookup$network == net]
  matched <- capad_commonwealth %>% filter(amp_clean %in% amps_in_net)
  if (nrow(matched) == 0) return(tibble(network = net, X = NA_real_, Y = NA_real_))
  geom <- matched %>% st_union() %>% st_centroid() %>% st_coordinates()
  tibble(network = net, X = geom[1, "X"], Y = geom[1, "Y"])
}) %>%
  filter(!is.na(X))


# 5b. AMP / group centres - sourced from the NATIONAL capad_commonwealth
# layer (previously this used the regional marine_parks_amp layer, which
# only has polygons for South-west/WA parks - so every other network's
# amp_group_centres came out NA and couldn't be pie-mapped at all). Using
# the national layer here should give identical centroids to before for
# South-west parks (same underlying geometry, same amp_clean matching),
# while also resolving centroids for the other 5 networks.
amp_centres <- capad_commonwealth %>%
  st_drop_geometry() %>%
  distinct(amp_clean) %>%
  left_join(
    capad_commonwealth %>% group_by(amp_clean) %>% summarise(geometry = st_union(geometry)) %>%
      st_centroid() %>% mutate(X = st_coordinates(.)[, 1], Y = st_coordinates(.)[, 2]) %>%
      st_drop_geometry(),
    by = "amp_clean"
  )

swc_manual_centres <- tribble(
  ~amp_group,                                    ~X,     ~Y,
  # Western arm shifted from 114.9 to 112.5 (further west) - the pie here
  # was one of the tightest-clustered against the Murat/Western Eyre pies
  # sitting near 114-115 deg E, which was capping every pie's size down
  # via the no-overlap check in add_pies(). Giving it more clear space
  # lets the whole map's pies render bigger. Y left unchanged. Move it
  # back or elsewhere if this drifts it into a spot you don't like.
  "South-west Corner Marine Park (western arm)", 112.5, -34.0,
  "South-west Corner Marine Park (eastern arm)", 120.6, -35.2,
  "South-west Corner Marine Park (offshore)",    117.5, -37.5
)

amp_group_centres <- survey %>%
  distinct(amp_group, amp_clean) %>%
  left_join(amp_centres, by = "amp_clean") %>%
  rows_update(swc_manual_centres, by = "amp_group")

# ==============================================================================
# 6. PIE LAYER (shared by both plot levels)
# ==============================================================================
# Pie radius encodes total survey effort again: rescaled from
# sqrt(total sites) onto [min_r, max_r] (bigger effort = bigger pie), same
# approach as the original script. On top of that, a no-overlap guarantee
# (added in UPDATE 3, kept here): if the rescaled radii would make any two
# pies on this map touch or overlap, ALL radii are shrunk by the same
# proportional factor - preserving relative size differences - until the
# worst-case pair clears `overlap_margin`. So `min_r`/`max_r` below are
# upper-bound TARGETS: actual sizes may come out smaller if pies are
# tightly clustered.
add_pies <- function(pie_data, palette, min_r = 0.1, max_r = 1, overlap_margin = 0.92) {

  pie_data <- pie_data %>% filter(total > 0)
  n <- nrow(pie_data)

  if (n > 0) {
    max_total <- max(pie_data$total, na.rm = TRUE)
    pie_data$r <- if (max_total > 0) {
      scales::rescale(sqrt(pie_data$total), to = c(min_r, max_r), from = c(0, sqrt(max_total)))
    } else {
      min_r
    }
  } else {
    pie_data$r <- numeric(0)
  }

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

  # Legend: always shows every platform.design combination in `palette` by
  # its full name (e.g. "stereo-BOSS.Preferential"), same for every group -
  # no collapsing to a generic "Preferential"/"Representative" pair, even
  # for boss/drop_camera which only have 2 distinct colours.
  list(
    ggnewscale::new_scale_fill(),
    scatterpie::geom_scatterpie(
      data      = pie_data,
      aes(x = X, y = Y, r = r),
      cols      = names(palette),
      colour    = "white",
      linewidth = 0.15
    ),
    scale_fill_manual(
      name   = "Survey design",
      values = palette,
      labels = names(palette)
    )
  )
}

# ==============================================================================
# 7. NATIONAL PLOT (Image 1 + 2) - now built on the national CAPAD layer
# ==============================================================================
make_national_pie_map <- function(group_name, save_name = NULL,
                                  min_r = 0.5, max_r = 3,  # TUNE - max_r is a ceiling; auto-shrinks to avoid overlap
                                  width = 11, height = 6) {

  grp <- method_groups[[group_name]]
  pie_data <- build_pie_data(group_name, level = "network") %>%
    left_join(network_centres, by = "network") %>%
    filter(!is.na(X))

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
    add_pies(pie_data, grp$palette, min_r = min_r, max_r = max_r) +
    coord_sf(xlim = c(90, 175), ylim = c(-60, -5), expand = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = "left",
          legend.box = "vertical",
          plot.background = element_rect(fill = "white", colour = NA))

  if (!is.null(save_name)) {
    dir.create(national_out_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(national_out_dir, paste0(save_name, ".png")), p,
           dpi = 600, width = width, height = height, bg = "white")
  }
  p
}

# ==============================================================================
# 8. NETWORK-LEVEL PLOT (Image 3) - one pie per marine park / SWC arm
# ==============================================================================
make_network_pie_map <- function(group_name, network_name, save_name = NULL,
                                 xlim, ylim, min_r = 0.25, max_r = 2,  # TUNE - max_r is a ceiling; auto-shrinks to avoid overlap
                                 width = 10, height = 5) {

  grp <- method_groups[[group_name]]
  amps_in_network <- network_lookup$amp_clean[network_lookup$network == network_name]

  pie_data <- build_pie_data(group_name, level = "amp") %>%
    inner_join(amp_group_centres, by = "amp_group") %>%
    filter(amp_clean %in% amps_in_network, !is.na(X))

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
# 8b. NETWORK-LEVEL PLOT, NATIONAL VERSION - for every network OTHER than
#     South-west (North, North-west, Temperate East, Coral Sea, Indian Ocean
#     Territories). Built on the national CAPAD layer (fed_mps_national)
#     instead of the SW-only regional shapefile, since no equivalent
#     regional file exists for these networks in this script.
#
#     TRADE-OFF vs make_network_pie_map(): no state-park overlay, no
#     mining-exclusion stripe pattern - just AMP zone colour (same 6-colour
#     national vocabulary as the national plot) + pies. Map extent is
#     computed automatically from the network's own polygon bounding box
#     (padded by `pad` degrees) rather than hand-picked - check each one
#     visually, especially Indian Ocean Territories (Christmas Island +
#     Cocos Islands are ~900km apart, so its auto extent will likely be
#     very wide and mostly empty).
# ==============================================================================
make_network_pie_map_national <- function(group_name, network_name, save_name = NULL,
                                          pad = 3, min_r = 0.25, max_r = 2,  # TUNE - max_r auto-shrinks to avoid overlap
                                          width = 11, height = 5) {

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

  bbox <- sf::st_bbox(net_amp)
  xlim <- c(bbox[["xmin"]] - pad, bbox[["xmax"]] + pad)
  ylim <- c(bbox[["ymin"]] - pad, bbox[["ymax"]] + pad)

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf(data = net_amp, aes(fill = zone_type), colour = NA, alpha = 0.8) +
    scale_fill_manual(values = amp_zone_colours_national, name = "Australian Marine Parks") +
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
# 9. GENERATE THE SET
# ==============================================================================
for (g in names(method_groups)) {
  make_national_pie_map(g, save_name = paste0("national-", g, "-pies"))
}

# Per-group size override: if a method_groups entry has a `network_size`
# list (currently just uvc/rov - see Section 3), use its min_r/max_r for
# the network-level maps instead of make_network_pie_map()'s defaults.
# Groups without `network_size` (bruv, drop_camera, boss) are unaffected
# and keep rendering at the function's default size.
for (g in names(method_groups)) {
  grp <- method_groups[[g]]
  size_args <- if (!is.null(grp$network_size)) grp$network_size else list()
  do.call(make_network_pie_map, c(
    list(
      group_name   = g,
      network_name = "South-west Marine Parks Network",
      xlim         = c(106, 139),
      ylim         = c(-40, -24),
      save_name    = paste0("swc-", g, "-pies")
    ),
    size_args
  ))
}
# ==============================================================================
# End of script
# ==============================================================================


