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
#
# ------------------------------------------------------------------------
# UPDATE 5 (this version):
#   1. Legend labels: dots -> spaces ("stereo-BRUV Preferential" instead
#      of "stereo-BRUV.Preferential").
#   2. `fit_dims()` added - derives ggsave() width/height from the true
#      lon/lat extent (correcting for longitude compression at latitude),
#      clamped to a sensible min/max, instead of a fixed canvas size
#      regardless of how stretched a network's extent is.
#   3. Added a general `manual_centre_overrides` table (was
#      `swc_manual_centres`) plus a safety-net jitter for any amp_group
#      centroids that still collide.
#   4. National match-diagnostics now also writes unmatched rows to CSV
#      and raises a warning() specifically flagging Indian Ocean
#      Territories misses.
#   5. `clean_amp_name()` now also strips "(on shelf)".
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 6 (this version):
#   fit_dims() gained a `legend_allowance` (inches) parameter. coord_sf()
#   enforces the true geographic aspect ratio on the PANEL itself, but the
#   width/height fit_dims() previously worked out assumed the whole canvas
#   was available to the panel - it wasn't, because legend.position =
#   "left" eats a chunk of that width for the legend. That forced the
#   panel to shrink to keep its aspect ratio correct, leaving the leftover
#   space as blank margin (mostly above the panel, since the legend is
#   top-justified and is often taller than the panel on smaller networks).
#   Reserving legend_allowance inches up front means the panel gets the
#   full width/aspect it actually needs. TUNE VALUE - 2.2in is a starting
#   guess based on how wide the longest legend labels/titles are.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 7 (this version):
#   ggsave() opens the PNG device before drawing, so a render error
#   partway through (e.g. "object 'X' not found" during aesthetic
#   computation) can still leave a blank/partial file on disk once the
#   device is flushed and closed. Both save blocks now delete that file
#   and re-raise so the failure still gets caught/logged upstream instead
#   of silently leaving a bad PNG behind.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 8 (this version):
#   `safe_pie_layer()` wraps scale_pie_radii() + pie_layer() in a
#   tryCatch. If pie_data is empty, malformed, or scale_pie_radii()/
#   pie_layer() error for any reason, this catches it, logs a message, and
#   returns an EMPTY layer instead of propagating the error - so the
#   calling make_*() function still builds and saves a map (basemap + zone
#   polygons), just without a pie layer, instead of crashing outright.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 9 (this version):
#   1. SWC's pie-size legend was overlapping its own design legend (the
#      white key box was painting over the front of "Preferential"/
#      "Representative") because SWC's legend - three stacked groups:
#      State Marine Parks, Australian Marine Parks (incl. the mining-
#      exclusion pattern swatch), and Survey design - runs much longer
#      down the left side than the two-group legend the other five
#      networks use, and reaches into the default bottom-left corner
#      where the size key sits. Given `make_network_pie_map()` its own
#      `legend_pos` override for the SWC call only, moved to the
#      bottom-right of the panel instead.
#   2. SWC's xlim/ylim tightened from the old hand-picked c(106, 139) x
#      c(-40, -24) - noticeably bigger than the actual pie/park footprint,
#      leaving several degrees of blank ocean above and below every
#      render - to c(107, 137) x c(-38, -29). CHECK against the actual
#      northernmost/southernmost SWC pies and adjust further if anything
#      gets clipped.
#   3. `pad` for the other five (auto bbox-derived) networks dropped from
#      the function default of 3 to 1.5, passed explicitly in Section 9 -
#      3 degrees of padding on every side was reading as a lot of empty
#      margin, most visibly above the panel, for tightly clustered
#      networks.
#   4. Section 7/8/8b theme()s: legend.title bumped to size 11, legend.text
#      to size 9 (previously default ggplot sizes), for readability at the
#      report's print size.
# ------------------------------------------------------------------------

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

r
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
#
# ------------------------------------------------------------------------
# UPDATE 5 (this version):
#   1. Legend labels: dots -> spaces ("stereo-BRUV Preferential" instead
#      of "stereo-BRUV.Preferential").
#   2. `fit_dims()` added - derives ggsave() width/height from the true
#      lon/lat extent (correcting for longitude compression at latitude),
#      clamped to a sensible min/max, instead of a fixed canvas size
#      regardless of how stretched a network's extent is.
#   3. Added a general `manual_centre_overrides` table (was
#      `swc_manual_centres`) plus a safety-net jitter for any amp_group
#      centroids that still collide.
#   4. National match-diagnostics now also writes unmatched rows to CSV
#      and raises a warning() specifically flagging Indian Ocean
#      Territories misses.
#   5. `clean_amp_name()` now also strips "(on shelf)".
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 6 (this version):
#   fit_dims() gained a `legend_allowance` (inches) parameter. coord_sf()
#   enforces the true geographic aspect ratio on the PANEL itself, but the
#   width/height fit_dims() previously worked out assumed the whole canvas
#   was available to the panel - it wasn't, because legend.position =
#   "left" eats a chunk of that width for the legend. That forced the
#   panel to shrink to keep its aspect ratio correct, leaving the leftover
#   space as blank margin (mostly above the panel, since the legend is
#   top-justified and is often taller than the panel on smaller networks).
#   Reserving legend_allowance inches up front means the panel gets the
#   full width/aspect it actually needs. TUNE VALUE - 2.2in is a starting
#   guess based on how wide the longest legend labels/titles are.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 7 (this version):
#   ggsave() opens the PNG device before drawing, so a render error
#   partway through (e.g. "object 'X' not found" during aesthetic
#   computation) can still leave a blank/partial file on disk once the
#   device is flushed and closed. Both save blocks now delete that file
#   and re-raise so the failure still gets caught/logged upstream instead
#   of silently leaving a bad PNG behind.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 8 (this version):
#   `safe_pie_layer()` wraps scale_pie_radii() + pie_layer() in a
#   tryCatch. If pie_data is empty, malformed, or scale_pie_radii()/
#   pie_layer() error for any reason, this catches it, logs a message, and
#   returns an EMPTY layer instead of propagating the error - so the
#   calling make_*() function still builds and saves a map (basemap + zone
#   polygons), just without a pie layer, instead of crashing outright.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 9 (this version):
#   1. SWC's pie-size legend was overlapping its own design legend (the
#      white key box was painting over the front of "Preferential"/
#      "Representative") because SWC's legend - three stacked groups:
#      State Marine Parks, Australian Marine Parks (incl. the mining-
#      exclusion pattern swatch), and Survey design - runs much longer
#      down the left side than the two-group legend the other five
#      networks use, and reaches into the default bottom-left corner
#      where the size key sits. Given `make_network_pie_map()` its own
#      `legend_pos` override for the SWC call only, moved to the
#      bottom-right of the panel instead.
#   2. SWC's xlim/ylim tightened from the old hand-picked c(106, 139) x
#      c(-40, -24) - noticeably bigger than the actual pie/park footprint,
#      leaving several degrees of blank ocean above and below every
#      render - to c(107, 137) x c(-38, -29). CHECK against the actual
#      northernmost/southernmost SWC pies and adjust further if anything
#      gets clipped.
#   3. `pad` for the other five (auto bbox-derived) networks dropped from
#      the function default of 3 to 1.5, passed explicitly in Section 9 -
#      3 degrees of padding on every side was reading as a lot of empty
#      margin, most visibly above the panel, for tightly clustered
#      networks.
#   4. Section 7/8/8b theme()s: legend.title bumped to size 11, legend.text
#      to size 9 (previously default ggplot sizes), for readability at the
#      report's print size.
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 10 (this version):
#   1. Legend LABEL bug fixed in pie_layer() (Section 6): scale_fill_
#      manual()'s `labels` was a static, position-dependent vector built
#      from names(palette). ggplot's actual rendered legend breaks aren't
#      guaranteed to keep that order once zero-value categories get
#      dropped (drop = TRUE default), so a legend swatch could end up
#      captioned with the WRONG category - e.g. a park with only
#      mono-BRUV data rendering with a "stereo-BRUV" label. Confirmed
#      against the CSV across 16+ rendered maps. Fix: `labels` is now a
#      FUNCTION of `breaks` (`labels = function(breaks) gsub("\\.", " ",
#      breaks)`), so the label is always derived from whatever breaks
#      ggplot actually uses, regardless of internal ordering/dropped
#      levels.
#   2. Pie-SIZE legend fixed to be true-to-scale. It previously lived in a
#      separate ggplot object stretched into an arbitrary inset box
#      (patchwork::inset_element()), with no link to the main panel's
#      actual degrees-per-inch - a "200" reference circle could render
#      bigger than a real 220-site pie purely because of that mismatch
#      (caught via the North-west BOSS group: two real pies at 105/220
#      sites, reference key looked nowhere close to matching). Fix:
#      `pie_size_legend_layer()` draws the reference circles directly IN
#      the map's own coord_sf() panel, in the same degree units as the
#      real pies' `r` - guaranteeing they're always true-to-scale. This
#      replaces `pie_size_legend()` + `inset_element()` everywhere
#      (Sections 7, 8, 8b). Tried keeping it inside the ggplot legend
#      column via a physically-calibrated inset box first, but that
#      didn't reproduce reliably (see conversation record) - it now lives
#      on the map itself, in a corner chosen per network to avoid
#      overlapping real park polygons/pies (`size_anchor` argument).
# ------------------------------------------------------------------------
#
# ------------------------------------------------------------------------
# UPDATE 11 (this version):
#   1. Size-key anchor corners retuned per network after checking renders:
#      North -> bottomleft, North-west -> bottomright, Temperate East ->
#      bottomright, South-west -> bottomleft (was bottomright), Coral Sea
#      -> bottomleft, Indian Ocean Territories -> topleft (least certain -
#      its two island clusters sit at opposite horizontal extremes, so
#      check this one first).
#   2. Indian Ocean Territories' bbox padding (`pad`) bumped from the
#      shared 1.5 default to 4, to open up more clear space around
#      whichever corner the size key sits in - it's the most spatially
#      awkward network (Christmas Island + Cocos (Keeling) Islands,
#      ~900km apart over open ocean).
#   3. Pie sizes brought down globally, not just for the three previously-
#      crowded networks: national min_r/max_r 0.5/3 -> 0.4/2.3, SWC and
#      the default for the other-five-networks 0.25/2 -> 0.2/1.5, uvc/rov
#      network_size 0.15/0.7 -> 0.12/0.55. North-west, Coral Sea, and
#      Temperate East (already overridden smaller for crowding) cut
#      further again, roughly another 20-25%.
# ------------------------------------------------------------------------
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
pie_size_legend_layer <- function(scale_info, xlim, ylim, ref_totals = NULL,
                                  unit_label = "sites",
                                  anchor = c("bottomleft", "bottomright",
                                             "topleft", "topright"),
                                  pad_frac = 0.04) {
  anchor <- match.arg(anchor)
  if (scale_info$max_total <= 0) return(list())

  if (is.null(ref_totals)) {
    brks <- scales::breaks_extended(n = 3)(c(0, scale_info$max_total))
    ref_totals <- sort(unique(round(brks[brks > 0 & brks <= scale_info$max_total])))
    if (length(ref_totals) == 0) ref_totals <- round(scale_info$max_total)
  }

  ref_r <- radius_for_totals(ref_totals, scale_info)   # same units as the real pie r
  gap   <- max(ref_r) * 2.4
  x_pad <- diff(xlim) * pad_frac
  y_pad <- diff(ylim) * pad_frac

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
    geom_text(data = key, aes(x = x, y = y - r * 1.3, label = total),
              size = 3, inherit.aes = FALSE),
    annotate("text", x = x0 - max(ref_r) * 0.2,
             y = y0 + max(ref_r) * 1.6,
             label = paste0("Pie size \u2248 ", unit_label),
             hjust = 0, size = 3)
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
                                 width = 14, height = 6.5) {

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
          legend.title = element_text(size = 14),
          legend.text  = element_text(size = 12),
          legend.key.size = unit(1, "lines"),
          legend.spacing.y = unit(2, "pt"),
          legend.box.spacing = unit(4, "pt"),
          legend.margin = margin(2, 2, 2, 2),
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
  "North Marine Parks Network"            = "topleft",    # was bottomleft
  "North-west Marine Parks Network"       = "topleft",    # was bottomright
  "Coral Sea Marine Park"                 = "bottomleft",
  "Indian Ocean Territories Marine Parks" = "topleft",
  "Temperate East Marine Parks Network"   = "bottomright"
)

network_pad_overrides <- list(
  "Indian Ocean Territories Marine Parks" = 4
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
