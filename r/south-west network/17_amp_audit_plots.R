###
# Project: NESP 5.6 Project - South west Corner Report
# Task:    Survey-effort pie chart overlays (national + network level)
# Author:  Abbey Gibbons
# Note:    SELF-CONTAINED - loads its own spatial data, does not require the
#          main zone-map script to have been run first. Uses the same file
#          paths as that script, so if your data folder is the same, it will
#          just work; otherwise adjust the paths in section 1.
#
# Outputs:
#   1. National overview map with one pie per marine REGION (Image 1 & 2)
#   2. Network-level map with one pie per marine PARK / grouping (Image 3)
#   Both repeated per survey method family (BRUV, UVC, ROV, drop camera, BOSS)
###

# ==============================================================================
# 0. ASSUMPTIONS TO CONFIRM BEFORE TRUSTING THE OUTPUT
# ==============================================================================
# - "UVC split by RLS/AIMS" -> I've only counted platform == "RLS-UVC" or
#   "AIMS-UVC". The sheet also has "JCU-UVC", "UVC", "RLS-UVC (modified)" for
#   a handful of rows - currently EXCLUDED from the uvc pies. Decide whether
#   these should be folded into RLS/AIMS or shown as a 3rd/4th slice.
# - "BOSS/horizontal drop camera" -> I've mapped this to platform ==
#   "stereo-BOSS" (the only BOSS-type platform in the sheet). Confirm this is
#   what Tim means, since he names it separately from "drop camera (downward
#   facing)" (mono downwards), which I mapped literally.
# - The spreadsheet's Network list is: Coral Sea, Indian Ocean Territories,
#   North, North-west, South-west, Temperate East. There is NO "South-east
#   Marine Parks Network" row in this sheet, so Macquarie Island / the SE
#   network Tim asks about in Image 1 has no data here yet - the national
#   pie map will simply have no pie in that region until that data exists.
# - Name-matching between the CSV's "Australian Marine Park" column (e.g.
#   "South-west Corner Marine Park (western arm)") and the shapefile's
#   `name` field (e.g. "South-west Corner") needs to be checked against your
#   actual shapefile - see clean_amp_name() below, adjust the regex to match
#   however your shapefiles are actually named.
# ==============================================================================

library(tidyverse)
library(sf)
library(scatterpie)
library(ggnewscale)
library(ggpattern)
library(scales)

# ==============================================================================
# 1. LOAD SPATIAL DATA (self-contained - same sources as the main zone-map
#    script; adjust these paths if your project layout differs)
# ==============================================================================
sf_use_s2(TRUE)

e <- ext(106.0, 145.0, -45.0, -22.0)  # widened below for the national plot

data_dir <- "data/south-west network"

# Aus outline
aus <- st_read(file.path(data_dir, "spatial/shapefiles/STE_2021_AUST_GDA2020.shp")) %>%
  st_make_valid()

# CAPAD Marine 2024 - source of Commonwealth zone RES_NUMBER labels (unused
# directly in this script, but kept for consistency / future label needs)
capad <- st_read(file.path(data_dir, "spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2024_-_Marine.shp"))

# Marine parks (Commonwealth + State)
# NOTE: previously filtered down to a hardcoded list of ~27 south/south-west
# named parks here, which meant every plot - including the national one -
# only ever showed that subset. That filter is removed so the national plot
# shows every park in this shapefile; the SWC network-level plot still ends
# up showing only its own parks, since it separately filters by
# `amps_in_network` (from the survey CSV) later in the script.
# CAVEAT: the shapefile itself is named
# "south-and-western-australia_marine-parks-all.shp" - if it only actually
# contains South/Western Australian parks (not east coast, Coral Sea, GBR,
# etc.), you'll need to point this at a genuinely national AMP shapefile to
# get true Australia-wide coverage.
marine_parks <- st_read(file.path(data_dir, "spatial/shapefiles/south-and-western-australia_marine-parks-all.shp"))

marine_parks <- marine_parks %>%
  dplyr::mutate(
    zone = dplyr::if_else(
      zone == "Special Purpose Zone" & stringr::str_detect(zone_type, "Mining Exclusion"),
      "Special Purpose Zone (Mining Exclusion)",
      zone
    )
  )

amp_zone_levels <- c("National Park Zone",
                     "Habitat Protection Zone",
                     "Multiple Use Zone",
                     "Special Purpose Zone",
                     "Special Purpose Zone (Mining Exclusion)")

marine_parks_amp <- marine_parks %>%
  dplyr::filter(epbc %in% "Commonwealth") %>%
  dplyr::mutate(zone = factor(zone, levels = amp_zone_levels),
                # Mining Exclusion gets a hatched pattern on top of its fill
                # colour so it reads clearly against the plain Special
                # Purpose Zone it sits inside - requires the ggpattern
                # package (geom_sf_pattern below).
                pattern_type = dplyr::if_else(
                  zone == "Special Purpose Zone (Mining Exclusion)",
                  "stripe", "none"
                ))

# Colour lookup, same simple scheme as the network-scale panels (Tim's
# request in Image 1) - built straight from the shapefile's own `colour`
# column so it always matches the rest of the report
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
    zone = case_when(
      zone == "Reef Observation Area"   ~ "Sanctuary Zone",
      zone == "National Park Zone"      ~ "Sanctuary Zone",
      zone == "Habitat Protection Zone" ~ "Recreational Use Zone",
      TRUE                              ~ zone
    ),
    colour = case_when(
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
  # "Sites" can be a comma-separated list e.g. "100, 110" (multiple
  # deployments/years) - sum to a single site count per row
  mutate(sites_n = purrr::map_dbl(
    sites, ~ sum(as.numeric(str_split(.x, ",\\s*")[[1]]), na.rm = TRUE)
  )) %>%
  select(network, amp, platform, design, sites_n)

# Strip the AMP suffixes so names can be matched to shapefile polygons.
# ADJUST this to match how your shapefile actually spells things.
clean_amp_name <- function(x) {
  x %>%
    str_remove(" \\(eastern arm\\)| \\(western arm\\)| \\(offshore\\)") %>%
    str_remove(" Marine Park$")
}

survey <- survey %>%
  mutate(amp_group = amp,                 # keep raw value - carries the
         # (eastern/western arm/offshore)
         # split Tim asked for, already
         # present in the spreadsheet!
         amp_clean = clean_amp_name(amp))  # for matching to shapefile `name`

# ==============================================================================
# 3. METHOD GROUPINGS (per Tim's comment on Image 2)
# ==============================================================================
# Each pie = one method family, sliced by platform x design(Preferential/
# Representative). Colours: dark = Preferential, light = Representative,
# within a platform's hue.
method_groups <- list(

  bruv = list(
    platforms = c("stereo-BRUV", "mono-BRUV"),
    palette = c("stereo-BRUV.Preferential"    = "#b2182b",
                "stereo-BRUV.Representative"  = "#f4a9a0",
                "mono-BRUV.Preferential"      = "#2166ac",
                "mono-BRUV.Representative"    = "#a6c8e0"),
    label = "BRUV (stereo + mono)"
  ),

  uvc = list(
    platforms = c("RLS-UVC", "AIMS-UVC"),   # see assumption note in section 0
    palette = c("RLS-UVC.Preferential"    = "#b2182b",
                "RLS-UVC.Representative"  = "#f4a9a0",
                "AIMS-UVC.Preferential"   = "#2166ac",
                "AIMS-UVC.Representative" = "#a6c8e0"),
    label = "UVC (RLS vs AIMS)"
  ),

  rov = list(
    platforms = c("stereo-ROV", "mono-ROV"),
    palette = c("stereo-ROV.Preferential"   = "#b2182b",
                "stereo-ROV.Representative" = "#f4a9a0",
                "mono-ROV.Preferential"     = "#2166ac",
                "mono-ROV.Representative"   = "#a6c8e0"),
    label = "ROV (stereo + mono)"
  ),

  drop_camera = list(
    platforms = c("drop camera (downward facing)"),
    palette = c("drop camera (downward facing).Preferential"   = "#b2182b",
                "drop camera (downward facing).Representative" = "#f4a9a0"),
    label = "Drop camera (mono, downward)"
  ),

  boss = list(
    platforms = c("stereo-BOSS"),           # see assumption note in section 0
    palette = c("stereo-BOSS.Preferential"   = "#b2182b",
                "stereo-BOSS.Representative" = "#f4a9a0"),
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

  # guarantee every expected column exists, even if zero everywhere
  missing_cols <- setdiff(names(grp$palette), names(wide))
  wide[missing_cols] <- 0

  wide %>%
    mutate(total = rowSums(across(all_of(names(grp$palette)))))
}

# ==============================================================================
# 5. SPATIAL CENTRES FOR THE PIES
# ==============================================================================
# 5a. Network centres (national plot) - dissolve all AMP polygons that belong
#     to each network (via the lookup implicit in `survey`) and take the
#     centroid of the union.
network_lookup <- survey %>% distinct(network, amp_clean)

network_centres <- purrr::map_dfr(unique(network_lookup$network), function(net) {
  amps_in_net <- network_lookup$amp_clean[network_lookup$network == net]
  matched <- marine_parks_amp %>% filter(name %in% amps_in_net)
  if (nrow(matched) == 0) return(tibble(network = net, X = NA_real_, Y = NA_real_))
  geom <- matched %>% st_union() %>% st_centroid() %>% st_coordinates()
  tibble(network = net, X = geom[1, "X"], Y = geom[1, "Y"])
}) %>%
  filter(!is.na(X))  # drop networks with no matching polygons in this shapefile

# NOTE: no South-east network in the data yet (see section 0) - once that
# data exists, add its polygon source and it will flow through automatically.

# 5b. AMP / group centres (network-level plot). For most parks this is a
#     straight centroid; for South-west Corner's three arms, replace with
#     hand-placed points so the three pies don't overlap (Tim's "Western,
#     Eastern and offshore" split in Image 3) - EDIT these coordinates to
#     taste once you see the draft.
amp_centres <- marine_parks_amp %>%
  st_drop_geometry() %>%
  distinct(name) %>%
  left_join(
    marine_parks_amp %>% group_by(name) %>% summarise(geometry = st_union(geometry)) %>%
      st_centroid() %>% mutate(X = st_coordinates(.)[, 1], Y = st_coordinates(.)[, 2]) %>%
      st_drop_geometry(),
    by = "name"
  )

swc_manual_centres <- tribble(
  ~amp_group,                                        ~X,      ~Y,
  "South-west Corner Marine Park (western arm)",     114.9,  -34.0,
  "South-west Corner Marine Park (eastern arm)",     120.6,  -35.2,
  "South-west Corner Marine Park (offshore)",        117.5,  -37.5
)

amp_group_centres <- survey %>%
  distinct(amp_group, amp_clean) %>%
  left_join(amp_centres, by = c("amp_clean" = "name")) %>%
  # overwrite the three SWC rows with the manual placements
  rows_update(swc_manual_centres, by = "amp_group")

# ==============================================================================
# 6. PIE LAYER (shared by both plot levels)
# ==============================================================================
# CHANGED: radius is no longer `sqrt(total) * scale_factor` applied directly
# to raw site counts (that blew the BOSS/BRUV pies up to several degrees
# wide - bigger than the whole map - because their `total` is in the
# hundreds, while ROV/UVC totals are only 1-3). Instead we rescale
# sqrt(total) onto a fixed radius range (in degrees) that's appropriate for
# the extent of the plot, so every method group - regardless of how many
# sites it has - produces pies of a sane, comparable size. Rows with
# total == 0 are dropped so empty parks don't get a phantom dot.
#
# min_r/max_r should be tuned to the plot's coordinate extent:
#   - national plot spans ~57 deg lon x ~22 deg lat  -> bigger min/max
#   - network plot spans ~15 deg lon x ~12 deg lat    -> smaller min/max
add_pies <- function(pie_data, palette, min_r = 0.15, max_r = 0.6) {

  pie_data <- pie_data %>% filter(total > 0)

  if (nrow(pie_data) > 0) {
    max_total <- max(pie_data$total, na.rm = TRUE)
    pie_data <- pie_data %>%
      mutate(r = if (max_total > 0) {
        scales::rescale(sqrt(total), to = c(min_r, max_r), from = c(0, sqrt(max_total)))
      } else {
        min_r
      })
  } else {
    pie_data$r <- numeric(0)
  }

  list(
    ggnewscale::new_scale_fill(),
    scatterpie::geom_scatterpie(
      data       = pie_data,
      aes(x = X, y = Y, r = r),
      cols       = names(palette),
      colour     = "white",
      linewidth  = 0.15
    ),
    scale_fill_manual(name = "Survey effort", values = palette,
                      labels = names(palette))
  )
}

# ==============================================================================
# 7. NATIONAL PLOT (Image 1 + 2)
# ==============================================================================
# Simple base map, same fill scheme as the network-scale panels
# (amp_zone_colours), zoomed to include Macquarie Is. (~158.9E, -54.6S) once
# South-east network data is available.
make_national_pie_map <- function(group_name, save_name = NULL,
                                  width = 10, height = 7) {

  grp <- method_groups[[group_name]]
  pie_data <- build_pie_data(group_name, level = "network") %>%
    left_join(network_centres, by = "network") %>%
    filter(!is.na(X))

  p <- ggplot() +
    geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.1) +
    geom_sf_pattern(data = marine_parks_amp,
                    aes(fill = zone, pattern = pattern_type),
                    colour = NA, alpha = 0.9,
                    # white stripes on the zone's own fill colour, to match
                    # the reference style (Image 3) rather than a dark
                    # hatch on a pale fill
                    pattern_fill = "white", pattern_colour = NA,
                    pattern_density = 0.5, pattern_spacing = 0.035,
                    pattern_angle = 45, pattern_size = 0.6) +
    scale_pattern_manual(values = c(none = "none", stripe = "stripe"),
                         guide = "none") +
    # override.aes forces the Mining Exclusion legend swatch to render with
    # the same stripe pattern as the polygon, instead of a flat colour chip
    scale_fill_manual(name = "Australian Marine Parks", values = amp_zone_colours,
                      guide = guide_legend(override.aes = list(
                        pattern = ifelse(amp_zone_levels ==
                                           "Special Purpose Zone (Mining Exclusion)",
                                         "stripe", "none")))) +
    # national plot: bigger extent -> bigger radius range
    add_pies(pie_data, grp$palette, min_r = 0.15, max_r = 0.72) +
    coord_sf(xlim = c(105, 162), ylim = c(-56, -8), expand = FALSE) +
    labs(title = grp$label, x = NULL, y = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          legend.position = "left",
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
                                 xlim, ylim, width = 10, height = 8) {

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
                    pattern_fill = "white", pattern_colour = NA,
                    pattern_density = 0.5, pattern_spacing = 0.035,
                    pattern_angle = 45, pattern_size = 0.6) +
    scale_pattern_manual(values = c(none = "none", stripe = "stripe"),
                         guide = "none") +
    scale_fill_manual(name = "Australian Marine Parks", values = amp_zone_colours,
                      guide = guide_legend(override.aes = list(
                        pattern = ifelse(amp_zone_levels ==
                                           "Special Purpose Zone (Mining Exclusion)",
                                         "stripe", "none")))) +
    new_scale_fill() +
    geom_sf(data = net_state, aes(fill = zone), colour = NA, alpha = 0.5) +
    scale_fill_manual(name = "State Marine Parks",
                      values = with(net_state, setNames(colour, zone))) +
    # network plot: smaller extent -> smaller radius range
    add_pies(pie_data, grp$palette, min_r = 0.07, max_r = 0.28) +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = paste(network_name, "-", grp$label), x = NULL, y = NULL) +
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
# CHANGED: output paths now point at the project's
# plots/network/spatial/AMP_audit folder (per the folder structure you
# shared) instead of a local "plots/national" / "plots/network". Adjust the
# root below if your working directory isn't the project root.
audit_root       <- "plots/network/spatial/AMP_audit"
national_out_dir <- file.path(audit_root, "national")
network_out_dir  <- file.path(audit_root, "network")

for (g in names(method_groups)) {
  make_national_pie_map(g, save_name = paste0("national-", g, "-pies"))
}

for (g in names(method_groups)) {
  make_network_pie_map(
    group_name   = g,
    network_name = "South-west Marine Parks Network",
    xlim         = c(108, 123),
    ylim         = c(-39, -27),
    save_name    = paste0("swc-", g, "-pies")
  )
}

# ==============================================================================
# End of script
# ==============================================================================
