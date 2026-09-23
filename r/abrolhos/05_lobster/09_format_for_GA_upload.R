###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Abrolhos lobster pot survey (all years)
# Task:    Build GlobalArchive-ready Points and count CSVs from the tidied
#          pots/measurements tables, using the raw GPS exports to recover a
#          real date_time and to assign opcodes.
###
#
# For each year this produces two files:
#
# 1. <campaign-prefix>_Points.csv
#    campaignid, opcode, date_time, longitude_dd, latitude_dd, depth_m, successful, status
#    - Every pot deployment gets an opcode now, successful or not (an
#      unsuccessful pot's overall catch count isn't trusted, but individual
#      lobsters can still have been measured from it -- see build_opcode_lookup).
#    - date_time is local WA time (+08:00), built from a separate GPS export
#      that has real retrieval timestamps (the tidied pots CSV only has a
#      date, no time-of-day).
#    - status is Fished / No-take, derived from zone_type ("National Park
#      Zone" -> No-take, anything else -> Fished).
#    - depth_m is filled from missing_depth_overrides for any row missing a
#      recorded depth, using values looked up by hand from the AusBathyTopo
#      raster (see the BATHY section at the bottom of this script).
#
# 2. <campaign-prefix>_count.csv
#    campaignid, opcode, stage, count, family, genus, species, code
#    (long format: one row per opcode per stage that was actually present)
#    - Includes both successful and unsuccessful pot deployments -- an
#      unsuccessful pot's overall catch count wasn't trusted in the field,
#      but individual lobsters measured from it still count here. A pot
#      with zero measurements (whether successful or not) simply produces
#      no rows, same as any other zero-catch case (see below). This is a
#      deliberate departure from lobster-catch-per-pot.csv, which excludes
#      unsuccessful pots entirely.
#    - Each opcode is split into up to four rows -- stage "F" (female), "M"
#      (male), and for lobsters whose sex wasn't recorded, "AD" (adult,
#      carapace length over ad_threshold_mm) or "J" (juvenile, at or below
#      that threshold).
#    - opcode is "<pot_number>.<day_index>", where day_index is the
#      chronological rank (1, 2, 3...) of that pot's sampling dates within
#      the year -- e.g. "4.2" = pot 4, second day it was sampled that year.
#    - counts come from the measurements CSV. A stage with zero lobsters at
#      an opcode is left out entirely (no count=0 rows) -- so an opcode may
#      have anywhere from 0 to 4 rows, and an opcode with nothing caught at
#      all won't appear in this file. If you need every opcode represented
#      even at zero catch, join against the metadata file's opcode list
#      instead of relying on this file alone.
#    - family/genus/species/code are constant for this file (every
#      record is Western Rock Lobster) -- see lobster_taxon/lobster_caab
#      below.
#
# 3. <campaign-prefix>_length.csv
#    campaignid, opcode, family, genus, species, code, stage, count, length_mm,
#    precision_mm, range_mm, rms_mm
#    - One row per distinct (opcode, stage, length_mm) combination -- almost
#      always one row per individual lobster (count = 1), but two lobsters
#      from the same opcode with the same stage AND the same length_mm
#      collapse into a single row with count = 2 (or more).
#    - stage uses the same F/M/AD/J rule as the count CSV (see
#      assign_stage()), so the two files are always consistent with each
#      other.
#    - precision_mm/range_mm/rms_mm are intentionally left blank.
#    - A measurement whose pot deployment has no opcode (i.e. the pot was
#      marked unsuccessful) is written with a BLANK opcode and flagged with
#      a warning when the script runs -- these need a manual decision about
#      whether they should be linked to an opcode anyway, dropped, or left
#      blank. See the warning message for which rows are affected.
#
# WHAT YOU NEED TO EDIT FOR A NEW YEAR
# -------------------------------------
# - Add an entry to years_cfg (below) pointing at that year's pots CSV,
#   measurements CSV, and a GPS/metadata export that has real timestamps.
# - The metadata CSV's shape varies by source (see load_time_metadata_*
#   below) -- you may need to write a new loader function for a new export
#   format, following the pattern of the two already here.
# - missing_date_overrides handles rows where date_retrieved is blank in the
#   tidied pots CSV. This is a manual, per-row decision (see comments) --
#   there's no way to fully automate it, since it depends on what actually
#   makes sense for that specific pot's timeline. Leave it with no rows for
#   a year that doesn't need it.
#
# WHY SOME THINGS ARE HARDCODED
# ------------------------------
# Real field data is messy. missing_date_overrides encodes manual decisions
# about how to fill in a couple of rows with no recorded retrieval time at
# all. If a future year has this same problem, add rows to the tibble rather
# than trying to make the fallback fully automatic -- an automatic guess can
# silently create duplicate opcodes (this happened during development: pot
# 4's neighbour-based guess collided with pot 4's own other deployment, and
# only a human could tell that from the data).

rm(list = ls())

library(tidyverse)
library(lubridate)
library(sf)

clean_names_ <- function(df) {
  # Small local stand-in for janitor::clean_names() (lower_snake_case column
  # names), so this script has no dependency beyond the tidyverse.
  names(df) <- names(df) %>%
    str_trim() %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") %>%
    tolower() %>%
    str_replace_all("^_+|_+$", "") %>%
    str_replace_all("_+", "_")
  df
}

uploads_dir <- "data/abrolhos/raw/lobster"   # EDIT to wherever your source CSVs live
tidy_dir    <- "data/abrolhos/tidy/lobster"  # EDIT to wherever the tidied pots/measurements CSVs live
output_dir  <- "data/abrolhos/tidy/lobster"  # EDIT to wherever you want the Points/count CSVs written
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
# Per-year configuration
# ---------------------------------------------------------------------------
years_cfg <- list(
  "2025" = list(
    pots_csv             = file.path(tidy_dir, "abrolhosAMP_lobster-pots_2025.csv"),
    measurements_csv     = file.path(tidy_dir, "abrolhosAMP_lobster-measurements_2025.csv"),
    time_metadata_csv    = file.path(uploads_dir, "2025/2025_Yamatji_Lobster.csv"),
    time_metadata_format = "yamatji"
  ),
  "2026" = list(
    pots_csv             = file.path(tidy_dir, "abrolhosAMP_lobster-pots_2026.csv"),
    measurements_csv     = file.path(tidy_dir, "abrolhosAMP_lobster-measurements_2026.csv"),
    time_metadata_csv    = file.path(uploads_dir, "2026/pot_metadata_0.csv"),
    time_metadata_format = "pot_metadata"
  )
)

# zone_type lookup, aligned by row position to each year's pots CSV.
# The all-years file's row order is confirmed identical to pots_2025.csv
# rows followed by pots_2026.csv rows (see allyears_split below).
allyears_csv   <- file.path(tidy_dir, "abrolhosAMP_lobster-pots_all-years.csv")
allyears_split <- list("2025" = c(1, 148), "2026" = c(149, 287))

# Manual fixes for rows where date_retrieved is blank in the tidied pots
# CSV. Each row is decided by hand after looking at the raw GPS export and
# checking it doesn't collide with another date already recorded for that
# same pot -- see fill_missing_dates() for how these get applied.
missing_date_overrides <- tribble(
  ~year, ~pot_number, ~date_retrieved,
  2025,  "4",          "2025-04-10",  # date_time_set for this row is 2025-04-09 09:37:20
  # local in the raw GPS export; pots are retrieved the
  # day after they're set, so 2025-04-10. (Neighbour+1
  # day would give 2025-04-11, which collides with pot
  # 4's own separate 2025-04-11 deployment -- the
  # set-time is the reliable signal.)
  2025,  "15",         "2025-04-11",  # date_time_set for this row is 2025-04-10 10:04:01
  # local, so +1 day = 2025-04-11.
)

# Manual fixes for rows where depth_m is blank in the tidied pots CSV.
# Filled by hand from the AusBathyTopo bathymetry raster (see the BATHY
# section at the bottom of this script) rather than automatically, since
# the bathymetry lookup there is a one-off check, not wired into the
# metadata CSV build -- add a row here for each opcode you've confirmed a
# depth for.
missing_depth_overrides <- tribble(
  ~year, ~opcode, ~depth_m,
  2025,  "46.2",  34.5,  # no depth recorded in the field; AusBathyTopo 250m raster gives 34.5m at this position
)

# Every record in the count/length files is Western Rock Lobster -- these
# are constant for the whole file, not derived per-row.
lobster_taxon <- list(family = "Palinuridae", genus = "Panulirus", species = "cygnus")
lobster_caab  <- "28820005"

# Measurements with no recorded sex ("Unknown") are still given a stage,
# based on carapace length: above this threshold is scored "AD" (adult),
# at or below it is "J" (juvenile).
ad_threshold_mm <- 76

# Bycatch sources are a different shape every year (whatever the field
# team's raw export looks like that season), so unlike years_cfg above
# there's no shared loader -- each year gets its own build_bycatch_YYYY()
# function further down. Add a new one, in the same style, for a new year.
bycatch_cfg <- list(
  "2026" = list(tidy_csv = file.path(tidy_dir, "abrolhosAMP_lobster-bycatch_2026.csv"),
                comment_source_csv = file.path(uploads_dir, "2026/count_and_length_1.csv"),
                pot_metadata_csv = years_cfg[["2026"]]$time_metadata_csv)
)

# Species identifications, from field comments, agreed by hand (see the
# taxon_from_comment() helper for how these get matched). code is
# left NA where none was given (e.g. hermit crab, unidentified to family).
bycatch_taxa <- tribble(
  ~keyword,     ~family,          ~genus,        ~species,       ~code,
  "red throat", "Lethrinidae",    "Lethrinus",   "miniatus",     "37351009",
  "redthroat",  "Lethrinidae",    "Lethrinus",   "miniatus",     "37351009",
  "chromis",    "Pomacentridae",  "Chromis",     "spp",          "37372907",
  "turbo",      "Turbinidae",     "Turbo",       "spp",          "24045901",
  "leather",    "Monacanthidae",  "Meuschenia",  "hippocrepis",  "37465004",
  "scallop",    "Pectinidae",     "unknown",     "spp",          "23270000",
)
octopus_taxon <- list(family = "Octopodidae", genus = "unknown", species = "spp", code = "23659921")


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

zone_to_status <- function(zone_type) {
  case_when(
    is.na(zone_type)                       ~ NA_character_,
    str_detect(zone_type, "National Park") ~ "No-take",
    .default                               = "Fished"
  )
}

normalise_pot <- function(x) {
  # Pot numbers appear as int, "01", " 1", etc. across files -- normalise to
  # a plain string with no leading zeros/whitespace so joins work.
  x <- str_trim(as.character(x))
  x <- str_remove(x, "^0+")
  if_else(x == "", "0", x)
}

fill_missing_dates <- function(pots, overrides) {
  # Fill date_retrieved for rows where it's blank, using `overrides`. Falls
  # back to "neighbour date + 1 day" for any row not covered by an override,
  # but that fallback is a guess -- add an explicit override instead if you
  # hit this case.
  missing_idx <- which(is.na(pots$date_retrieved))
  for (i in missing_idx) {
    pot <- as.character(pots$pot_number[i])
    yr  <- pots$year[i]
    hit <- overrides %>% filter(year == yr, pot_number == pot)

    if (nrow(hit) == 1) {
      pots$date_retrieved[i] <- hit$date_retrieved[1]
      next
    }

    neighbour_date <- NA_character_
    for (j in c(i - 1, i + 1)) {
      if (j >= 1 && j <= nrow(pots) && !is.na(pots$date_retrieved[j])) {
        neighbour_date <- pots$date_retrieved[j]
        break
      }
    }
    if (is.na(neighbour_date)) {
      stop(sprintf(
        "Row %d (pot %s, %d) has no date and no dated neighbour -- add an entry to missing_date_overrides.",
        i, pot, yr))
    }
    fallback <- as.character(as.Date(neighbour_date) + days(1))
    message(sprintf(
      "  WARNING: no override for pot %s (%d) -- guessing %s (neighbour date + 1 day). Check this and add an override if it's wrong.",
      pot, yr, fallback))
    pots$date_retrieved[i] <- fallback
  }
  pots
}


# ---------------------------------------------------------------------------
# Time metadata loaders -- one per export format. Each returns a dataframe
# with pot_number_norm plus either (rank, dt_ret_local) or
# (date_retrieved, dt_ret_local), depending on how it should be joined.
# ---------------------------------------------------------------------------

load_time_metadata_yamatji <- function(path) {
  # Format: one row per pot per retrieval, columns include pot_number,
  # date_time_set and date_time_retrieved (both UTC). Multiple rows can
  # share a pot_number (reused across sampling days), so we rank rows within
  # each pot by their order in the file and match that same rank/position in
  # the tidied pots CSV.
  #
  # Two corrections, mirroring the original tidying script:
  # - A few rows have date_time_set and date_time_retrieved entered the
  #   wrong way around (retrieved < set) -- these are swapped back.
  # - A couple of rows have no date_time_retrieved at all. Pots are set one
  #   day and retrieved the next, so these fall back to date_time_set + 1
  #   day, at noon (no real time-of-day is available for these).
  meta <- read_csv(path, show_col_types = FALSE) %>%
    clean_names_() %>%
    mutate(
      pot_number_norm = normalise_pot(pot_number),
      set_local = with_tz(mdy_hms(date_time_set, tz = "UTC"), tzone = "Australia/Perth"),
      ret_local = with_tz(mdy_hms(date_time_retrieved, tz = "UTC"), tzone = "Australia/Perth")
    )

  swapped <- !is.na(meta$ret_local) & !is.na(meta$set_local) & meta$ret_local < meta$set_local
  new_set <- if_else(swapped, meta$ret_local, meta$set_local)
  new_ret <- if_else(swapped, meta$set_local, meta$ret_local)
  meta$set_local <- new_set
  meta$ret_local <- new_ret
  if (sum(swapped) > 0) {
    message(sprintf(
      "  yamatji loader: swapped set/retrieved times for %d row(s) (pot %s).",
      sum(swapped), paste(sort(unique(meta$pot_number_norm[swapped])), collapse = ", ")))
  }

  missing <- is.na(meta$ret_local) & !is.na(meta$set_local)
  if (sum(missing) > 0) {
    meta$ret_local[missing] <- floor_date(meta$set_local[missing], "day") + days(1) + hours(12)
    message(sprintf(
      "  yamatji loader: no retrieval time for %d row(s) -- used date_time_set + 1 day, noon, for pot(s) %s.",
      sum(missing), paste(sort(unique(meta$pot_number_norm[missing])), collapse = ", ")))
  }

  meta %>%
    mutate(dt_ret_local = ret_local) %>%
    group_by(pot_number_norm) %>%
    mutate(rank = row_number() - 1) %>%
    ungroup() %>%
    select(pot_number_norm, rank, dt_ret_local)
}

load_time_metadata_pot_metadata <- function(path) {
  # Format: a large multi-project export. Rows for this campaign have an
  # opcode starting with 6 digits (a set-date code); everything else (test
  # rows, other projects) is discarded. Join key is pot_number + local
  # retrieval date (this format doesn't need positional ranking -- pot_number
  # + date uniquely identifies every row here).
  read_csv(path, show_col_types = FALSE) %>%
    clean_names_() %>%
    mutate(opcode_s = str_trim(as.character(opcode))) %>%
    filter(str_detect(opcode_s, "^\\d{6}")) %>%
    mutate(
      pot_number_norm = normalise_pot(pot_number),
      dt_ret_local = with_tz(mdy_hms(date_time_retrieved, tz = "UTC"), tzone = "Australia/Perth"),
      date_retrieved = as.character(as_date(dt_ret_local))
    ) %>%
    select(pot_number_norm, date_retrieved, dt_ret_local)
}

time_metadata_loaders <- list(
  yamatji      = load_time_metadata_yamatji,
  pot_metadata = load_time_metadata_pot_metadata
)


# ---------------------------------------------------------------------------
# Shared pot loading / opcode assignment
# ---------------------------------------------------------------------------

load_and_prepare_pots <- function(cfg, year, overrides) {
  # Read a year's pots CSV, normalise pot_number, and fill any missing
  # date_retrieved values. Shared by both build_points_csv and
  # build_count_csv so the two files always agree on dates and opcodes.
  read_csv(cfg$pots_csv, show_col_types = FALSE) %>%
    clean_names_() %>%
    mutate(
      year = year,
      pot_number = str_trim(as.character(pot_number)),
      pot_number_norm = normalise_pot(pot_number),
      date_retrieved = as.character(date_retrieved)
    ) %>%
    fill_missing_dates(overrides)
}

build_opcode_lookup <- function(pots) {
  # opcode = "<pot_number>.<day_index>", where day_index is the chronological
  # rank (1, 2, 3...) of ALL of that pot's sampling dates within the year --
  # successful AND unsuccessful deployments both count towards the ranking,
  # so every physical pot deployment gets an opcode (an unsuccessful pot can
  # still have real measurements attached to it, e.g. individual lobsters
  # were measured even though the pot's total catch was flagged unreliable).
  # build_count_csv filters down to successful-only opcodes afterwards, for
  # the aggregate counts -- this function itself does not filter.
  pots %>%
    distinct(pot_number, date_retrieved) %>%
    arrange(suppressWarnings(as.numeric(pot_number)), pot_number, date_retrieved) %>%
    group_by(pot_number) %>%
    mutate(opcode = paste0(pot_number, ".", row_number())) %>%
    ungroup() %>%
    select(pot_number, date_retrieved, opcode)
}


# ---------------------------------------------------------------------------
# Points CSV
# ---------------------------------------------------------------------------

build_points_csv <- function(year, cfg, zone_lookup, overrides, depth_overrides) {
  pots <- load_and_prepare_pots(cfg, year, overrides)
  opcode_lookup <- build_opcode_lookup(pots)
  pots <- pots %>% left_join(opcode_lookup, by = c("pot_number", "date_retrieved"))

  loader <- time_metadata_loaders[[cfg$time_metadata_format]]
  meta <- loader(cfg$time_metadata_csv)

  if ("rank" %in% names(meta)) {
    pots <- pots %>% group_by(pot_number_norm) %>% mutate(rank = row_number() - 1) %>% ungroup()
    merged <- pots %>% left_join(meta, by = c("pot_number_norm", "rank"))
  } else {
    merged <- pots %>% left_join(meta, by = c("pot_number_norm", "date_retrieved"))
  }

  merged <- merged %>%
    mutate(
      time_str = if_else(is.na(dt_ret_local), "12:00:00", format(dt_ret_local, "%H:%M:%S")),
      date_time = paste0(date_retrieved, "T", time_str, "+08:00"),
      zone_type = zone_lookup,
      status = zone_to_status(zone_type)
    ) %>%
    left_join(depth_overrides %>% filter(year == !!year) %>% select(opcode, depth_override = depth_m),
              by = "opcode") %>%
    mutate(depth_m = if_else(is.na(depth_m), depth_override, depth_m))

  merged %>%
    transmute(
      campaignid = campaign,
      opcode,
      date_time,
      longitude_dd = longitude,
      latitude_dd = latitude,
      depth_m,
      successful,
      status
    )
}


# ---------------------------------------------------------------------------
# Count CSV (long format: one row per opcode per stage)
# ---------------------------------------------------------------------------

assign_stage <- function(sex, length_mm) {
  # F/M come straight from recorded sex. A lobster with no recorded sex
  # ("Unknown") is still scored AD or J based on carapace length, rather
  # than being dropped -- but only if a length was actually recorded.
  case_when(
    sex == "Female"                                  ~ "F",
    sex == "Male"                                     ~ "M",
    sex == "Unknown" & !is.na(length_mm) & length_mm > ad_threshold_mm ~ "AD",
    sex == "Unknown" & !is.na(length_mm)              ~ "J",
    .default = NA_character_
  )
}

build_count_csv <- function(year, cfg, overrides) {
  all_pots <- load_and_prepare_pots(cfg, year, overrides)
  opcode_lookup <- build_opcode_lookup(all_pots)  # ranked over ALL deployments, see note above
  pots <- all_pots %>%
    left_join(opcode_lookup, by = c("pot_number", "date_retrieved"))
  # NOTE: unsuccessful pots are included here (not filtered out) -- a pot
  # flagged unsuccessful in the field can still have real measurements
  # attached (its total catch just wasn't trusted), and those are counted
  # here same as any other. A pot with zero measurements, successful or
  # not, naturally produces no rows once zero-count stages are filtered
  # out below, so nothing extra needs to be excluded explicitly.

  meas <- read_csv(cfg$measurements_csv, show_col_types = FALSE) %>%
    clean_names_() %>%
    mutate(pot_number = str_trim(as.character(pot_number)),
           date_retrieved = as.character(date_retrieved),
           stage = assign_stage(sex, carapace_length_mm))

  undropped <- sum(is.na(meas$stage))
  if (undropped > 0) {
    message(sprintf(
      "  %d count: %d measurement(s) excluded (Unknown sex, no recorded length -- can't classify AD vs J).",
      year, undropped))
  }

  stages <- c("F", "M", "AD", "J")
  counts <- meas %>%
    filter(!is.na(stage)) %>%
    count(pot_number, date_retrieved, stage) %>%
    pivot_wider(names_from = stage, values_from = n, values_fill = 0)
  for (s in stages) {
    if (!s %in% names(counts)) counts[[s]] <- 0
  }

  wide <- pots %>%
    left_join(counts %>% select(pot_number, date_retrieved, all_of(stages)),
              by = c("pot_number", "date_retrieved")) %>%
    mutate(across(all_of(stages), ~ replace_na(.x, 0))) %>%
    transmute(campaignid = campaign, opcode, pot_number, date_retrieved,
              F = F, M = M, AD = AD, J = J) %>%
    arrange(suppressWarnings(as.numeric(pot_number)), pot_number, date_retrieved)

  dupes <- sum(duplicated(wide$opcode))
  if (dupes > 0) {
    stop(sprintf(
      "%d: %d duplicate opcode(s) in count data -- check missing_date_overrides for this year.",
      year, dupes))
  }

  wide %>%
    pivot_longer(cols = all_of(stages), names_to = "stage", values_to = "count") %>%
    filter(count > 0) %>%
    arrange(suppressWarnings(as.numeric(pot_number)), pot_number, date_retrieved, stage) %>%
    mutate(family = lobster_taxon$family, genus = lobster_taxon$genus,
           species = lobster_taxon$species, code = lobster_caab) %>%
    select(campaignid, opcode, stage, count, family, genus, species, code)
}


# ---------------------------------------------------------------------------
# Length CSV (one row per individually measured lobster)
# ---------------------------------------------------------------------------

build_length_csv <- function(year, cfg, overrides) {
  all_pots <- load_and_prepare_pots(cfg, year, overrides)
  opcode_lookup <- build_opcode_lookup(all_pots)  # ranked over ALL deployments, see note above

  meas <- read_csv(cfg$measurements_csv, show_col_types = FALSE) %>%
    clean_names_() %>%
    mutate(pot_number = str_trim(as.character(pot_number)),
           date_retrieved = as.character(date_retrieved)) %>%
    left_join(opcode_lookup, by = c("pot_number", "date_retrieved"))

  missing <- meas %>% filter(is.na(opcode))
  if (nrow(missing) > 0) {
    message(sprintf(
      "  %d length: WARNING %d measurement(s) have no matching opcode at all (pot/date not found anywhere in the pots CSV, successful or not) -- these are written with a BLANK opcode and need checking.",
      year, nrow(missing)))
    print(missing %>% select(pot_number, date_retrieved, carapace_length_mm))
  }

  meas %>%
    mutate(stage = assign_stage(sex, carapace_length_mm)) %>%
    transmute(
      campaignid = campaign,
      opcode,
      family = lobster_taxon$family,
      genus = lobster_taxon$genus,
      species = lobster_taxon$species,
      code = lobster_caab,
      stage,
      length_mm = carapace_length_mm,
      precision_mm = NA_real_,
      range_mm = NA_real_,
      rms_mm = NA_real_
    ) %>%
    # one row per distinct (opcode, stage, length_mm) combination, with count
    # = how many individual lobsters share that exact combination -- almost
    # always 1, but collapses genuine duplicates within the same opcode.
    count(campaignid, opcode, family, genus, species, code, stage, length_mm,
          precision_mm, range_mm, rms_mm, name = "count") %>%
    arrange(suppressWarnings(as.numeric(str_extract(opcode, "^[^.]+"))), opcode) %>%
    select(campaignid, opcode, family, genus, species, code, stage, count,
           length_mm, precision_mm, range_mm, rms_mm)
}


# ---------------------------------------------------------------------------
# Bycatch CSVs -- one build_bycatch_YYYY() function per year's raw source
# format, since these aren't standardised the way the lobster pot exports
# are. Each ends by calling finalise_bycatch(), which does the shared
# aggregation into the campaignid/opcode/stage/count/family/genus/species/
# code/comment shape.
# ---------------------------------------------------------------------------

taxon_from_comment <- function(comment) {
  # Matches bycatch_taxa by keyword (case-insensitive) against a comment
  # string. Returns a one-row tibble of family/genus/species/code, or
  # NULL if nothing matched.
  comment_lower <- tolower(comment)
  for (i in seq_len(nrow(bycatch_taxa))) {
    if (str_detect(comment_lower, fixed(bycatch_taxa$keyword[i]))) {
      return(bycatch_taxa[i, c("family", "genus", "species", "code")])
    }
  }
  NULL
}

finalise_bycatch <- function(records, campaign) {
  # records: a data frame with one row per animal, columns opcode, family,
  # genus, species, code, comment. Aggregates to one row per
  # opcode+family+genus+species+code+comment, in the same column shape
  # as the count CSVs (plus comment).
  records %>%
    count(opcode, family, genus, species, code, comment, name = "count") %>%
    mutate(campaignid = campaign, stage = NA_character_) %>%
    select(campaignid, opcode, stage, count, family, genus, species, code, comment) %>%
    arrange(suppressWarnings(as.numeric(str_extract(opcode, "^[^.]+"))), opcode)
}

build_bycatch_2026 <- function(year, cfg, bycfg, overrides) {
  # Primary source: the tidied bycatch CSV (same shape/pipeline as the pots
  # and measurements CSVs -- campaign, pot_number, date_retrieved, species
  # category). This alone has no species detail beyond the 4 broad
  # categories (Bony Fish, Other Crustacean, Octopus, Other), so a small
  # comment lookup is built from the raw ArcGIS-style export (comment_source_csv,
  # joined through pot_metadata_csv via pot_ID, the same way the old
  # single-source version of this function worked) and merged in by
  # pot_number + date_retrieved + species. Verified once, by hand, that
  # every duplicate (pot_number, date_retrieved, species) group in the raw
  # export has an identical comment across all its rows, so this key is
  # safe to join on even though it isn't a per-animal unique id.
  all_pots <- load_and_prepare_pots(cfg, year, overrides)
  opcode_lookup <- build_opcode_lookup(all_pots)

  require_cols <- function(df, cols, source_label, path) {
    # Turns a cryptic "object 'x' not found" (which doesn't say which file
    # or what the file actually contains) into a clear, actionable error.
    missing <- setdiff(cols, names(df))
    if (length(missing) > 0) {
      stop(sprintf(
        "build_bycatch_2026: %s ('%s') is missing column(s): %s.\nColumns actually found: %s",
        source_label, path, paste(missing, collapse = ", "), paste(names(df), collapse = ", ")))
    }
    df
  }

  pot_meta_raw <- read_csv(bycfg$pot_metadata_csv, show_col_types = FALSE) %>%
    clean_names_() %>%
    require_cols(c("pot_id", "opcode", "pot_number", "date_time_retrieved"),
                 "pot_metadata_csv", bycfg$pot_metadata_csv)
  pot_meta <- pot_meta_raw %>%
    select(pot_id, opcode_raw = opcode, pot_number, date_time_retrieved)

  comment_lookup <- read_csv(bycfg$comment_source_csv, show_col_types = FALSE) %>%
    clean_names_() %>%
    require_cols(c("pot_id", "species", "comment"), "comment_source_csv", bycfg$comment_source_csv) %>%
    left_join(pot_meta, by = "pot_id") %>%
    filter(str_detect(str_trim(as.character(opcode_raw)), "^\\d{6}")) %>%
    mutate(
      pot_number = normalise_pot(pot_number),
      dt_ret_local = with_tz(mdy_hms(date_time_retrieved, tz = "UTC"), tzone = "Australia/Perth"),
      date_retrieved = as.character(as_date(dt_ret_local)),
      comment = str_trim(as.character(comment))
    ) %>%
    distinct(pot_number, date_retrieved, species, comment) %>%
    mutate(matched = TRUE)  # marks a real key match, vs. a genuinely blank comment for that key

  tidy <- read_csv(bycfg$tidy_csv, show_col_types = FALSE) %>%
    clean_names_() %>%
    require_cols(c("pot_number", "date_retrieved", "species", "campaign"), "tidy_csv", bycfg$tidy_csv) %>%
    mutate(pot_number = normalise_pot(pot_number),
           date_retrieved = as.character(date_retrieved),
           row_id = row_number()) %>%
    left_join(opcode_lookup, by = c("pot_number", "date_retrieved")) %>%
    left_join(comment_lookup, by = c("pot_number", "date_retrieved", "species"))

  unmatched <- tidy %>% filter(is.na(opcode))
  if (nrow(unmatched) > 0) {
    message(sprintf("  2026 bycatch: WARNING %d row(s) have no matching opcode.", nrow(unmatched)))
    print(unmatched %>% select(pot_number, date_retrieved))
  }
  no_comment_row <- tidy %>% filter(is.na(matched))
  if (nrow(no_comment_row) > 0) {
    message(sprintf(
      "  2026 bycatch: WARNING %d row(s) have no matching key at all in the raw export (pot_number/date_retrieved/species didn't line up) -- classified as unknown using just the broad category.",
      nrow(no_comment_row)))
    print(no_comment_row %>% select(pot_number, date_retrieved, species))
  }

  flagged <- list()
  records <- pmap_dfr(tidy, function(...) {
    row <- list(...)
    comment <- if (!is.na(row$comment)) str_trim(as.character(row$comment)) else ""
    species_category <- str_trim(as.character(row$species))

    if (species_category == "Octopus") {
      return(tibble(opcode = row$opcode, family = octopus_taxon$family, genus = octopus_taxon$genus,
                    species = octopus_taxon$species, code = octopus_taxon$code,
                    comment = if (comment == "") NA_character_ else comment))
    }
    if (str_detect(tolower(comment), "hermit") && !str_detect(tolower(comment), "possibly")) {
      return(tibble(opcode = row$opcode, family = "unknown", genus = "unknown", species = "unknown",
                    code = NA_character_, comment = "hermit crab"))
    }
    taxon <- taxon_from_comment(comment)
    if (!is.null(taxon)) {
      return(tibble(opcode = row$opcode, family = taxon$family, genus = taxon$genus,
                    species = taxon$species, code = taxon$code, comment = NA_character_))
    }
    # Nothing matched -- flag for manual review, write as unknown with
    # whatever text is available.
    flagged[[length(flagged) + 1]] <<- list(row_id = row$row_id, species_category = species_category, comment = comment)
    tibble(opcode = row$opcode, family = "unknown", genus = "unknown", species = "unknown",
           code = NA_character_, comment = if (comment == "") species_category else comment)
  })

  if (length(flagged) > 0) {
    message(sprintf("  2026 bycatch: %d row(s) fell through to 'unknown' and need manual review:", length(flagged)))
    for (f in flagged) {
      message(sprintf("    row %s: species='%s' comment='%s'", f$row_id, f$species_category, f$comment))
    }
  }

  campaign <- read_csv(cfg$pots_csv, show_col_types = FALSE) %>% pull(campaign) %>% first()
  finalise_bycatch(records, campaign)
}

bycatch_builders <- list("2026" = build_bycatch_2026)




write_csv_safe <- function(df, path, input_paths) {
  # Refuses to write an output file if its path exactly matches one of this
  # year's configured INPUT files -- prevents a repeat of a real incident
  # where the bycatch output happened to share a filename with its own tidy
  # input, silently overwriting real data with the script's own output on
  # the very next run.
  norm_path <- normalizePath(path, mustWork = FALSE)
  norm_inputs <- normalizePath(input_paths, mustWork = FALSE)
  if (norm_path %in% norm_inputs) {
    stop(sprintf(
      "Refusing to write '%s' -- this path is also one of this year's configured INPUT files. Writing to it would overwrite real source data. Check years_cfg / bycatch_cfg for a filename collision with output_dir.",
      path))
  }
  write.csv(df, path, row.names = FALSE, na = "")
}

allyears <- read_csv(allyears_csv, show_col_types = FALSE) %>% clean_names_()

for (year_key in names(years_cfg)) {
  year <- as.integer(year_key)
  cfg  <- years_cfg[[year_key]]
  rng  <- allyears_split[[year_key]]
  zone_lookup <- allyears$zone_type[rng[1]:rng[2]]

  input_paths <- c(cfg$pots_csv, cfg$measurements_csv, cfg$time_metadata_csv, allyears_csv)
  if (year_key %in% names(bycatch_cfg)) {
    input_paths <- c(input_paths, unlist(bycatch_cfg[[year_key]]))
  }

  points <- build_points_csv(year, cfg, zone_lookup, missing_date_overrides, missing_depth_overrides)
  points_path <- file.path(output_dir, sprintf("abrolhosAMP_lobster-pots_%d_metadata.csv", year))
  write_csv_safe(points, points_path, input_paths)
  message(sprintf("%d: wrote %s (%d rows, %d nulls)",
                  year, points_path, nrow(points), sum(is.na(points))))

  counts <- build_count_csv(year, cfg, missing_date_overrides)
  counts_path <- file.path(output_dir, sprintf("abrolhosAMP_lobster_%d_count.csv", year))
  write_csv_safe(counts, counts_path, input_paths)
  message(sprintf("%d: wrote %s (%d rows, %d nulls)",
                  year, counts_path, nrow(counts), sum(is.na(counts))))

  lengths <- build_length_csv(year, cfg, missing_date_overrides)
  lengths_path <- file.path(output_dir, sprintf("abrolhosAMP_lobster_%d_length.csv", year))
  write_csv_safe(lengths, lengths_path, input_paths)
  message(sprintf("%d: wrote %s (%d rows, %d blank opcodes)",
                  year, lengths_path, nrow(lengths), sum(is.na(lengths$opcode))))

  if (year_key %in% names(bycatch_cfg)) {
    bycatch <- bycatch_builders[[year_key]](year, cfg, bycatch_cfg[[year_key]], missing_date_overrides)
    bycatch_path <- file.path(output_dir, sprintf("abrolhosAMP_lobster_%d_bycatch_count.csv", year))
    write_csv_safe(bycatch, bycatch_path, input_paths)
    message(sprintf("%d: wrote %s (%d rows)", year, bycatch_path, nrow(bycatch)))
  }
}



#_______________________________________________________________________________
#BATHY
#_______________________________________________________________________________
points_2025 <- read_csv(
  file.path(output_dir, "abrolhosAMP_lobster-pots_2025_metadata.csv"),
  show_col_types = FALSE
)
# Read in AusBathyTopo bathymetry

bathy <- terra::rast(

  "data/abrolhos/spatial/rasters/AusBathyTopo__Australia__2024_250m_MSL_cog.tif"

)

names(bathy) <- "bathy_depth_m"

# Convert metadata to spatial points

metadata_sf <- st_as_sf(

  points_2025,

  coords = c("longitude_dd", "latitude_dd"),

  crs = 4326,

  remove = FALSE

)

# Reproject points to bathymetry CRS if needed

metadata_vect <- terra::vect(metadata_sf)

if (!terra::same.crs(metadata_vect, bathy)) {

  metadata_vect <- terra::project(metadata_vect, terra::crs(bathy))

}

# Extract bathymetry value

bathy_values <- terra::extract(bathy, metadata_vect) %>%

  dplyr::select(bathy_depth_m)

# Combine with metadata and replace missing depth_m

metadata_bathy <- bind_cols(points_2025, bathy_values) %>%

  mutate(

    depth_m_original = depth_m,

    depth_m = if_else(

      is.na(depth_m) | depth_m == "",

      as.character(bathy_depth_m),

      as.character(depth_m)

    ))

glimpse(metadata_bathy)
