###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    FSS candidate CSVs (05/06) + final models (01_fit-final-models.R)
# Task:    Build the Table C 1.1 / C 2.1 data, and shared helper functions
# Author:  Annika Leunig
# Date:    August 2026
###
# =============================================================================

library(here)
library(tidyverse)
library(mgcv)

# Locate this folder. Works in RStudio, in a plain R session, and when Quarto
# renders (Quarto sets the working directory to the document's own folder).
# rstudioapi::isAvailable() returns FALSE outside RStudio, whereas
# getActiveDocumentContext() calls verifyAvailable() and hard-errors.
if (!exists("appc_dir") || !file.exists(file.path(appc_dir, "00_config.yml"))) {
  appc_dir <- local({
    cands <- getwd()
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      p <- tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),
                    error = function(e) "")
      if (nzchar(p)) cands <- c(p, cands)
    }
    hit <- cands[file.exists(file.path(cands, "00_config.yml"))]
    if (!length(hit)) {
      stop("Could not find 00_config.yml.\n",
           "Set appc_dir <- \"<path to the appendix-C folder>\" before sourcing, ",
           "or setwd() to that folder.")
    }
    hit[1]
  })
}

config <- yaml::read_yaml(file.path(appc_dir, "00_config.yml"))
name <- config$name
park <- config$park

appc_out <- here("output", "model-output", park, "appendix-C")

# =============================================================================
# LOOKUPS - the only place labels and orderings are set
# =============================================================================

# Predictor name in the data/FSS output -> short label used in the Model column
# and on the figure axes. `Z` follows the reporting convention for depth.
# The order of this vector also sets the order terms appear in the Model string.
term_labels <- c(
  geoscience_detrended = "detrended",
  geoscience_roughness = "roughness",
  geoscience_aspect    = "aspect",
  geoscience_depth     = "Z",
  reef                 = "reef",
  year                 = "year",
  status               = "status"
)

# Axis labels on the figures (sentence case rather than the table shorthand).
# Continuous terms only - factors never become a response-curve x axis.
term_axis_labels <- c(
  geoscience_detrended = "Detrended",
  geoscience_roughness = "Roughness",
  geoscience_aspect    = "Aspect",
  geoscience_depth     = "Depth",
  reef                 = "Reef"
)

# Column order on the importance heatmaps, and panel order on the curve plots.
# `year` and `status` are omitted on purpose: the heatmap colours a term by the
# SIGN of its Spearman correlation with the response, which is undefined for an
# unordered factor. Their importance weights are still in the FSS CSVs if you
# want them reported some other way.
habitat_term_order <- c("geoscience_detrended", "geoscience_roughness",
                        "geoscience_aspect", "geoscience_depth")
fish_term_order    <- c("geoscience_detrended", "geoscience_roughness",
                        "geoscience_aspect", "geoscience_depth", "reef")

# Row order in Table C 1.1 / C 2.1 and on the heatmaps. Geographe models all six
# benthic classes. Note `seagrasses` is plural - it must match the response
# string used by 03_create-metrics_benthos.R and the FSS loop in 05.
habitat_response_order <- c("macroalgae", "sand", "seagrasses", "rock",
                            "sessile_invertebrates", "reef")
fish_response_order    <- c("species_richness", "total_abundance", "b20", "cti")

response_labels <- c(
  macroalgae            = "Macroalgae",
  sand                  = "Sediment",
  rock                  = "Bare rock",
  seagrasses            = "Seagrass",
  sessile_invertebrates = "Sessile invertebrates",
  reef                  = "Reef",
  species_richness      = "Species richness",
  total_abundance       = "Total abundance",
  b20                   = "Large Reef Fish Index*",
  cti                   = "Reef Fish Thermal Index"
)

# =============================================================================
# HELPERS
# =============================================================================

# Predictor set of a fitted gam, used to match it back to its FSS candidate row.
# s(x, by = year) reports `x` as term[1] and `year` as the by variable; the by
# variable is picked up separately from the parametric terms, which is what we
# want since `year` is also a main effect in every Geographe model.
gam_predictor_set <- function(model) {
  smooth_terms <- if (length(model$smooth) > 0) {
    vapply(model$smooth, function(s) s$term[1], character(1))
  } else character()
  parametric <- attr(terms(model), "term.labels")
  parametric <- parametric[!grepl("^s\\(", parametric)]
  sort(unique(c(smooth_terms, parametric)))
}

# Same, but numeric terms only - factor terms cannot be a response-curve x axis
smooth_predictor_set <- function(model, data) {
  tms <- gam_predictor_set(model)
  tms[vapply(tms, function(t) is.numeric(data[[t]]), logical(1))]
}

# FSS modname strings are pred.vars joined by "+"
predictor_set_from_modname <- function(modname) {
  tms <- str_split(as.character(modname), "\\+")[[1]] %>% str_trim()
  tms <- tms[nzchar(tms) & tms != "null"]
  sort(unique(tms))
}

# Pretty "detrended+aspect+Z+year" string for the Model column
label_model <- function(modname) {
  tms <- predictor_set_from_modname(modname)
  if (!length(tms)) return("null")
  tms <- tms[order(match(tms, names(term_labels)))]
  paste(dplyr::coalesce(unname(term_labels[tms]), tms), collapse = "+")
}

# Column names vary a little between FSSgam versions - find the first that fits
pick_col <- function(df, candidates, required = TRUE, what = candidates[1]) {
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) {
    if (required) {
      stop("Could not find a '", what, "' column in the FSS CSV.\n",
           "Columns present: ", paste(names(df), collapse = ", "))
    }
    return(NULL)
  }
  hit[1]
}

# Normalise one FSS candidate CSV into a tidy frame
tidy_fss_csv <- function(path, response_in_column, expected = NULL) {

  if (!file.exists(path)) {
    stop("FSS candidate CSV not found:\n  ", path,
         "\nRe-run 05_model-data_benthos.R / 06_model-data_fish.R, or check the ",
         "`name` and `park` values in 00_config.yml.")
  }

  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  message("Read ", basename(path), " - columns: ", paste(names(raw), collapse = ", "))

  # write.csv dumps row names into an unnamed first column
  if (names(raw)[1] %in% c("", "X")) names(raw)[1] <- ".rowname"

  # 05_model-data_benthos.R writes all.mod.fits[ , -2]. That boilerplate assumes
  # column 1 is `modname` and column 2 the long formula string - but
  # list_rbind(names_to = "response") puts `response` first, so it drops
  # `modname` instead. The formula survives, so rebuild the model names from it.
  mod_col <- pick_col(raw, c("modname", "model", "mod.name"), required = FALSE)

  if (!is.null(mod_col)) {
    modname <- as.character(raw[[mod_col]])
  } else {
    form_col <- pick_col(raw, c("formula", "form"), what = "modname or formula")
    known <- intersect(names(raw), names(term_labels))   # one column per predictor
    modname <- vapply(as.character(raw[[form_col]]), function(f) {
      hit <- known[vapply(known, function(k) grepl(k, f, fixed = TRUE), logical(1))]
      if (!length(hit)) "null" else paste(hit, collapse = "+")
    }, character(1), USE.NAMES = FALSE)
    message("  no modname column - model names rebuilt from `formula`")
  }

  resp <- if (response_in_column) {
    resp_col <- pick_col(raw, c("response", "resp", "taxa"), what = "response")
    as.character(raw[[resp_col]])
  } else {
    # do.call("rbind", out.all) on a named list builds row names as
    # "<response>.<original rowname>". Those trailing row names are NOT always
    # numeric - FSSgam often carries the model name through - so match the
    # known response names by prefix rather than stripping a numeric suffix.
    rn <- as.character(raw$.rowname)
    if (is.null(expected)) {
      sub("\\..*$", "", rn)
    } else {
      vapply(rn, function(x) {
        hit <- expected[startsWith(x, expected)]
        if (!length(hit)) NA_character_ else hit[which.max(nchar(hit))]
      }, character(1), USE.NAMES = FALSE)
    }
  }

  if (all(is.na(resp))) {
    stop("Could not identify the response for any row of\n  ", basename(path),
         "\nRow names look like: ", paste(utils::head(unique(as.character(raw$.rowname)), 3),
                                          collapse = " | "),
         "\nExpected one of: ", paste(expected, collapse = ", "))
  }

  delta_col <- pick_col(raw, c("delta.AICc", "delta.aicc", "deltaAICc", "delta_AICc"),
                        what = "delta.AICc")
  wi_col    <- pick_col(raw, c("wi.AICc", "wi.aicc", "wiAICc", "omega.AICc"),
                        what = "wi.AICc")
  aicc_col  <- pick_col(raw, c("AICc", "aicc"), required = FALSE)
  r2_col    <- pick_col(raw, c("r2.vals", "r2", "R2", "dev.expl"), required = FALSE)
  edf_col   <- pick_col(raw, c("edf", "EDF"), required = FALSE)

  tibble(
    response   = resp,
    modname    = modname,
    delta_aicc = as.numeric(raw[[delta_col]]),
    omega_aicc = as.numeric(raw[[wi_col]]),
    aicc       = if (is.null(aicc_col)) NA_real_ else as.numeric(raw[[aicc_col]]),
    r2         = if (is.null(r2_col))   NA_real_ else as.numeric(raw[[r2_col]]),
    edf        = if (is.null(edf_col))  NA_real_ else as.numeric(raw[[edf_col]])
  )
}

# Flag which candidate row is the model actually carried forward, and backfill
# R2/EDF from the fitted object where the CSV did not carry them
mark_selected <- function(candidates, final_models) {

  gam_stats <- function(model) {
    s <- summary(model)
    list(
      r2  = unname(s$dev.expl),
      edf = if (!is.null(s$s.table)) sum(s$s.table[, "edf"]) else NA_real_
    )
  }

  purrr::imap_dfr(final_models, function(mod, resp) {

    final_set <- gam_predictor_set(mod)
    stats     <- gam_stats(mod)

    rows <- candidates %>%
      dplyr::filter(response == resp) %>%
      dplyr::mutate(
        selected = purrr::map_lgl(modname,
                                  ~ identical(predictor_set_from_modname(.x), final_set))
      )

    if (nrow(rows) == 0) {
      stop("No FSS candidate rows found for '", resp, "'.\n",
           "Responses present in the CSV: ",
           paste(sort(unique(candidates$response)), collapse = ", "),
           "\nThese must match the names of the model list in ",
           "01_fit-final-models.R.")
    }
    if (!any(rows$selected)) {
      warning("The final model for '", resp, "' (", paste(final_set, collapse = "+"),
              ") does not match any FSS candidate row within delta AICc <= 2. ",
              "Check that 01_fit-final-models.R still matches 05/06, and that ",
              "the factor terms (year/status) appear in the FSS modnames.")
    }

    rows %>%
      dplyr::mutate(
        r2  = dplyr::if_else(selected & is.na(r2),  stats$r2,  r2),
        edf = dplyr::if_else(selected & is.na(edf), stats$edf, edf)
      )
  })
}

# Final shaping for gt: pretty labels, ordering, response shown once per group
format_report_table <- function(df, response_order) {
  df %>%
    dplyr::filter(response %in% response_order) %>%
    dplyr::mutate(
      response_key = factor(response, levels = response_order),
      model        = purrr::map_chr(modname, label_model)
    ) %>%
    dplyr::arrange(response_key, delta_aicc) %>%
    dplyr::mutate(
      response = unname(response_labels[as.character(response_key)]),
      response = dplyr::if_else(duplicated(response_key), "", response)
    ) %>%
    dplyr::select(response, model, delta_aicc, omega_aicc, aicc, r2, edf, selected)
}

# =============================================================================
# PUBLIC ENTRY POINTS
# =============================================================================

load_final_models <- function() {
  f_hab  <- file.path(appc_out, paste0(name, "_final-models_habitat.rds"))
  f_fish <- file.path(appc_out, paste0(name, "_final-models_fish.rds"))
  for (f in c(f_hab, f_fish)) {
    if (!file.exists(f)) stop("Missing: ", f, "\nRun 01_fit-final-models.R first.")
  }
  list(habitat = readRDS(f_hab), fish = readRDS(f_fish))
}

load_model_data <- function() {
  list(
    habitat = readRDS(file.path(appc_out, paste0(name, "_habitat-data.rds"))),
    fish    = readRDS(file.path(appc_out, paste0(name, "_fish-data.rds")))
  )
}

get_habitat_report_table <- function(models = NULL) {
  if (is.null(models)) models <- load_final_models()$habitat
  candidates <- tidy_fss_csv(
    here("output", "model-output", park, "habitat",
         paste0(name, "_abiotic_all.mod.fits.csv")),
    response_in_column = TRUE
  )
  mark_selected(candidates, models) %>%
    format_report_table(habitat_response_order) %>%
    dplyr::select(-aicc)   # Table C 1.1 does not report raw AICc
}

get_fish_report_table <- function(models = NULL) {
  if (is.null(models)) models <- load_final_models()$fish

  maxn <- tidy_fss_csv(
    here("output", "model-output", park, "fish", "maxn",
         paste0(name, "_all.mod.fits.csv")),
    response_in_column = FALSE,
    expected = names(models)
  )
  b20 <- tidy_fss_csv(
    here("output", "model-output", park, "fish", "length",
         paste0(name, "_b20_all.mod.fits.csv")),
    response_in_column = FALSE,
    expected = names(models)
  )

  mark_selected(dplyr::bind_rows(maxn, b20), models) %>%
    format_report_table(fish_response_order)
}

