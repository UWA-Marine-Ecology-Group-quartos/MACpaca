###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    FSS candidate CSVs (05) + final models (01_fit-final-models.R)
# Task:    Build the Table C 1.1 data, and shared helper functions
# Author:  Annika Leunig
# Date:    August 2026
###

library(here)
library(tidyverse)
library(mgcv)

# Locate folder
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

# Lookups
term_labels <- c(
  geoscience_detrended = "detrended",
  geoscience_roughness = "roughness",
  geoscience_aspect    = "aspect",
  geoscience_depth     = "Z",
  year                 = "year"
)

term_axis_labels <- c(
  geoscience_detrended = "Detrended",
  geoscience_roughness = "Roughness",
  geoscience_aspect    = "Aspect",
  geoscience_depth     = "Depth"
)

# Heatmap columns. year omitted - the colour is the sign of a Spearman
# correlation, undefined for a factor.
habitat_term_order <- c("geoscience_detrended", "geoscience_roughness",
                        "geoscience_aspect", "geoscience_depth")

habitat_response_order <- c("macroalgae", "sand", "seagrasses", "rock",
                            "sessile_invertebrates", "reef")

response_labels <- c(
  macroalgae            = "Macroalgae",
  sand                  = "Sand",
  rock                  = "Rock",
  seagrasses            = "Seagrass",
  sessile_invertebrates = "Sessile invertebrates",
  reef                  = "Reef"
)

# Helpers
gam_predictor_set <- function(model) {
  smooth_terms <- if (length(model$smooth) > 0) {
    vapply(model$smooth, function(s) s$term[1], character(1))
  } else character()
  parametric <- attr(terms(model), "term.labels")
  parametric <- parametric[!grepl("^s\\(", parametric)]
  sort(unique(c(smooth_terms, parametric)))
}

smooth_predictor_set <- function(model, data) {
  tms <- gam_predictor_set(model)
  tms[vapply(tms, function(t) is.numeric(data[[t]]), logical(1))]
}

predictor_set_from_modname <- function(modname) {
  tms <- str_split(as.character(modname), "\\+")[[1]] %>% str_trim()
  tms <- sub("\\.by\\..*$", "", tms)
  tms <- tms[nzchar(tms) & tms != "null"]
  sort(unique(tms))
}

label_model <- function(modname) {
  tms <- predictor_set_from_modname(modname)
  if (!length(tms)) return("null")
  tms <- tms[order(match(tms, names(term_labels)))]
  paste(dplyr::coalesce(unname(term_labels[tms]), tms), collapse = "+")
}

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

# Tidy one FSS candidate CSV
tidy_fss_csv <- function(path) {

  if (!file.exists(path)) {
    stop("FSS candidate CSV not found:\n  ", path,
         "\nRe-run 05_model-data_benthos.R, or check the `name` and `park` ",
         "values in 00_config.yml.")
  }

  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  message("Read ", basename(path), " - columns: ", paste(names(raw), collapse = ", "))

  if (names(raw)[1] %in% c("", "X")) names(raw)[1] <- ".rowname"

  mod_col <- pick_col(raw, c("modname", "model", "mod.name"), required = FALSE)

  if (!is.null(mod_col)) {
    modname <- as.character(raw[[mod_col]])
  } else {
    form_col <- pick_col(raw, c("formula", "form"), what = "modname or formula")
    known <- intersect(names(raw), names(term_labels))
    modname <- vapply(as.character(raw[[form_col]]), function(f) {
      hit <- known[vapply(known, function(k) grepl(k, f, fixed = TRUE), logical(1))]
      if (!length(hit)) "null" else paste(hit, collapse = "+")
    }, character(1), USE.NAMES = FALSE)
    message("  no modname column - model names rebuilt from `formula`")
  }

  resp_col <- pick_col(raw, c("response", "resp", "taxa"), what = "response")
  resp <- as.character(raw[[resp_col]])

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

# Flag the carried-forward model, backfill R2/EDF from the fitted object
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
              "Check that 01_fit-final-models.R still matches 05, and that ",
              "`year` appears in the FSS modnames.")
    }

    rows %>%
      dplyr::mutate(
        r2  = dplyr::if_else(selected & is.na(r2),  stats$r2,  r2),
        edf = dplyr::if_else(selected & is.na(edf), stats$edf, edf)
      )
  })
}

# Shape for gt
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

# Entry points
load_final_models <- function() {
  f_hab <- file.path(appc_out, paste0(name, "_final-models_habitat.rds"))
  if (!file.exists(f_hab)) stop("Missing: ", f_hab, "\nRun 01_fit-final-models.R first.")
  readRDS(f_hab)
}

load_model_data <- function() {
  readRDS(file.path(appc_out, paste0(name, "_habitat-data.rds")))
}

get_habitat_report_table <- function(models = NULL) {
  if (is.null(models)) models <- load_final_models()
  candidates <- tidy_fss_csv(
    here("output", "model-output", park, "habitat",
         paste0(name, "_abiotic_all.mod.fits.csv"))
  )
  mark_selected(candidates, models) %>%
    format_report_table(habitat_response_order) %>%
    dplyr::select(-aicc)
}
