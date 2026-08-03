# ==============================================================================
# 02_load-model-selection.R
# Folder: create-appendix-C
#
# Reads two sources, both purely as OUTPUTS (nothing here re-runs or edits
# 05_model-data_benthos.R / 06_model-data_fish.R):
#   1. The FSS candidate tables (all.mod.fits.csv) those scripts already
#      wrote to output/model-output/<park>/habitat|fish/ - untouched.
#   2. The final hand-picked models fit by 00_/01_ in THIS folder, saved to
#      output/model-output/<park>/appendix-C/habitat|fish/.
#
# CSV LAYOUT DIFFERENCE (confirmed from the real scripts):
#   - Habitat: list_rbind(out.all, names_to = "response") -> response is an
#     explicit CSV column.
#   - Fish: do.call("rbind", out.all) on a named list -> response is embedded
#     in the rownames as "responsename.N". Two separate files exist:
#     fish/maxn/ (abundance, richness, cti) and fish/length/ (b20).
# ==============================================================================

library(tidyverse)
library(mgcv)

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

load_model_selection <- function() {

  # ---- helper: predictor set from a fitted gam, for matching to FSS rows ---
  gam_predictor_set <- function(model) {
    smooth_terms <- if (length(model$smooth) > 0) {
      vapply(model$smooth, function(s) s$term[1], character(1))
    } else character()
    parametric_terms <- attr(terms(model), "term.labels")
    parametric_terms <- parametric_terms[!grepl("^s\\(", parametric_terms)]
    sort(unique(c(smooth_terms, parametric_terms)))
  }

  gam_summary_stats <- function(model) {
    s <- summary(model)
    tibble(
      r2  = unname(s$dev.expl),
      edf = if (!is.null(s$s.table)) sum(s$s.table[, "edf"]) else NA_real_
    )
  }

  predictor_set_from_modname <- function(modname_str) {
    terms <- str_split(modname_str, "\\+")[[1]] %>% str_trim()
    terms[terms == "Z"] <- "geoscience_depth" # TODO confirm this alias holds for your FSS setup
    sort(unique(terms))
  }

  match_final_to_candidates <- function(final_models, candidates_by_response) {
    purrr::imap_dfr(final_models, function(mod, resp) {

      final_set <- gam_predictor_set(mod)

      candidates <- candidates_by_response %>%
        dplyr::filter(response == resp) %>%
        dplyr::mutate(pred_set = map(modname, predictor_set_from_modname))

      match_row <- candidates %>%
        dplyr::filter(map_lgl(pred_set, ~ identical(.x, final_set)))

      if (nrow(match_row) == 0) {
        warning("No FSS candidate row matched the final model predictor set for '", resp,
                "' - check gam_predictor_set()/predictor_set_from_modname() alignment.")
        match_row <- candidates[1, ]
      }

      stats <- gam_summary_stats(mod)

      tibble(
        response   = resp,
        model      = match_row$modname[1],
        delta_aicc = match_row$delta.AICc[1],
        omega_aicc = match_row$wi.AICc[1],
        aicc       = if ("AICc" %in% names(match_row)) match_row$AICc[1] else NA_real_,
        r2         = stats$r2,
        edf        = stats$edf
      )
    })
  }

  # ==========================================================================
  # HABITAT - candidate CSV from the original 05_ output, final models from
  # THIS folder's 00_fit-final-models_habitat.R output
  # ==========================================================================
  habitat_fss_dir <- paste0("output/model-output/", park, "/habitat/")
  habitat_appC_dir <- paste0("output/model-output/", park, "/appendix-C/habitat/")

  habitat_fits <- read_csv(paste0(habitat_fss_dir, name, "_abiotic_all.mod.fits.csv")) %>%
    dplyr::select(-1)

  final_models_habitat <- readRDS(paste0(habitat_appC_dir, name, "_final-models.rds"))

  habitat_report <- match_final_to_candidates(final_models_habitat, habitat_fits) %>%
    dplyr::filter(response != "reef") %>% # Table C1.1 shows the five primary categories only
    dplyr::mutate(
      response = case_match(response,
                            "macroalgae"            ~ "Macroalgae",
                            "seagrasses"            ~ "Seagrass",
                            "sand"                  ~ "Sand",
                            "rock"                  ~ "Rock",
                            "sessile_invertebrates" ~ "Sessile invertebrates"
      )
    )

  # ==========================================================================
  # FISH - candidate CSVs from the original 06_ output, final models from
  # THIS folder's 01_fit-final-models_fish.R output
  # ==========================================================================
  maxn_dir   <- paste0("output/model-output/", park, "/fish/maxn/")
  length_dir <- paste0("output/model-output/", park, "/fish/length/")
  fish_appC_dir <- paste0("output/model-output/", park, "/appendix-C/fish/")
  name_b20   <- paste(name, "b20", sep = "_")

  read_fish_fits <- function(path) {
    read_csv(path) %>%
      dplyr::rename(row_id = 1) %>%
      dplyr::mutate(response = str_remove(row_id, "\\.{1,3}\\d+$")) %>%
      dplyr::select(-row_id)
  }

  fish_fits <- bind_rows(
    read_fish_fits(paste0(maxn_dir,   name,     "_all.mod.fits.csv")),
    read_fish_fits(paste0(length_dir, name_b20, "_all.mod.fits.csv"))
  )

  final_models_fish <- readRDS(paste0(fish_appC_dir, name, "_final-models.rds"))

  fish_report <- match_final_to_candidates(final_models_fish, fish_fits) %>%
    dplyr::mutate(
      response = case_match(response,
                            "total_abundance"  ~ "Total abundance",
                            "species_richness" ~ "Species richness",
                            "cti"              ~ "Reef Fish Thermal Index",
                            "b20"              ~ "Large Reef Fish Index*"
      )
    )

  list(habitat = habitat_report, fish = fish_report)
}

# Usage in the .qmd:
# source("02_load-model-selection.R")
# model_selection <- load_model_selection()
