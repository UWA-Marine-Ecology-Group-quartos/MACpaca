###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Folder:  create-appendix-C
# Task:    Build Appendix C figures - variable importance heatmaps (C1.1/C2.1)
#          and GAM response-curve panels (C1.2/C2.2). Reads the final models
#          + data saved by 00_/01_ in this folder, and the FSS var.imp CSVs
#          already written by 05_/06_ (untouched).
###

rm(list = ls())

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

library(tidyverse)
library(mgcv)
library(patchwork)

habitat_fss_dir  <- paste0("output/model-output/", park, "/habitat/")
habitat_appC_dir <- paste0("output/model-output/", park, "/appendix-C/habitat/")
fish_fss_dir     <- paste0("output/model-output/", park, "/fish/")
fish_appC_dir    <- paste0("output/model-output/", park, "/appendix-C/fish/")

# ==============================================================================
# SHARED HELPERS
# ==============================================================================

gam_predictor_set <- function(model) {
  smooth_terms <- if (length(model$smooth) > 0) {
    vapply(model$smooth, function(s) s$term[1], character(1))
  } else character()
  parametric_terms <- attr(terms(model), "term.labels")
  parametric_terms <- parametric_terms[!grepl("^s\\(", parametric_terms)]
  sort(unique(c(smooth_terms, parametric_terms)))
}

# For response-curve panels specifically: gam_predictor_set() above correctly
# includes factor terms like "year"/"status" (needed for FSS-candidate
# matching), but those can't be used as a curve's x-axis - min()/max() on a
# factor errors. This filters down to just the numeric smooth terms.
smooth_predictor_set <- function(model, data) {
  terms <- gam_predictor_set(model)
  terms[vapply(terms, function(t) is.numeric(data[[t]]), logical(1))]
}

# TODO see prior discussion - this is a pragmatic proxy for "positive/negative
# relationship" (FSSgam importance weights aren't signed), not a report from
# FSSgam itself. Worth spot-checking against the response curves.
get_effect_sign <- function(model, term, data) {
  term_mat <- predict(model, type = "terms")
  col_match <- grep(paste0("(^|\\()", term, "(,|\\))"), colnames(term_mat), value = TRUE)[1]
  if (is.na(col_match)) return(0)
  eff <- term_mat[, col_match]
  s <- suppressWarnings(cor(eff, data[[term]], use = "complete.obs"))
  if (is.na(s)) 0 else sign(s)
}

build_importance_df <- function(var_imp, final_models, data_list, predictors) {
  purrr::imap_dfr(final_models, function(mod, resp) {
    imp_row <- var_imp[resp, predictors, drop = FALSE]
    in_set  <- gam_predictor_set(mod)
    tibble(
      response   = resp,
      predictor  = predictors,
      importance = as.numeric(imp_row[1, ]),
      in_model   = predictors %in% in_set,
      sign       = purrr::map_dbl(predictors, ~ get_effect_sign(mod, .x, data_list[[resp]]))
    )
  }) %>%
    mutate(signed_importance = importance * sign)
}

predictor_labels <- c(
  geoscience_detrended = "Detrended",
  geoscience_roughness = "Roughness",
  geoscience_slope     = "Slope",
  geoscience_aspect    = "Aspect",
  geoscience_depth     = "Depth",
  reef                 = "Reef"
)

plot_importance_heatmap <- function(df, response_levels) {
  df %>%
    mutate(
      response  = factor(response, levels = rev(response_levels)),
      predictor = factor(unname(predictor_labels[predictor]),
                         levels = unname(predictor_labels[unique(predictor)]))
    ) %>%
    ggplot(aes(x = predictor, y = response, fill = signed_importance)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(in_model, "X", "")), size = 4) +
    scale_fill_gradient2(
      low = "#2c5aa0", mid = "white", high = "#c0392b",
      midpoint = 0, limits = c(-1, 1), name = "Importance",
      guide = guide_colourbar(barwidth = unit(4, "cm"), barheight = unit(0.35, "cm"),
                              direction = "horizontal", title.position = "top")
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "top",
          axis.text.y = element_text(face = "italic"))
}

gam_partial_curve <- function(model, term, data, n = 100) {
  model_vars <- gam_predictor_set(model)
  other_vars <- setdiff(model_vars, term)

  x_seq <- seq(min(data[[term]], na.rm = TRUE), max(data[[term]], na.rm = TRUE), length.out = n)
  newdata <- setNames(data.frame(x_seq), term)

  for (v in other_vars) {
    if (is.numeric(data[[v]])) {
      newdata[[v]] <- mean(data[[v]], na.rm = TRUE)
    } else {
      # TODO confirm "first level" is the right reference year/status
      newdata[[v]] <- factor(levels(factor(data[[v]]))[1], levels = levels(factor(data[[v]])))
    }
  }

  pred <- predict(model, newdata = newdata, type = "response", se.fit = TRUE)

  newdata %>%
    transmute(
      x         = .data[[term]],
      fit       = pred$fit,
      se        = pred$se.fit,
      lower     = fit - se,
      upper     = fit + se,
      predictor = term
    )
}

plot_response_curves <- function(final_models, data_list, panels, y_lab = "Predicted probability") {
  plots <- list()
  letters_seq <- letters
  li <- 1

  for (resp in names(panels)) {
    mod <- final_models[[resp]]
    dat <- data_list[[resp]]
    resp_var <- all.vars(formula(mod))[1]
    is_first_term_for_response <- TRUE

    for (term in panels[[resp]]) {
      curve_df <- gam_partial_curve(mod, term, dat)

      # Raw data points - TODO for habitat this should be number/total_pts
      # (a proportion), for fish it's the raw count/index. Adjust below if
      # you want the points/curve compared differently.
      #
      # IMPORTANT: point_df is built here, with fixed column names, and
      # passed as `data=` to geom_point(). This is NOT cosmetic - aes()
      # mappings are lazily evaluated at print()/ggsave() time, by which
      # point this for-loop has finished and `term`/`y_raw` would resolve to
      # their LAST loop values for every panel if referenced directly inside
      # aes(). Baking them into columns of a fixed data frame first sidesteps
      # that entirely, since column lookups resolve against the data stored
      # in the layer, not the (by-then-stale) enclosing environment.
      point_df <- data.frame(px = dat[[term]], py = dat[[resp_var]])

      panel_title <- paste0(letters_seq[li], if (is_first_term_for_response) paste0("  ", resp) else "")
      is_first_term_for_response <- FALSE
      x_label <- unname(predictor_labels[term]) %||% term
      y_label <- if (li == 1) y_lab else NULL

      p <- ggplot(curve_df, aes(x = x, y = fit)) +
        geom_point(data = point_df, aes(x = px, y = py), alpha = 0.25, size = 1, inherit.aes = FALSE) +
        geom_line() +
        geom_line(aes(y = lower), linetype = "dashed") +
        geom_line(aes(y = upper), linetype = "dashed") +
        labs(x = x_label, y = y_label, title = panel_title) +
        theme_minimal(base_size = 10) +
        theme(plot.title = element_text(hjust = 0, size = 10, face = "bold"))

      plots[[paste(resp, term, sep = "_")]] <- p
      li <- li + 1
    }
  }

  wrap_plots(plots, ncol = 3)
}

# ==============================================================================
# HABITAT - Figure C1.1 + C1.2
# ==============================================================================

habitat_var_imp <- read.csv(paste0(habitat_fss_dir, name, "_abiotic_all.var.imp.csv"), row.names = 1)
final_models_habitat <- readRDS(paste0(habitat_appC_dir, name, "_final-models.rds"))
habi <- readRDS(paste0(habitat_appC_dir, name, "_habitat-data.rds"))

habitat_data_list <- setNames(rep(list(habi), length(final_models_habitat)), names(final_models_habitat))

habitat_predictors <- c("geoscience_detrended", "geoscience_roughness", "geoscience_aspect", "geoscience_depth")
# Confirmed against a real run: 05_model-data_benthos.R's pred.vars is
# c("geoscience_depth","geoscience_aspect","geoscience_roughness","geoscience_detrended")
# - no "slope". Your source PDF's "detrended+slope+Z" example must be from a
# different/earlier data run than what's live now.

habitat_imp_df <- build_importance_df(
  habitat_var_imp,
  final_models_habitat[setdiff(names(final_models_habitat), "reef")],
  habitat_data_list,
  habitat_predictors
)

fig_c1_1 <- plot_importance_heatmap(
  habitat_imp_df,
  response_levels = c("macroalgae", "seagrasses", "sand", "rock", "sessile_invertebrates")
)

ggsave(paste0(habitat_appC_dir, name, "_habitat-importance.png"), fig_c1_1, width = 6, height = 4, dpi = 300)

habitat_panels <- purrr::imap(
  final_models_habitat[setdiff(names(final_models_habitat), "reef")],
  function(mod, resp) smooth_predictor_set(mod, habitat_data_list[[resp]])
)

fig_c1_2 <- plot_response_curves(final_models_habitat, habitat_data_list, habitat_panels, y_lab = "Predicted probability")

ggsave(paste0(habitat_appC_dir, name, "_habitat-response-curves.png"), fig_c1_2, width = 9, height = 12, dpi = 300)

# ==============================================================================
# FISH - Figure C2.1 + C2.2
# ==============================================================================

read_var_imp <- function(path) {
  df <- read.csv(path, check.names = FALSE)
  names(df)[1] <- "response"

  bad_rows <- is.na(df$response) | df$response == ""
  if (any(bad_rows)) {
    warning("Dropping ", sum(bad_rows), " row(s) with blank/NA response in ", path,
            " - check whether the FSS run for that response actually failed.")
    df <- df[!bad_rows, ]
  }

  df %>% tibble::column_to_rownames("response")
}

fish_var_imp <- bind_rows(
  read_var_imp(paste0(fish_fss_dir, "maxn/",   name,                "_all.var.imp.csv")),
  read_var_imp(paste0(fish_fss_dir, "length/", paste0(name, "_b20"), "_all.var.imp.csv"))
)

final_models_fish <- readRDS(paste0(fish_appC_dir, name, "_final-models.rds"))
fabund <- readRDS(paste0(fish_appC_dir, name, "_fish-data.rds"))

fish_data_list <- list(
  total_abundance  = fabund %>% dplyr::filter(response %in% "total_abundance"),
  species_richness = fabund %>% dplyr::filter(response %in% "species_richness"),
  cti              = fabund %>% dplyr::filter(response %in% "cti"),
  b20              = fabund %>% dplyr::filter(response %in% "b20")
)

fish_predictors <- c("reef", "geoscience_detrended", "geoscience_roughness", "geoscience_aspect", "geoscience_depth")
# Matches 06_model-data_fish.R's pred.vars exactly:
# c("reef","geoscience_depth","geoscience_aspect","geoscience_roughness","geoscience_detrended")

fish_imp_df <- build_importance_df(fish_var_imp, final_models_fish, fish_data_list, fish_predictors)

fig_c2_1 <- plot_importance_heatmap(
  fish_imp_df,
  response_levels = c("species_richness", "total_abundance", "b20", "cti")
)

ggsave(paste0(fish_appC_dir, name, "_fish-importance.png"), fig_c2_1, width = 6, height = 4, dpi = 300)

fish_panels <- purrr::imap(
  final_models_fish,
  function(mod, resp) smooth_predictor_set(mod, fish_data_list[[resp]])
)

fig_c2_2 <- plot_response_curves(final_models_fish, fish_data_list, fish_panels, y_lab = "Predicted value")

ggsave(paste0(fish_appC_dir, name, "_fish-response-curves.png"), fig_c2_2, width = 9, height = 9, dpi = 300)
