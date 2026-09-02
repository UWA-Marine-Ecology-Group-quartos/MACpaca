###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    FSS variable importance CSVs (05) + final models (01_)
# Task:    Build Appendix C figures - importance heatmap and per-year GAM
#          response-curve panels
# Author:  Annika Leunig
# Date:    August 2026
###

rm(list = ls())

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

source(file.path(appc_dir, "02_load-model-selection.R"))

library(patchwork)

years           <- unlist(config$years)
year_levels     <- as.character(sort(years))
combine_benthos <- config$combine_benthos

figdir <- file.path(appc_out, "figures")
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)

models <- load_final_models()
dat    <- load_model_data()

# Model structure
model_has_term <- function(mod, tm) tm %in% gam_predictor_set(mod)

model_has_by_year <- function(mod) {
  if (!length(mod$smooth)) return(FALSE)
  any(vapply(mod$smooth, function(s) identical(s$by, "year"), logical(1)))
}

message("Final model structure:")
for (resp in names(models)) {
  message("  ", resp,
          " - year term: ", if (model_has_term(models[[resp]], "year")) "yes" else "no",
          "; year-varying smooths: ", if (model_has_by_year(models[[resp]])) "yes" else "no")
}

# Guard
if (!combine_benthos &&
    !all(vapply(models, model_has_term, logical(1), tm = "year"))) {
  stop("combine_benthos is FALSE but a habitat model has no `year` term. ",
       "Re-run 01_fit-final-models.R, or pool the benthos and collapse the ",
       "per-year figure loop below.")
}

# Importance heatmap
read_var_imp <- function(path) {
  if (!file.exists(path)) stop("Variable importance CSV not found:\n  ", path)
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x)[1] <- "response"
  x %>%
    tidyr::pivot_longer(-response, names_to = "term", values_to = "importance") %>%
    dplyr::mutate(response   = as.character(response),
                  importance = as.numeric(importance))
}

# FSSgam weights are unsigned
term_sign <- function(y, x) {
  if (all(is.na(x)) || all(is.na(y))) return(1)
  r <- suppressWarnings(stats::cor(y, x, method = "spearman", use = "complete.obs"))
  if (is.na(r) || r == 0) 1 else sign(r)
}

habitat_signs <- purrr::map_dfr(names(models), function(resp) {
  y <- dat[[resp]] / dat$total_pts
  purrr::map_dfr(habitat_term_order, function(tm) {
    tibble(response = resp, term = tm, sgn = term_sign(y, dat[[tm]]))
  })
})

build_importance_plot <- function(var_imp, signs, models, response_order, term_order,
                                  italic_y = FALSE) {

  selected_terms <- purrr::map(models, gam_predictor_set)

  df <- var_imp %>%
    dplyr::filter(response %in% response_order, term %in% term_order) %>%
    dplyr::left_join(signs, by = c("response", "term")) %>%
    dplyr::mutate(
      sgn      = dplyr::coalesce(sgn, 1),
      signed   = pmin(pmax(importance * sgn, -1), 1),
      in_top   = purrr::map2_lgl(response, term, ~ .y %in% selected_terms[[.x]]),
      response = factor(response, levels = rev(response_order)),
      term     = factor(term,     levels = term_order)
    )

  ggplot(df, aes(x = term, y = response, fill = signed)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(data = dplyr::filter(df, in_top), aes(label = "X"),
              size = 4.5, colour = "black") +
    scale_fill_gradient2(low = "#2500cb", mid = "white", high = "#e42315",
                         midpoint = 0, limits = c(-1, 1),
                         breaks = seq(-1, 1, 0.5), name = "Importance") +
    scale_x_discrete(labels = function(z) unname(term_axis_labels[z])) +
    scale_y_discrete(labels = function(z) unname(response_labels[z])) +
    theme_classic() +
    theme(
      axis.title      = element_blank(),
      axis.line       = element_blank(),
      axis.ticks      = element_blank(),
      axis.text.x     = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
      axis.text.y     = element_text(size = 10,
                                     face = if (italic_y) "italic" else "plain"),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title    = element_text(face = "bold", size = 9),
      legend.text     = element_text(size = 8),
      legend.key.width  = unit(2.6, "cm"),
      legend.key.height = unit(0.35, "cm"),
      plot.margin     = margin(5, 10, 5, 5)
    ) +
    guides(fill = guide_colourbar(title.position = "left", title.vjust = 0.9,
                                  ticks = FALSE))
}

fig_c1_1 <- build_importance_plot(
  var_imp  = read_var_imp(here("output", "model-output", park, "habitat",
                               paste0(name, "_abiotic_all.var.imp.csv"))),
  signs    = habitat_signs,
  models   = models,
  response_order = habitat_response_order,
  term_order     = habitat_term_order,
  italic_y = TRUE
)

ggsave(file.path(figdir, paste0(name, "_habitat-importance.png")), fig_c1_1,
       width = 7, height = 6, dpi = 300, bg = "white")

# Response curves
curve_data <- function(model, data, term, obs_y, n = 200, ref = NULL) {

  force(term)

  xs <- data[[term]]
  newdat <- data.frame(seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE),
                           length.out = n))
  names(newdat) <- term

  for (v in setdiff(gam_predictor_set(model), term)) {
    if (is.numeric(data[[v]])) {
      newdat[[v]] <- mean(data[[v]], na.rm = TRUE)
    } else {
      lv <- model$xlevels[[v]]
      if (is.null(lv)) lv <- levels(droplevels(factor(data[[v]])))
      pick <- if (!is.null(ref) && !is.na(ref[v]) && ref[v] %in% lv) unname(ref[v]) else lv[1]
      newdat[[v]] <- factor(pick, levels = lv)
    }
  }

  pr <- mgcv::predict.gam(model, newdata = newdat, type = "response", se.fit = TRUE)

  list(
    pred   = data.frame(x  = as.numeric(newdat[[term]]),
                        fit = as.numeric(pr$fit),
                        se  = as.numeric(pr$se.fit)),
    points = data.frame(x = as.numeric(xs), y = as.numeric(obs_y))
  )
}

curve_panel <- function(cd, x_lab, title = NULL, tag = NULL, ylim = NULL) {

  p <- ggplot() +
    geom_point(data = cd$points, aes(x = x, y = y),
               colour = "grey40", alpha = 0.3, size = 0.9) +
    geom_line(data = cd$pred, aes(x = x, y = fit), linewidth = 0.5) +
    geom_line(data = cd$pred, aes(x = x, y = fit + se),
              linetype = "dashed", linewidth = 0.4) +
    geom_line(data = cd$pred, aes(x = x, y = fit - se),
              linetype = "dashed", linewidth = 0.4) +
    labs(x = x_lab, y = NULL, title = title, tag = tag) +
    theme_classic() +
    theme(
      plot.title      = element_text(size = 9, hjust = 0, margin = margin(b = 2)),
      plot.tag        = element_text(size = 10, face = "bold"),
      plot.tag.position = c(0, 1),
      axis.title.x    = element_text(size = 8),
      axis.text       = element_text(size = 7),
      plot.margin     = margin(12, 6, 4, 4)
    )

  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p
}

build_curve_grid <- function(models, data_for, obs_for, response_order,
                             term_order, ncol, pad_rows = TRUE,
                             ref = NULL, ylim_for = NULL) {

  panels <- list()
  tag_i  <- 0

  for (resp in response_order) {

    mod <- models[[resp]]
    if (is.null(mod)) next

    d  <- data_for(resp)
    ys <- obs_for(resp)

    tms <- smooth_predictor_set(mod, d)
    tms <- tms[order(match(tms, term_order))]

    ylim <- if (is.null(ylim_for)) range(ys, na.rm = TRUE) else ylim_for(resp)

    for (j in seq_along(tms)) {
      tag_i <- tag_i + 1
      cd <- curve_data(mod, d, tms[j], ys, ref = ref)
      panels[[length(panels) + 1]] <- curve_panel(
        cd,
        x_lab = unname(term_axis_labels[tms[j]]),
        title = if (j == 1) unname(response_labels[resp]) else NULL,
        tag   = letters[tag_i],
        ylim  = ylim
      )
    }

    if (pad_rows && length(tms) < ncol) {
      for (k in seq_len(ncol - length(tms))) {
        panels[[length(panels) + 1]] <- patchwork::plot_spacer()
      }
    }
  }

  out <- patchwork::wrap_plots(panels, ncol = ncol)
  attr(out, "n_rows") <- ceiling(length(panels) / ncol)
  out
}

grid_ncol <- function(model_list, data_for) {
  max(vapply(names(model_list),
             function(r) length(smooth_predictor_set(model_list[[r]], data_for(r))),
             integer(1)))
}

row_height_in <- 1.5

# One figure per year
hab_ncol <- grid_ncol(models, function(resp) dat)

# Fixed across years so the panels stay comparable
hab_ylim <- function(resp) range(dat[[resp]] / dat$total_pts, na.rm = TRUE)

curve_years <- levels(dat$year)

for (yr in curve_years) {

  message("Building habitat response curves for: ", yr)

  dat_yr <- dat %>% dplyr::filter(as.character(year) == yr)

  fig_yr <- build_curve_grid(
    models         = models,
    data_for       = function(resp) dat_yr,
    obs_for        = function(resp) dat_yr[[resp]] / dat_yr$total_pts,
    response_order = habitat_response_order,
    term_order     = habitat_term_order,
    ncol           = hab_ncol,
    pad_rows       = TRUE,
    ref            = c(year = yr),
    ylim_for       = hab_ylim
  )

  ggsave(file.path(figdir, paste0(name, "_habitat-response-curves_", yr, ".png")),
         fig_yr,
         width = 2 * hab_ncol, height = row_height_in * attr(fig_yr, "n_rows"),
         dpi = 300, bg = "white")
}

message("Appendix C figures written to: ", figdir)
message("Habitat response curves drawn per year: ",
        paste(curve_years, collapse = ", "))

