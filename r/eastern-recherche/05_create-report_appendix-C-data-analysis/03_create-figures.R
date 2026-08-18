###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    FSS variable importance CSVs (05/06) + final models (01_)
# Task:    Build Appendix C figures - importance heatmaps (C 1.1, C 2.1) and
#          GAM response-curve panels (C 1.2, C 2.2)
# Author:  Annika Leunig
# Date:    August 2026
###

rm(list = ls())

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

source(file.path(appc_dir, "02_load-model-selection.R"))

library(patchwork)

figdir <- file.path(appc_out, "figures")
dir.create(figdir, recursive = TRUE, showWarnings = FALSE)

models <- load_final_models()
dat    <- load_model_data()

# =============================================================================
# PART 1 - VARIABLE IMPORTANCE HEATMAPS
# =============================================================================

# all.var.imp is rbind()ed over responses, so write.csv puts the response name
# in the first (unnamed) column and one column per predictor.
read_var_imp <- function(path) {
  if (!file.exists(path)) stop("Variable importance CSV not found:\n  ", path)
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x)[1] <- "response"
  x %>%
    tidyr::pivot_longer(-response, names_to = "term", values_to = "importance") %>%
    dplyr::mutate(response   = as.character(response),
                  importance = as.numeric(importance))
}

# FSSgam importance weights are unsigned. The red/blue colouring in the report
# figure is direction of effect, taken here as the sign of the Spearman
# correlation between the observed response and the predictor. This is a
# pragmatic proxy, not an FSSgam output - worth eyeballing against the response
# curves in C 1.2 / C 2.2 before the report goes out.
term_sign <- function(y, x) {
  if (all(is.na(x)) || all(is.na(y))) return(1)
  r <- suppressWarnings(stats::cor(y, x, method = "spearman", use = "complete.obs"))
  if (is.na(r) || r == 0) 1 else sign(r)
}

habitat_signs <- purrr::map_dfr(names(models$habitat), function(resp) {
  y <- dat$habitat[[resp]] / dat$habitat$total_pts
  purrr::map_dfr(habitat_term_order, function(tm) {
    tibble(response = resp, term = tm, sgn = term_sign(y, dat$habitat[[tm]]))
  })
})

fish_signs <- purrr::map_dfr(names(models$fish), function(resp_i) {
  d <- dat$fish %>% dplyr::filter(.data$response == resp_i)
  purrr::map_dfr(fish_term_order, function(tm) {
    tibble(response = resp_i, term = tm, sgn = term_sign(d$count, d[[tm]]))
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
  models   = models$habitat,
  response_order = habitat_response_order,
  term_order     = habitat_term_order,
  italic_y = TRUE
)

fig_c2_1 <- build_importance_plot(
  var_imp = dplyr::bind_rows(
    read_var_imp(here("output", "model-output", park, "fish", "maxn",
                      paste0(name, "_all.var.imp.csv"))),
    read_var_imp(here("output", "model-output", park, "fish", "length",
                      paste0(name, "_b20_all.var.imp.csv")))
  ),
  signs    = fish_signs,
  models   = models$fish,
  response_order = fish_response_order,
  term_order     = fish_term_order,
  italic_y = FALSE
)

ggsave(file.path(figdir, paste0(name, "_habitat-importance.png")), fig_c1_1,
       width = 7, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(figdir, paste0(name, "_fish-importance.png")), fig_c2_1,
       width = 7, height = 5, dpi = 300, bg = "white")

# =============================================================================
# PART 2 - RESPONSE-CURVE PANELS
# =============================================================================

# Predict across one predictor, holding the others at their mean (numeric) or
# first level (factor). Everything is materialised into plain data frames inside
# the function so nothing is captured lazily by the ggplot below.
curve_data <- function(model, data, term, obs_y, n = 200) {

  force(term)

  xs <- data[[term]]
  newdat <- data.frame(seq(min(xs, na.rm = TRUE), max(xs, na.rm = TRUE),
                           length.out = n))
  names(newdat) <- term

  for (v in setdiff(gam_predictor_set(model), term)) {
    if (is.numeric(data[[v]])) {
      newdat[[v]] <- mean(data[[v]], na.rm = TRUE)
    } else {
      lv <- levels(droplevels(factor(data[[v]])))
      newdat[[v]] <- factor(lv[1], levels = lv)
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

# Assemble one row of panels per response, padded with spacers so responses
# always start a new row (matching the report layout).
build_curve_grid <- function(models, data_for, obs_for, response_order,
                             term_order, ncol) {

  panels <- list()
  tag_i  <- 0

  for (resp in response_order) {

    mod <- models[[resp]]
    if (is.null(mod)) next

    d  <- data_for(resp)
    ys <- obs_for(resp)

    tms <- smooth_predictor_set(mod, d)
    tms <- tms[order(match(tms, term_order))]

    ylim <- range(ys, na.rm = TRUE)

    for (j in seq_along(tms)) {
      tag_i <- tag_i + 1
      cd <- curve_data(mod, d, tms[j], ys)
      panels[[length(panels) + 1]] <- curve_panel(
        cd,
        x_lab = unname(term_axis_labels[tms[j]]),
        title = if (j == 1) unname(response_labels[resp]) else NULL,
        tag   = letters[tag_i],
        ylim  = ylim
      )
    }

    # pad the row out so the next response starts on a fresh line
    if (length(tms) < ncol) {
      for (k in seq_len(ncol - length(tms))) {
        panels[[length(panels) + 1]] <- patchwork::plot_spacer()
      }
    }
  }

  patchwork::wrap_plots(panels, ncol = ncol)
}

# ---- Habitat: observed proportion of points, not raw counts ------------------
fig_c1_2 <- build_curve_grid(
  models        = models$habitat,
  data_for      = function(resp) dat$habitat,
  obs_for       = function(resp) dat$habitat[[resp]] / dat$habitat$total_pts,
  response_order = habitat_response_order,
  term_order     = habitat_term_order,
  ncol           = 3
)

# ---- Fish: modelled count metric --------------------------------------------
fish_subset <- function(resp) dat$fish %>% dplyr::filter(.data$response == resp)

fig_c2_2 <- build_curve_grid(
  models        = models$fish,
  data_for      = fish_subset,
  obs_for       = function(resp) fish_subset(resp)$count,
  response_order = fish_response_order,
  term_order     = fish_term_order,
  ncol           = 2
)

ggsave(file.path(figdir, paste0(name, "_habitat-response-curves.png")), fig_c1_2,
       width = 8, height = 2.1 * length(habitat_response_order), dpi = 300, bg = "white")
ggsave(file.path(figdir, paste0(name, "_fish-response-curves.png")), fig_c2_2,
       width = 6.5, height = 2.1 * length(fish_response_order), dpi = 300, bg = "white")

message("Appendix C figures written to: ", figdir)
