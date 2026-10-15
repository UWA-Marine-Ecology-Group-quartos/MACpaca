##
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    Table builders for the fish GAMM significance tables (Table C.3,
#          Table C.4) - year/status p-values and the status test by year
# Author:  Annika Leunig
# Date:    August 2026
###
# Uses response_labels, gam_predictor_set() etc from 02_load-model-selection.R
# and the same gt() styling helpers (report_header_html, report_row_padding,
# report_font_size) from Gt_model_tables.R - source both before this file.

library(gt)
library(dplyr)
library(kableExtra)
library(knitr)
library(purrr)
library(tibble)

# ---- pull the year/status rows out of a GAM's parametric coefficient table --
# summary.gam()$p.table has one row per dummy-coded factor level (not one row
# per whole factor) - e.g. "year2024", "statusNo-Take". Column names for the
# test statistic/p-value differ by family ("t value"/"z value"), so they are
# picked up by position rather than by name.
gam_term_pvalues <- function(model) {
  pt <- as.data.frame(summary(model)$p.table)
  pt$term <- rownames(pt)

  pt <- pt[pt$term != "(Intercept)", , drop = FALSE]
  pt <- pt[grepl("^(year|status)", pt$term), , drop = FALSE]

  tibble(
    term      = pt$term,
    estimate  = pt[[1]],
    se        = pt[[2]],
    statistic = pt[[3]],
    p_value   = pt[[4]]
  )
}

# Turn "year2024" / "statusNo-Take" into a readable contrast, using the
# reference (first) level actually fitted in that model.
describe_term <- function(term, model) {
  if (startsWith(term, "status")) {
    ref <- model$xlevels$status[1]
    lvl <- sub("^status", "", term)
    return(paste0("Zone status: ", lvl, " vs ", ref))
  }

  if (startsWith(term, "year")) {
    ref <- model$xlevels$year[1]
    yr  <- sub("^year", "", term)
    return(paste0("Year: ", yr, " vs ", ref))
  }

  term
}

# =============================================================================
# TABLE C.3 - year and zone status significance, per fish GAMM
# =============================================================================
# One row per factor-level contrast (per-level p-values), for every fish
# model that carries a year or status term.

build_fish_term_pvalue_table <- function(models, response_order) {

  result <- purrr::imap_dfr(models, function(mod, resp) {

    terms <- gam_predictor_set(mod)

    if (!any(c("year", "status") %in% terms)) {
      return(NULL)
    }

    pvalues <- gam_term_pvalues(mod)

    if (!nrow(pvalues)) {
      return(NULL)
    }

    pvalues %>%
      mutate(
        response = resp,
        contrast = vapply(
          term,
          describe_term,
          character(1),
          model = mod
        )
      ) %>%
      select(response, contrast, estimate, se, statistic, p_value)
  })

  if (!nrow(result)) {
    return(
      tibble(
        response  = character(),
        contrast  = character(),
        estimate  = numeric(),
        se        = numeric(),
        statistic = numeric(),
        p_value   = numeric()
      )
    )
  }

  result %>%
    mutate(response_key = factor(response, levels = response_order)) %>%
    arrange(response_key, contrast) %>%
    mutate(
      response = unname(response_labels[as.character(response_key)]),
      response = if_else(duplicated(response_key), "", response)
    ) %>%
    select(-response_key)
}

format_pvalue_table <- function(df) {
  df %>%
    mutate(
      estimate  = num_fmt(estimate,  3),
      se        = num_fmt(se,        3),
      statistic = num_fmt(statistic, 3),
      p_value   = dplyr::if_else(
        as.numeric(p_value) < 0.001,
        "<0.001",
        num_fmt(p_value, 3)
      )
    )
}

build_fish_term_pvalue_gt <- function(df) {

  if (!nrow(df)) {
    return(
      gt(tibble(Note = "No year or status significance terms were available."))
    )
  }

  df %>%
    format_pvalue_table() %>%
    gt() %>%
    cols_label(
      response  = "Response",
      contrast  = "Term",
      estimate  = "Estimate",
      se        = "SE",
      statistic = "Statistic",
      p_value   = "p-value"
    ) %>%
    cols_align(
      align = "left",
      columns = c(response, contrast)
    ) %>%
    cols_align(
      align = "center",
      columns = c(estimate, se, statistic, p_value)
    ) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(report_font_size),
      data_row.padding = report_row_padding,
      table_body.hlines.style = "none",
      column_labels.background.color = report_header_html
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
}

# PDF - kableExtra. gt's own LaTeX backend corrupts this table (see the "WHY
# TWO BACKENDS" note at the top of Gt_model_tables.R) - same fix, same pattern.
build_fish_term_pvalue_kable <- function(df) {

  if (!nrow(df)) {
    return(
      kableExtra::kbl(
        data.frame(Note = "No year or status significance terms were available."),
        booktabs = TRUE,
        escape = TRUE
      ) %>%
        kable_styling(
          latex_options = "hold_position",
          font_size = report_font_size,
          full_width = FALSE
        )
    )
  }

  body <- df %>% format_pvalue_table()
  n_col <- ncol(body)

  body %>%
    kbl(
      booktabs  = TRUE,
      longtable = TRUE,
      escape    = FALSE,
      linesep   = "",
      col.names = c(
        "Response", "Term", "Estimate", "SE", "Statistic", "p-value"
      ),
      align = c("l", "l", rep("c", n_col - 2))
    ) %>%
    kable_styling(
      latex_options = c("repeat_header", "hold_position"),
      font_size = report_font_size,
      full_width = FALSE
    ) %>%
    row_spec(0, bold = TRUE, background = report_header_latex)
}

build_fish_term_pvalue_report <- function(df) {

  if (knitr::is_latex_output()) {
    build_fish_term_pvalue_kable(df)
  } else {
    build_fish_term_pvalue_gt(df)
  }
}

# =============================================================================
# TABLE C.4 - zone status test laid out by survey year
# =============================================================================
# Status is fitted as a single main effect (no year:status interaction), so
# its estimate/SE/p-value are the same in every year - this table repeats that
# shared test alongside the year-specific predicted means, for readability.

predict_at <- function(model, year_val, status_val, data) {

  newdat <- data.frame(
    year = factor(year_val, levels = model$xlevels$year),
    status = factor(status_val, levels = model$xlevels$status)
  )

  for (v in setdiff(gam_predictor_set(model), c("year", "status"))) {
    if (v %in% names(data)) {
      newdat[[v]] <- mean(data[[v]], na.rm = TRUE)
    }
  }

  pr <- mgcv::predict.gam(
    model,
    newdata = newdat,
    type = "response",
    se.fit = TRUE
  )

  tibble(
    fit = as.numeric(pr$fit),
    se = as.numeric(pr$se.fit)
  )
}

build_status_by_year_table <- function(models, data_for, response_order) {

  result <- purrr::imap_dfr(models, function(mod, resp) {

    terms <- gam_predictor_set(mod)

    # Only include models containing both year and status.
    if (!all(c("year", "status") %in% terms)) {
      return(NULL)
    }

    # Skip models without the required factor levels.
    if (
      is.null(mod$xlevels$year) ||
      length(mod$xlevels$year) < 1 ||
      is.null(mod$xlevels$status) ||
      length(mod$xlevels$status) < 2
    ) {
      return(NULL)
    }

    d <- data_for(resp)

    if (is.null(d) || !nrow(d)) {
      return(NULL)
    }

    levs <- mod$xlevels$status
    ref_lvl <- levs[1]
    alt_lvl <- setdiff(levs, ref_lvl)[1]

    status_row <- gam_term_pvalues(mod) %>%
      filter(startsWith(term, "status"))

    if (!length(alt_lvl) || !nrow(status_row)) {
      return(NULL)
    }

    purrr::map_dfr(mod$xlevels$year, function(yr) {

      ref <- predict_at(mod, yr, ref_lvl, d)
      alt <- predict_at(mod, yr, alt_lvl, d)

      tibble(
        response = resp,
        year = as.character(yr),
        !!paste0(ref_lvl, "_fit") := ref$fit,
        !!paste0(ref_lvl, "_se") := ref$se,
        !!paste0(alt_lvl, "_fit") := alt$fit,
        !!paste0(alt_lvl, "_se") := alt$se,
        status_estimate = status_row$estimate[1],
        status_se = status_row$se[1],
        status_p = status_row$p_value[1]
      )
    })
  })

  if (!nrow(result)) {
    warning(
      paste(
        "No fish models contained both year and status terms.",
        "Table C.4 will be empty."
      ),
      call. = FALSE
    )

    return(
      tibble(
        response = character(),
        year = character(),
        status_estimate = numeric(),
        status_se = numeric(),
        status_p = numeric()
      )
    )
  }

  result %>%
    mutate(response_key = factor(response, levels = response_order)) %>%
    arrange(response_key, year) %>%
    mutate(
      response = unname(response_labels[as.character(response_key)]),
      response = if_else(duplicated(response_key), "", response)
    ) %>%
    select(-response_key)
}

build_status_by_year_gt <- function(df) {

  if (!nrow(df)) {
    return(
      gt(tibble(Note = "No fish models contained both year and status terms."))
    )
  }

  fit_cols <- grep("_fit$", names(df), value = TRUE)
  se_cols  <- grep("_se$", names(df), value = TRUE)
  status_lvls <- sub("_fit$", "", fit_cols)

  if (length(fit_cols) < 2 || length(se_cols) < 2) {
    stop(
      "Table C.4 requires two status levels, each with a predicted mean and SE.",
      call. = FALSE
    )
  }

  body <- df %>%
    mutate(
      across(all_of(fit_cols), ~ num_fmt(.x, 3)),
      across(all_of(se_cols), ~ num_fmt(.x, 3)),
      status_estimate = num_fmt(status_estimate, 3),
      status_se = num_fmt(status_se, 3),
      status_p = dplyr::if_else(
        as.numeric(status_p) < 0.001,
        "<0.001",
        num_fmt(status_p, 3)
      )
    )

  labs <- c(
    response = "Response",
    year = "Year",
    status_estimate = "Status estimate",
    status_se = "Status SE",
    status_p = "Status p-value"
  )

  for (i in seq_along(fit_cols)) {
    labs[[fit_cols[i]]] <- paste0(status_lvls[i], " (predicted)")
    labs[[se_cols[i]]] <- paste0(status_lvls[i], " (SE)")
  }

  body %>%
    select(
      response, year,
      all_of(fit_cols[1]), all_of(se_cols[1]),
      all_of(fit_cols[2]), all_of(se_cols[2]),
      status_estimate, status_se, status_p
    ) %>%
    gt() %>%
    cols_label(.list = as.list(labs)) %>%
    cols_align(align = "left", columns = response) %>%
    cols_align(align = "center", columns = -response) %>%
    tab_options(
      table.width = pct(100),
      table.font.size = px(report_font_size),
      data_row.padding = report_row_padding,
      table_body.hlines.style = "none",
      column_labels.background.color = report_header_html
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
}

# PDF - kableExtra. Same reason as build_fish_term_pvalue_kable() above.
build_status_by_year_kable <- function(df) {

  if (!nrow(df)) {
    return(
      kableExtra::kbl(
        data.frame(Note = "No fish models contained both year and status terms."),
        booktabs = TRUE,
        escape = TRUE
      ) %>%
        kable_styling(
          latex_options = "hold_position",
          font_size = report_font_size,
          full_width = FALSE
        )
    )
  }

  fit_cols <- grep("_fit$", names(df), value = TRUE)
  se_cols <- grep("_se$", names(df), value = TRUE)
  status_lvls <- sub("_fit$", "", fit_cols)

  if (length(fit_cols) < 2 || length(se_cols) < 2) {
    stop(
      "Table C.4 requires two status levels, each with a predicted mean and SE.",
      call. = FALSE
    )
  }

  body <- df %>%
    mutate(
      across(all_of(fit_cols), ~ num_fmt(.x, 3)),
      across(all_of(se_cols), ~ num_fmt(.x, 3)),
      status_estimate = num_fmt(status_estimate, 3),
      status_se = num_fmt(status_se, 3),
      status_p = dplyr::if_else(
        as.numeric(status_p) < 0.001,
        "<0.001",
        num_fmt(status_p, 3)
      )
    ) %>%
    select(
      response, year,
      all_of(fit_cols[1]), all_of(se_cols[1]),
      all_of(fit_cols[2]), all_of(se_cols[2]),
      status_estimate, status_se, status_p
    )

  col_names <- c(
    "Response", "Year",
    paste0(status_lvls[1], " (predicted)"),
    paste0(status_lvls[1], " (SE)"),
    paste0(status_lvls[2], " (predicted)"),
    paste0(status_lvls[2], " (SE)"),
    "Status estimate", "Status SE", "Status p-value"
  )

  n_col <- ncol(body)

  body %>%
    kbl(
      booktabs = TRUE,
      escape = FALSE,
      linesep = "",
      col.names = col_names,
      align = c("l", rep("c", n_col - 1))
    ) %>%
    kable_styling(
      latex_options = c("scale_down", "hold_position"),
      font_size = report_font_size,
      full_width = FALSE
    ) %>%
    row_spec(0, bold = TRUE, background = report_header_latex)
}

build_status_by_year_report <- function(df) {

  if (knitr::is_latex_output()) {
    build_status_by_year_kable(df)
  } else {
    build_status_by_year_gt(df)
  }
}
