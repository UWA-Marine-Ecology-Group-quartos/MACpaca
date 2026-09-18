###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    Table builder for the fish GAMM significance table (Table C1.3) -
#          zone status p-values per fish metric
# Author:  Annika Leunig
# Date:    September 2026
###
# Uses response_labels, gam_predictor_set() etc from 02_load-model-selection.R
# and the same gt() styling helpers (report_header_html, report_row_padding,
# report_font_size) from Gt_model_tables.R - source both before this file.
#
# Ningaloo's fish models only ever carry a `status` term (never `year` - fish
# years are pooled), so only Table C1.3 (year/status significance) applies
# here. The TEMPLATE's status-by-year table is dropped: it requires both
# `year` and `status` in a model before it reports anything, so it would
# always come back empty for this park.

library(gt)
library(dplyr)
library(kableExtra)
library(knitr)

# ---- pull the status rows out of a GAM's parametric coefficient table ------
# summary.gam()$p.table has one row per dummy-coded factor level (not one row
# per whole factor) - e.g. "statusNo-Take". Column names for the test
# statistic/p-value differ by family ("t value"/"z value"), so they are
# picked up by position rather than by name.
gam_term_pvalues <- function(model) {
  pt <- as.data.frame(summary(model)$p.table)
  pt$term <- rownames(pt)
  pt <- pt[pt$term != "(Intercept)", , drop = FALSE]
  pt <- pt[grepl("^status", pt$term), , drop = FALSE]

  tibble(
    term      = pt$term,
    estimate  = pt[[1]],
    se        = pt[[2]],
    statistic = pt[[3]],
    p_value   = pt[[4]]
  )
}

# Turn "statusNo-Take" into a readable contrast, using the reference (first)
# level actually fitted in that model.
describe_term <- function(term, model) {
  ref <- model$xlevels$status[1]
  lvl <- sub("^status", "", term)
  paste0("Zone status: ", lvl, " vs ", ref)
}

# =============================================================================
# TABLE C1.3 - zone status significance, per fish GAMM
# =============================================================================
# One row per factor-level contrast (per-level p-values), for every fish
# model that carries a status term.
build_fish_term_pvalue_table <- function(models, response_order) {
  purrr::imap_dfr(models, function(mod, resp) {
    pv <- gam_term_pvalues(mod)
    if (!nrow(pv)) return(NULL)
    pv %>%
      mutate(
        response = resp,
        contrast = vapply(term, describe_term, character(1), model = mod),
        .before  = 1
      )
  }) %>%
    mutate(response_key = factor(response, levels = response_order)) %>%
    arrange(response_key, term) %>%
    mutate(
      response = unname(response_labels[as.character(response_key)]),
      response = if_else(duplicated(response_key), "", response)
    ) %>%
    select(response, contrast, estimate, se, statistic, p_value)
}

format_pvalue_table <- function(df) {
  df %>%
    mutate(
      estimate  = num_fmt(estimate,  3),
      se        = num_fmt(se,        3),
      statistic = num_fmt(statistic, 3),
      p_value   = dplyr::if_else(as.numeric(p_value) < 0.001, "<0.001",
                                 num_fmt(p_value, 3))
    )
}

build_fish_term_pvalue_gt <- function(df) {
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
    cols_align(align = "left",   columns = c(response, contrast)) %>%
    cols_align(align = "center", columns = c(estimate, se, statistic, p_value)) %>%
    tab_options(
      table.width             = pct(100),
      table.font.size         = px(report_font_size),
      data_row.padding        = report_row_padding,
      table_body.hlines.style = "none",
      column_labels.background.color = report_header_html
    ) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels())
}

# PDF - kableExtra. gt's own LaTeX backend corrupts this table (see the "WHY
# TWO BACKENDS" note at the top of Gt_model_tables.R) - same fix, same pattern.
build_fish_term_pvalue_kable <- function(df) {
  body  <- df %>% format_pvalue_table()
  n_col <- ncol(body)

  body %>%
    kbl(
      booktabs  = TRUE,
      longtable = TRUE,
      escape    = FALSE,
      linesep   = "",
      col.names = c("Response", "Term", "Estimate", "SE", "Statistic", "p-value"),
      align     = c("l", "l", rep("c", n_col - 2))
    ) %>%
    kable_styling(
      latex_options = c("repeat_header", "hold_position"),
      font_size     = report_font_size,
      full_width    = FALSE
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
