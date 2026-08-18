###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    gt builders for Table C 1.1 (habitat) and Table C 2.1 (fish)
# Author:  Annika Leunig
# Date:    August 2026
###

# Captions live in the .qmd chunk options (tbl-cap), not here - that keeps the
# LaTeX float and the "Table C x.y" counter under Quarto's control.

library(gt)
library(dplyr)

build_report_table <- function(df, show_aicc = FALSE) {

  sel_rows <- which(df$selected)
  body <- df %>% dplyr::select(-selected)

  tbl <- body %>%
    gt() %>%
    cols_label(
      response   = "Response",
      model      = "Model",
      delta_aicc = md("$\\Delta$ AICc"),   # if this renders literally in the PDF,
      omega_aicc = md("$\\omega$ AICc"),   # swap both for plain "Delta AICc" etc.
      r2         = md("R$^2$"),
      edf        = "EDF"
    ) %>%
    fmt_number(columns = c(delta_aicc, omega_aicc), decimals = 3,
               drop_trailing_zeros = TRUE) %>%
    fmt_number(columns = r2,  decimals = 5, drop_trailing_zeros = TRUE) %>%
    fmt_number(columns = edf, decimals = 2) %>%
    sub_missing(missing_text = "-") %>%
    cols_align(align = "left",   columns = c(response)) %>%
    cols_align(align = "center", columns = c(model, delta_aicc, omega_aicc, r2, edf)) %>%
    cols_width(response ~ pct(24), model ~ pct(30)) %>%
    # Selected (most parsimonious) model shown in bold, as per the caption
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(columns = everything(), rows = sel_rows)
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_options(
      table.width                    = pct(100),
      table.font.size                = px(10),
      data_row.padding               = px(3),
      column_labels.border.top.style = "solid",
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      table.border.top.style         = "none",
      table.border.bottom.color      = "black",
      table_body.hlines.style        = "none",
      row.striping.include_table_body = FALSE
    )

  if (show_aicc) {
    tbl <- tbl %>%
      cols_label(aicc = "AICc") %>%
      fmt_number(columns = aicc, decimals = 2) %>%
      cols_align(align = "center", columns = aicc) %>%
      cols_move(columns = aicc, after = omega_aicc)
  }

  tbl
}

build_habitat_model_table <- function(df) build_report_table(df, show_aicc = FALSE)
build_fish_model_table    <- function(df) build_report_table(df, show_aicc = TRUE)
