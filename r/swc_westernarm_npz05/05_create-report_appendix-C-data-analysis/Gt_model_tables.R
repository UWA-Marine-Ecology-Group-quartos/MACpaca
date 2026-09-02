###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    Table builder for Table C 1.1 (habitat)
# Author:  Annika Leunig
# Date:    August 2026
###
# gt's LaTeX backend ignores cell_borders() and opt_row_striping(), so PDF is
# built with kableExtra and HTML with gt.
#
# REQUIRED in 04_quarto.qmd include-in-header, arydshln last:
#        \usepackage{arydshln}
#        \renewcommand{\arraystretch}{1.35}

library(gt)
library(dplyr)
library(kableExtra)
library(knitr)

# Shared look
report_header_html  <- "#D9D9D9"
report_header_latex <- "gray!25"
report_rule_colour  <- "#929292"
report_row_padding  <- px(7)
report_font_size    <- 10

# Fractions of \linewidth, must sum to 1
report_col_fracs <- c(response = 0.16, model = 0.34, delta_aicc = 0.12,
                      omega_aicc = 0.12, r2 = 0.14, edf = 0.12)

# Letter (21.59cm) less 2cm margins
report_text_width_cm <- 17.59
report_tabcolsep_cm  <- 0.4214

col_width <- function(frac, n_col) {
  avail <- report_text_width_cm - n_col * report_tabcolsep_cm
  paste0(round(frac * avail, 3), "cm")
}

# Number formatting
num_fmt <- function(x, digits, drop_trailing = FALSE) {
  out <- formatC(as.numeric(x), format = "f", digits = digits)
  if (drop_trailing) {
    out <- sub("(\\.\\d*?)0+$", "\\1", out)
    out <- sub("\\.$", "", out)
  }
  out[is.na(x)] <- "-"
  out
}

format_numeric_cols <- function(df) {
  df %>%
    mutate(
      delta_aicc = num_fmt(delta_aicc, 3, drop_trailing = TRUE),
      omega_aicc = num_fmt(omega_aicc, 3, drop_trailing = TRUE),
      r2         = num_fmt(r2,         5, drop_trailing = TRUE),
      edf        = num_fmt(edf,        2)
    )
}

# Row bookkeeping
report_row_index <- function(df) {
  grp_start <- which(df$response != "")
  list(
    selected  = which(df$selected),
    grp_start = grp_start,
    grp_last  = utils::head(grp_start[-1] - 1L, length(grp_start) - 1L),
    grp_size  = diff(c(grp_start, nrow(df) + 1L))
  )
}

column_labels_md <- function() {
  c(response = "Response", model = "Model",
    delta_aicc = "$\\Delta$ AICc", omega_aicc = "$\\omega$ AICc",
    r2 = "$R^2$", edf = "EDF")
}

# HTML - gt
build_report_gt <- function(df) {

  idx  <- report_row_index(df)
  body <- df %>% select(-selected) %>% format_numeric_cols()

  tbl <- body %>%
    gt() %>%
    cols_label(.list = lapply(column_labels_md(), md)) %>%
    cols_align(align = "left",   columns = response) %>%
    cols_align(align = "center", columns = -response) %>%
    tab_options(
      table.width             = pct(100),
      table.font.size         = px(report_font_size),
      data_row.padding        = report_row_padding,
      table_body.hlines.style = "none",
      row.striping.include_table_body = FALSE,
      column_labels.background.color  = report_header_html
    ) %>%
    tab_style(style = cell_text(v_align = "middle"),
              locations = cells_body()) %>%
    tab_style(style = cell_text(v_align = "middle", weight = "bold"),
              locations = cells_column_labels()) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(rows = idx$selected)) %>%
    tab_style(style = cell_borders(sides = "bottom", style = "dashed",
                                   color = report_rule_colour, weight = px(1)),
              locations = cells_body(rows = idx$grp_last))

  # Formulas built up front - gt resolves the LHS in tidyselect's data mask,
  # where a loop variable is not visible
  width_specs <- lapply(names(report_col_fracs), function(cn) {
    stats::as.formula(
      paste0("`", cn, "` ~ pct(", 100 * report_col_fracs[[cn]], ")"),
      env = environment()
    )
  })

  do.call(gt::cols_width, c(list(tbl), width_specs))
}

# PDF - kableExtra
build_report_kable <- function(df, centre_response = FALSE) {

  idx  <- report_row_index(df)
  body <- df %>% select(-selected) %>% format_numeric_cols()

  n_col <- ncol(body)

  dash <- paste0(
    "\\noalign{\\arrayrulecolor{gray}}",
    "\\cdashline{1-", n_col, "}",
    "\\noalign{\\arrayrulecolor{black}}"
  )

  # collapse_rows() is unreliable under longtable, so \multirow by hand
  if (centre_response) {
    for (g in seq_along(idx$grp_start)) {
      first <- idx$grp_start[g]
      n     <- idx$grp_size[g]
      if (n <= 1L) next
      lab   <- body$response[first]
      body$response[first] <- ""
      mid   <- first + (n - 1L) %/% 2L
      body$response[mid] <- paste0("\\multirow{1}{=}{", lab, "}")
    }
  }

  k <- body %>%
    kbl(
      booktabs  = TRUE,
      longtable = TRUE,
      escape    = FALSE,
      linesep   = "",
      col.names = unname(column_labels_md()),
      align     = c("l", rep("c", n_col - 1))
    ) %>%
    kable_styling(
      latex_options = c("repeat_header", "hold_position"),
      font_size     = report_font_size,
      full_width    = FALSE
    ) %>%
    row_spec(0, bold = TRUE, background = report_header_latex)

  for (i in seq_len(n_col)) {
    k <- k %>% column_spec(i, width = col_width(report_col_fracs[[i]], n_col),
                           latex_valign = "m")
  }

  if (length(idx$selected)) k <- k %>% row_spec(idx$selected, bold = TRUE)
  if (length(idx$grp_last)) k <- k %>% row_spec(idx$grp_last,
                                                extra_latex_after = dash)
  k
}

# Dispatch
build_report_table <- function(df, centre_response = FALSE) {
  if (knitr::is_latex_output()) {
    build_report_kable(df, centre_response = centre_response)
  } else {
    build_report_gt(df)
  }
}

# TODO switch centre_response on if the groups run several rows deep
build_habitat_model_table <- function(df) {
  build_report_table(df, centre_response = FALSE)
}
