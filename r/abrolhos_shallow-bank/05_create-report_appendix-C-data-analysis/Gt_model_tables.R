###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    Table builders for Table C.1 (habitat) and Table C.2 (fish)
# Author:  Annika Leunig
# Date:    August 2026
###
# Captions live in the .qmd chunk options (tbl-cap), not here - that keeps the
# LaTeX float and the "Table C x.y" counter under Quarto's control.
#
# WHY TWO BACKENDS
# gt's LaTeX backend silently drops two things this table needs:
#   * cell_borders()      - ignored entirely (rstudio/gt#1712), so the dashed
#                           response separators never appeared in the PDF
#   * opt_row_striping()  - ignored (rstudio/gt#1625)
# The documented gt workaround (explicit cell_fill + quarto.disable_processing)
# would take table processing away from Quarto and break the tbl-cap counter,
# so PDF output is built with kableExtra instead. HTML keeps using gt.
#
# REQUIRED IN 04_quarto.qmd - inside the single include-in-header block,
# with arydshln loaded last:
#        \usepackage{arydshln}
#        \renewcommand{\arraystretch}{1.35}   % <- row height in the PDF

library(gt)
library(dplyr)
library(kableExtra)
library(knitr)

# ---- shared look -------------------------------------------------------------
# Header only: the body is unshaded in both formats.
report_header_html  <- "#D9D9D9"
report_header_latex <- "gray!25"   # xcolor expression
report_rule_colour  <- "#929292"   # dashed response separator, HTML
report_row_padding  <- px(7)       # HTML row height; PDF uses \arraystretch
report_font_size    <- 10

# Column widths as fractions of \linewidth. Each set sums to 1, so the table
# spans the full text block and therefore matches the caption width exactly.
# Order matches the rendered column order (aicc sits after omega_aicc).
#
# TODO If a Model string wraps awkwardly (e.g. this park's final models carry
# more factor terms than the default widths expect), take some width off r2
# and give it to model - just keep each set summing to 1.
report_col_fracs <- list(
  habitat = c(response = 0.16, model = 0.34, delta_aicc = 0.12,
              omega_aicc = 0.12, r2 = 0.14, edf = 0.12),
  fish    = c(response = 0.14, model = 0.30, delta_aicc = 0.11,
              omega_aicc = 0.11, aicc = 0.12, r2 = 0.12, edf = 0.10)
)

# Text block width, from the qmd geometry: letter (21.59cm) less 2cm margins.
# Update this if those margins change.
report_text_width_cm <- 17.59
report_tabcolsep_cm  <- 0.4214   # 2 x \tabcolsep at the 6pt default

# \tabcolsep is added on both sides of every column, so it has to come back out
# of each width or the row overruns the text block and LaTeX reports an
# overfull hbox. Widths must be plain lengths, not \dimexpr: column_spec()
# feeds the current column spec back through sub() as a regex, and backslashes
# or braces in a width make that pattern invalid on the second call.
col_width <- function(frac, n_col) {
  avail <- report_text_width_cm - n_col * report_tabcolsep_cm
  paste0(round(frac * avail, 3), "cm")
}

# ---- number formatting -------------------------------------------------------
# gt does this via fmt_number(); kableExtra needs the columns pre-formatted, so
# both paths go through here to guarantee the two outputs agree.
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
      edf        = num_fmt(edf,        2),
      across(any_of("aicc"), ~ num_fmt(.x, 2))
    )
}

# ---- row bookkeeping ---------------------------------------------------------
# No row groups: format_report_table() blanks the response label on duplicate
# rows so each response appears once inline. A non-empty label therefore marks
# the first row of a new response group.
report_row_index <- function(df) {
  grp_start <- which(df$response != "")
  list(
    selected  = which(df$selected),
    grp_start = grp_start,
    # last row of every group except the final one - the table's own bottom
    # border closes that group, so it needs no separator
    grp_last  = utils::head(grp_start[-1] - 1L, length(grp_start) - 1L),
    # group sizes, in the order the groups appear
    grp_size  = diff(c(grp_start, nrow(df) + 1L))
  )
}

column_labels_md <- function(show_aicc) {
  labs <- c(response = "Response", model = "Model",
            delta_aicc = "$\\Delta$ AICc", omega_aicc = "$\\omega$ AICc",
            r2 = "$R^2$", edf = "EDF")
  if (show_aicc) labs <- append(labs, c(aicc = "AICc"), after = 4)
  labs
}

# =============================================================================
# HTML - gt
# =============================================================================
build_report_gt <- function(df, show_aicc = FALSE) {

  idx  <- report_row_index(df)
  body <- df %>% select(-selected) %>% format_numeric_cols()

  if (show_aicc) body <- body %>% relocate(aicc, .after = omega_aicc)

  fracs <- report_col_fracs[[if (show_aicc) "fish" else "habitat"]]

  tbl <- body %>%
    gt() %>%
    cols_label(.list = lapply(column_labels_md(show_aicc), md)) %>%
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

  # same proportions as the PDF.
  # The width specs are built as complete formulas and passed as arguments,
  # rather than looped over with cols_width(all_of(nm) ~ ...). gt resolves the
  # left-hand side inside tidyselect's own data mask, where a loop variable
  # called `nm` is not visible - hence "object 'nm' not found".
  width_specs <- lapply(names(fracs), function(cn) {
    stats::as.formula(
      paste0("`", cn, "` ~ pct(", 100 * fracs[[cn]], ")"),
      env = environment()
    )
  })

  do.call(gt::cols_width, c(list(tbl), width_specs))
}

# =============================================================================
# PDF - kableExtra
# =============================================================================
# extra_latex_after fires immediately after the row's \\, which is where
# arydshln's \cdashline belongs. \cdashline opens its own \noalign group, so
# the colour switches need \noalign groups of their own. No \global: colortbl's
# \arrayrulecolor already assigns globally, and prefixing it errors out.
#
# centre_response = TRUE merges each response label into a \multirow spanning
# its candidate models, vertically centred. Used for Table C.2, where the
# groups are several rows deep. Worth switching on for C.1 too if the habitat
# groups come back more than one row deep once you have seen the table - pass
# centre_response = TRUE in build_habitat_model_table() below.
build_report_kable <- function(df, show_aicc = FALSE, centre_response = FALSE) {

  idx  <- report_row_index(df)
  body <- df %>% select(-selected) %>% format_numeric_cols()

  if (show_aicc) body <- body %>% relocate(aicc, .after = omega_aicc)

  n_col <- ncol(body)
  fracs <- report_col_fracs[[if (show_aicc) "fish" else "habitat"]]

  dash <- paste0(
    "\\noalign{\\arrayrulecolor{gray}}",
    "\\cdashline{1-", n_col, "}",
    "\\noalign{\\arrayrulecolor{black}}"
  )

  # Vertically centre the response label across its group. Done by hand rather
  # than with collapse_rows(), which is unreliable under longtable: the label
  # moves to the middle row of the group and \multirow lifts it by the right
  # number of lines.
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
      linesep   = "",                       # no booktabs 5-row gap
      col.names = unname(column_labels_md(show_aicc)),
      align     = c("l", rep("c", n_col - 1))
    ) %>%
    kable_styling(
      latex_options = c("repeat_header", "hold_position"),   # no striping
      font_size     = report_font_size,
      full_width    = FALSE       # TRUE forces xltabular, which arydshln breaks
    ) %>%
    row_spec(0, bold = TRUE, background = report_header_latex)

  # latex_valign = "m" middle-aligns the fixed-width columns; without it
  # kableExtra emits \begin{minipage}[t] and text sits at the top of the row.
  for (i in seq_len(n_col)) {
    k <- k %>% column_spec(i, width = col_width(fracs[[i]], n_col),
                           latex_valign = "m")
  }

  if (length(idx$selected)) k <- k %>% row_spec(idx$selected, bold = TRUE)
  if (length(idx$grp_last)) k <- k %>% row_spec(idx$grp_last,
                                                extra_latex_after = dash)
  k
}

# =============================================================================
# Dispatch
# =============================================================================
build_report_table <- function(df, show_aicc = FALSE, centre_response = FALSE) {
  if (knitr::is_latex_output()) {
    build_report_kable(df, show_aicc = show_aicc,
                       centre_response = centre_response)
  } else {
    build_report_gt(df, show_aicc = show_aicc)
  }
}

build_habitat_model_table <- function(df) {
  build_report_table(df, show_aicc = FALSE, centre_response = FALSE)
}

build_fish_model_table <- function(df) {
  build_report_table(df, show_aicc = TRUE, centre_response = TRUE)
}
