###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Quarto render outputs
# Task:    Move rendered Appendix A outputs into the quartos/ folder
# Author:  Annika Leunig
# Date:    August 2026
###

# Clear your environment
rm(list = ls())

# Load the fs package for robust file/folder operations
# (base R's file.rename() struggles with moving non-empty folders on Windows)
library(fs)

# Set the study name
script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park

# TODO Change report_name to your quarto output name, or set `report_name:` in
# 00_config.yml. Either way it must match the `output-file:` entries in
# 09_quarto.qmd, minus the extension.
report_name <- if (!is.null(config$report_name)) {
  config$report_name
} else {
  "Project 4.21-Eastern Recherche-2-Appendix A-q-Natural values"
}

pdf_name  <- paste0(report_name, ".pdf")
html_name <- paste0(report_name, ".html")

html_files_dir <- "09_quarto_files" # supporting folder Quarto generates alongside the HTML

source_dir <- paste0(
  "r/", park,
  "/02_create-report_appendix-A-natural-values"
)

dest_dir <- paste0("quartos/", park)

if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# --- Move the PDF ---
if (file.exists(file.path(source_dir, pdf_name))) {
  dest_pdf_path <- file.path(dest_dir, pdf_name)

  # Remove any stale copy left over from a previous run
  if (file.exists(dest_pdf_path)) file_delete(dest_pdf_path)

  file_move(
    path = file.path(source_dir, pdf_name),
    new_path = dest_pdf_path
  )
  message("Moved PDF to: ", dest_pdf_path)
} else {
  message("No PDF found to move at: ", file.path(source_dir, pdf_name))
}

# --- Move the HTML ---
if (file.exists(file.path(source_dir, html_name))) {
  dest_html_path <- file.path(dest_dir, html_name)

  if (file.exists(dest_html_path)) file_delete(dest_html_path)

  file_move(
    path = file.path(source_dir, html_name),
    new_path = dest_html_path
  )
  message("Moved HTML to: ", dest_html_path)
} else {
  message("No HTML found to move at: ", file.path(source_dir, html_name))
}

# --- Move the HTML's supporting _files directory ---
if (dir.exists(file.path(source_dir, html_files_dir))) {
  dest_files_path <- file.path(dest_dir, html_files_dir)

  # Remove any stale leftover folder from a previous run
  # (this is what was silently blocking file.rename() before)
  if (dir.exists(dest_files_path)) dir_delete(dest_files_path)

  dir_copy(
    path = file.path(source_dir, html_files_dir),
    new_path = dest_files_path
  )
  dir_delete(file.path(source_dir, html_files_dir))

  message("Moved supporting files folder to: ", dest_files_path)
} else {
  message("No supporting files folder found at: ", file.path(source_dir, html_files_dir))
}

