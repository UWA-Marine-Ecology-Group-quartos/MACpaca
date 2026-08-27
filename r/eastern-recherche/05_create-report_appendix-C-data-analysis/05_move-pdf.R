###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Task:    Post-render - move the Appendix C outputs into quartos/<park>/
# Author:  Annika Leunig
# Date:    August 2026
###
# =============================================================================

# Clear your environment
rm(list = ls())

# fs is used because base R's file.rename() struggles with moving non-empty
# folders on Windows
library(fs)
library(here)

# Locate this folder. Works in RStudio, in a plain R session, and when Quarto
# renders. rstudioapi::isAvailable() returns FALSE outside RStudio, whereas
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

config <- yaml::read_yaml(file.path(appc_dir, "00_config.yml"))

park        <- config$park
report_name <- config$report_name

if (is.null(report_name) || !nzchar(report_name)) {
  stop("`report_name` is missing from 00_config.yml. It must match the ",
       "`output-file:` in 04_quarto.qmd, minus the extension.")
}

pdf_name       <- paste0(report_name, ".pdf")
html_name      <- paste0(report_name, ".html")
html_files_dir <- "04_quarto_files"   # supporting folder Quarto generates alongside the HTML

# Quarto writes its output next to the .qmd, so the source is this folder
source_dir <- appc_dir
dest_dir   <- here("quartos", park)

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

# --- Move the HTML (only rendered if an html format is added to the qmd) ---
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
  # (this is what silently blocks file.rename())
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

