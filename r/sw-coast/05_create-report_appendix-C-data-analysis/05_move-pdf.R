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

# Built from `report_name` in 00_config.yml, which must match the two
# `output-file:` values in 04_quarto.qmd minus their extensions.
pdf_name  <- paste0(config$report_name, ".pdf")
html_name <- paste0(config$report_name, ".html")
html_files_dir <- "04_quarto_files" # supporting folder Quarto generates alongside the HTML

# TODO Check the drive letter and the appendix-C folder name match your machine
source_dir <- paste0(
  "r/", park,
  "/05_create-report_appendix-C-data-analysis"
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
