rm(list = ls())

# fs handles non-empty folders on Windows, file.rename() does not
library(fs)

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)
config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)
name <- config$name
park <- config$park

# Must match the two output-file values in 04_quarto.qmd
pdf_name  <- paste0(config$report_name, ".pdf")
html_name <- paste0(config$report_name, ".html")
html_files_dir <- "04_quarto_files"

# TODO check the drive letter and folder name
source_dir <- paste0(
  "r/", park,
  "/05_create-report_appendix-C-data-analysis"
)
dest_dir <- paste0("quartos/", park)

if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# PDF
if (file.exists(file.path(source_dir, pdf_name))) {
  dest_pdf_path <- file.path(dest_dir, pdf_name)

  if (file.exists(dest_pdf_path)) file_delete(dest_pdf_path)

  file_move(
    path = file.path(source_dir, pdf_name),
    new_path = dest_pdf_path
  )
  message("Moved PDF to: ", dest_pdf_path)
} else {
  message("No PDF found to move at: ", file.path(source_dir, pdf_name))
}

# HTML
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

# Supporting files
if (dir.exists(file.path(source_dir, html_files_dir))) {
  dest_files_path <- file.path(dest_dir, html_files_dir)

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
