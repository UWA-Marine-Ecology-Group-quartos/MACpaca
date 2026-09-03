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

# Quarto output names - must match the `output-file:` entries in the qmd YAML
pdf_name  <- "Project 4.21-SWC-western-arm-2-Appendix A2-q-Natural values.pdf"
html_name <- "Project 4.21-SWC-western-arm-2-Appendix A2-q-Natural values.html"

# The appendix folder is wherever this script lives, so the npz build
# (r/swc_westernarm_npz05/...) is picked up without editing a hard-coded path.
source_dir <- script_dir

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

# --- Move the HTML's supporting _files directories ---
# Quarto names these after the .qmd file, not the output file, so they are
# detected rather than hard-coded - the npz qmd may be numbered differently.
html_files_dirs <- list.dirs(source_dir, recursive = FALSE, full.names = FALSE)
html_files_dirs <- html_files_dirs[grepl("_files$", html_files_dirs)]

if (length(html_files_dirs) > 0) {

  for (html_files_dir in html_files_dirs) {

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
  }

} else {
  message("No supporting files folder found in: ", source_dir)
}
