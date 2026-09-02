# Clear your environment
rm(list = ls())

# Set the study name
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
config <- yaml::read_yaml(file.path(script_dir, "00_config.yml"))
name <- config$name
park <- config$park

pdf_name <- "Project 4.21-swc_westernarm-2-Appendix B-q-Pressures.pdf"
html_name <- "Project 4.21-swc_westernarm-2-Appendix B-q-Pressures.html"

# Make sure Australian marine parks is set to working directory
dest_dir <- paste0("quartos/", park)
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

source_dir <- paste0("r/", park, "/03_create-report_appendix-B-pressures")

file.rename(
  from = file.path(source_dir, pdf_name),
  to = file.path(dest_dir, pdf_name)
)

# embed-resources is on for this appendix's html format, so it's a single
# self-contained file - no supporting _files folder to move alongside it.
file.rename(
  from = file.path(source_dir, html_name),
  to = file.path(dest_dir, html_name)
)
