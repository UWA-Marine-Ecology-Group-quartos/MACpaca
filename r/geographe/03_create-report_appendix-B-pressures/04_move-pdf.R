# Clear your environment
rm(list = ls())

# Set the study name
script_dir <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) ""
)

# Fall back to the script's own folder if RStudio isn't driving the render
if (!nzchar(script_dir) || !file.exists(file.path(script_dir, "00_config.yml"))) {
  script_dir <- file.path("r", "geographe",
                          "03_create-report_appendix-B-pressures")
}

config <- yaml::read_yaml(file.path(script_dir, "00_config.yml"))
name <- config$name
park <- config$park

# TODO Change base_name to your quarto output name (no extension)
base_name <- "Project 4.21-Geographe-2-Appendix B-q-Pressures"

# Make sure Australian marine parks is set to working directory
src_dir  <- paste0("r/", park, "/03_create-report_appendix-B-pressures")
dest_dir <- paste0("quartos/", park)
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# Windows sometimes still holds a lock on a freshly written file, so retry
# a couple of times and fall back to copy + delete if the rename fails
move_output <- function(from, to) {
  if (!file.exists(from) && !dir.exists(from)) return(invisible(NULL))

  if (dir.exists(to) && !dir.exists(from)) unlink(to)
  if (dir.exists(from)) unlink(to, recursive = TRUE)

  for (i in 1:3) {
    ok <- suppressWarnings(file.rename(from, to))
    if (isTRUE(ok)) return(invisible(TRUE))
    Sys.sleep(1)
  }

  # Rename failed (file lock, or a different volume) - copy then remove
  ok <- file.copy(from, dirname(to), recursive = dir.exists(from),
                  overwrite = TRUE)
  if (isTRUE(all(ok))) unlink(from, recursive = TRUE)
  invisible(ok)
}

# PDF and HTML
for (ext in c(".pdf", ".html")) {
  f <- paste0(base_name, ext)
  move_output(file.path(src_dir, f), file.path(dest_dir, f))
}

# Supporting files folder (only created if embed-resources is switched off)
move_output(file.path(src_dir, paste0(base_name, "_files")),
            file.path(dest_dir, paste0(base_name, "_files")))
