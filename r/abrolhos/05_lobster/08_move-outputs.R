# Move the rendered report(s) into quartos/ alongside the other park reports
rm(list = ls())

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
park <- config$park

report_name <- "Project 4.21-Abrolhos-2-Appendix A-q-Lobster"
extensions  <- c("html", "pdf")

source_dir <- file.path("r", park, "05_lobster")
dest_dir   <- file.path("quartos", park)
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

for (ext in extensions) {
  file_name <- paste0(report_name, ".", ext)
  from <- file.path(source_dir, file_name)

  if (!file.exists(from)) {
    message("Not found, skipping: ", file_name)
    next
  }

  moved <- file.rename(from = from, to = file.path(dest_dir, file_name))
  message(if (moved) "Moved: " else "FAILED to move: ", file_name)
}
