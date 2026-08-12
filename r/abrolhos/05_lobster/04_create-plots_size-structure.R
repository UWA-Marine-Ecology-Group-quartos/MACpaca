###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Size structure and zone comparison plots
# Author:  Henry Evans
# Date:    August 2026
###

rm(list = ls())

library(tidyverse)

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
name <- config$name
legal_size_mm <- config$legal_size_mm

tidy_dir <- "data/abrolhos/tidy/lobster"
plot_dir <- "plots/abrolhos/lobster"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

zone_colours <- c("National Park Zone"   = "#7bbc63",
                  "Special Purpose Zone" = "#6daff4")

# In the by-sex plots the zone still sets the hue, and sex sets the shade: males
# get a lighter tint of the same zone colour. Keyed on interaction(zone, sex),
# which ggplot builds with a "." separator.
sex_zone_colours <- c(
  setNames(unname(zone_colours), paste0(names(zone_colours), ".Female")),
  setNames(colorspace::lighten(unname(zone_colours), 0.5),
           paste0(names(zone_colours), ".Male"))
)

measurements <- read.csv(file.path(tidy_dir, paste0(name, "_lobster-measurements_all-years.csv")),
                         colClasses = c(pot_number = "character")) %>%
  dplyr::filter(species %in% "Western Rock Lobster", !is.na(carapace_length_mm)) %>%
  dplyr::mutate(year = factor(year))

catch_per_pot <- read.csv(file.path(tidy_dir, paste0(name, "_lobster-catch-per-pot.csv")),
                          colClasses = c(pot_number = "character")) %>%
  dplyr::mutate(year = factor(year))

# Size distribution by zone and year -------------------------------------------

ggplot(measurements, aes(x = carapace_length_mm, fill = zone)) +
  geom_histogram(binwidth = 5, colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = legal_size_mm, colour = "red", linetype = "dashed") +
  scale_fill_manual(values = zone_colours) +
  facet_grid(year ~ zone) +
  labs(x = "Carapace length (mm)", y = "Number of lobsters",
       fill = "Zone",
       caption = paste0("Dashed line shows the ", legal_size_mm, " mm legal minimum size")) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(file.path(plot_dir, paste0(name, "_lobster-size-distribution_zone-year.png")),
       height = 6, width = 8, dpi = 300, bg = "white")

# Size distribution by sex -----------------------------------------------------

ggplot(dplyr::filter(measurements, !sex %in% "Unknown"),
       aes(x = carapace_length_mm, fill = interaction(zone, sex))) +
  geom_histogram(binwidth = 5, colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = legal_size_mm, colour = "red", linetype = "dashed") +
  scale_fill_manual(values = sex_zone_colours) +
  facet_grid(year + sex ~ zone) +
  labs(x = "Carapace length (mm)", y = "Number of lobsters",
       caption = "Lighter bars are males") +
  theme_classic() +
  theme(legend.position = "none")

ggsave(file.path(plot_dir, paste0(name, "_lobster-size-distribution_sex.png")),
       height = 8, width = 8, dpi = 300, bg = "white")

# Standardised size distributions ----------------------------------------------
# As above, but each panel is scaled to sum to 100% so the shape of the size
# distribution can be compared between zones and years independently of how many
# lobsters were caught. Sample sizes are labelled because a percentage from a
# small sample is a lot noisier than the same percentage from a large one.
#
# The percentages are computed here rather than with after_stat(count/sum(count)),
# because in ggplot2 that expression sums over the whole dataset rather than
# within each facet, which would leave the abundance signal in the plot.

binwidth <- 5

bin_percent <- function(data, ...) {
  data %>%
    dplyr::mutate(bin = floor(carapace_length_mm / binwidth) * binwidth + binwidth / 2) %>%
    dplyr::count(..., bin, name = "count") %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(percent = count / sum(count) * 100,
                  n = sum(count)) %>%
    dplyr::ungroup()
}

zone_year_percent <- bin_percent(measurements, year, zone)

panel_n_zone_year <- zone_year_percent %>%
  dplyr::distinct(year, zone, n)

ggplot(zone_year_percent, aes(x = bin, y = percent, fill = zone)) +
  geom_col(width = binwidth, colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = legal_size_mm, colour = "red", linetype = "dashed") +
  geom_text(data = panel_n_zone_year, aes(x = Inf, y = Inf, label = paste0("n = ", n)),
            hjust = 1.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = zone_colours) +
  facet_grid(year ~ zone) +
  labs(x = "Carapace length (mm)", y = "Percentage of lobsters measured (%)",
       fill = "Zone",
       caption = paste0("Each panel sums to 100%. Dashed line shows the ",
                        legal_size_mm, " mm legal minimum size")) +
  theme_classic() +
  theme(legend.position = "none")

ggsave(file.path(plot_dir, paste0(name, "_lobster-size-distribution_zone-year_standardised.png")),
       height = 6, width = 8, dpi = 300, bg = "white")

measurements_sex <- dplyr::filter(measurements, !sex %in% "Unknown")

sex_percent <- bin_percent(measurements_sex, year, sex, zone)

panel_n_sex <- sex_percent %>%
  dplyr::distinct(year, sex, zone, n)

ggplot(sex_percent, aes(x = bin, y = percent, fill = interaction(zone, sex))) +
  geom_col(width = binwidth, colour = "black", linewidth = 0.2) +
  geom_vline(xintercept = legal_size_mm, colour = "red", linetype = "dashed") +
  geom_text(data = panel_n_sex, aes(x = Inf, y = Inf, label = paste0("n = ", n)),
            hjust = 1.1, vjust = 1.5, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = sex_zone_colours) +
  facet_grid(year + sex ~ zone) +
  labs(x = "Carapace length (mm)", y = "Percentage of lobsters measured (%)",
       caption = "Each panel sums to 100%. Lighter bars are males") +
  theme_classic() +
  theme(legend.position = "none")

ggsave(file.path(plot_dir, paste0(name, "_lobster-size-distribution_sex_standardised.png")),
       height = 8, width = 8, dpi = 300, bg = "white")

# Confirm every panel sums to 100% before the plots are trusted
stopifnot(all(abs(dplyr::summarise(dplyr::group_by(zone_year_percent, year, zone),
                                   total = sum(percent))$total - 100) < 1e-8))
stopifnot(all(abs(dplyr::summarise(dplyr::group_by(sex_percent, year, sex, zone),
                                   total = sum(percent))$total - 100) < 1e-8))

# Catch rate by zone and year --------------------------------------------------

catch_summary <- catch_per_pot %>%
  dplyr::group_by(year, zone) %>%
  dplyr::summarise(mean_per_pot = mean(n_total),
                   se_per_pot   = sd(n_total) / sqrt(n()),
                   n_pots       = n(),
                   .groups = "drop")

ggplot(catch_summary, aes(x = year, y = mean_per_pot, fill = zone)) +
  geom_col(colour = "black", position = position_dodge(0.9), linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_per_pot - se_per_pot, ymax = mean_per_pot + se_per_pot),
                width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = paste0("n=", n_pots), y = 0.15),
            position = position_dodge(0.9), size = 3) +
  scale_fill_manual(values = zone_colours) +
  labs(x = "Year", y = "Lobsters per pot (mean +/- SE)", fill = "Zone") +
  theme_classic()

ggsave(file.path(plot_dir, paste0(name, "_lobster-catch-rate_zone-year.png")),
       height = 5, width = 7, dpi = 300, bg = "white")

# Legal and sublegal catch rate ------------------------------------------------

legal_summary <- catch_per_pot %>%
  tidyr::pivot_longer(c(n_legal, n_sublegal),
                      names_to = "size_class", values_to = "count") %>%
  dplyr::mutate(size_class = if_else(size_class %in% "n_legal", "Legal", "Sublegal")) %>%
  dplyr::group_by(year, zone, size_class) %>%
  dplyr::summarise(mean_per_pot = mean(count),
                   se_per_pot   = sd(count) / sqrt(n()),
                   .groups = "drop")

ggplot(legal_summary, aes(x = zone, y = mean_per_pot, fill = zone)) +
  geom_col(colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = mean_per_pot - se_per_pot, ymax = mean_per_pot + se_per_pot),
                width = 0.2) +
  scale_fill_manual(values = zone_colours) +
  facet_grid(size_class ~ year) +
  labs(x = NULL, y = "Lobsters per pot (mean +/- SE)", fill = "Zone") +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

ggsave(file.path(plot_dir, paste0(name, "_lobster-catch-rate_legal-sublegal.png")),
       height = 6, width = 7, dpi = 300, bg = "white")

message("Size structure plots written to ", plot_dir)
print(as.data.frame(catch_summary))
