###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Tweedie GLM of legal and sublegal catch, inside vs outside the NPZ
# Author:  Henry Evans
# Date:    August 2026
###

# Model follows the specification provided by the project supervisor:
#   legal    ~ status * year + depth_z + (1 | site)
#   sublegal ~ status * year + depth_z + (1 | site)
# fitted with glmmTMB using family = tweedie(link = "log"), with predicted
# means and standard errors for each status-by-year combination.
#
# The same model is also fitted to legal sized males, legal sized females, and
# large males (>= large_male_mm) separately. The sexes differ in behaviour and in
# how the fishery takes them, so a change in legal catch can be driven by one sex
# alone; large males are the part of the catch a fishery removes first, so they
# are where an effect of protection should show up first. The combined legal
# model cannot show either. Note these are not independent tests - the two sexes
# together make up the legal catch already modelled above, and the large males
# are a subset of the legal males.
#
# Finally the model is fitted to all lobsters together (legal + sublegal), which
# is the total catch per pot. It is reported on its own rather than beside the
# size classes, because it is their sum rather than another comparison, and the
# legal and sublegal responses move in opposite directions between zones so the
# total partly cancels them out.
#
# IMPORTANT - the (1 | site) term is currently switched OFF.
# "Site" is the string (line) of pots. Neither survey year records a string
# identifier: 2025 pot numbers happen to run in order along each line, but the
# 2026 numbering is scrambled relative to position, so strings cannot be
# recovered reliably from the data alone. Rather than invent them by spatial
# clustering, the random effect is left out until the field string IDs are
# available. Set use_site <- TRUE below once the data carries a `site` column.
#
# Consequence: pots within a string are not independent, so the standard errors
# and p-values below are anti-conservative. The predicted means are affected far
# less than their uncertainty, so the plots are indicative but the significance
# tests should not be reported as final.

rm(list = ls())

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)

config <- yaml::read_yaml("r/abrolhos/05_lobster/00_config.yml")
name <- config$name
large_male_mm <- config$large_male_mm

tidy_dir   <- "data/abrolhos/tidy/lobster"
plot_dir   <- "plots/abrolhos/lobster"
output_dir <- "output/abrolhos/lobster"
dir.create(plot_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

use_site <- FALSE   # flip to TRUE when a `site` (string) column is available

status_colours <- c("Inside NPZ"  = "#7bbc63",
                    "Outside NPZ" = "#6daff4")

# Response column -> label used in every output table and plot. The order here
# sets the panel order.
class_labels <- c(legal        = "Legal",
                  sublegal     = "Sublegal",
                  legal_male   = "Legal male",
                  legal_female = "Legal female",
                  male_large   = paste0("Male ", large_male_mm, " mm+"),
                  total        = "All lobsters")

# Classes shown in the multi-panel figures. "All lobsters" is left out of these
# and plotted on its own, because it is the sum of the legal and sublegal panels
# rather than a further size class to read across.
panel_classes <- class_labels[setdiff(names(class_labels), "total")]

# Data -------------------------------------------------------------------------

catch <- read.csv(file.path(tidy_dir, paste0(name, "_lobster-catch-per-pot.csv")),
                  colClasses = c(pot_number = "character")) %>%
  dplyr::mutate(
    # "Outside NPZ" first so model coefficients read as the effect of protection
    status = factor(if_else(zone %in% "National Park Zone",
                            "Inside NPZ", "Outside NPZ"),
                    levels = c("Outside NPZ", "Inside NPZ")),
    year = factor(year),
    # One 2026 pot is logged at 0 m, which is not a possible seabed depth here.
    # Treated as missing so it drops from the model rather than distorting depth.
    depth_m = if_else(depth_m <= 0, NA_real_, depth_m)
  ) %>%
  dplyr::mutate(depth_z = as.numeric(scale(depth_m)))

n_dropped <- sum(is.na(catch$depth_z))
model_data <- catch %>% dplyr::filter(!is.na(depth_z))

message("Pots available: ", nrow(catch),
        " | dropped for missing/implausible depth: ", n_dropped,
        " | used in models: ", nrow(model_data))

if (use_site && !"site" %in% names(model_data)) {
  stop("use_site is TRUE but the data has no `site` column.")
}

# Fit --------------------------------------------------------------------------

site_term <- if (use_site) " + (1 | site)" else ""

fit_catch <- function(response) {
  full <- glmmTMB(
    as.formula(paste0(response, " ~ status * year + depth_z", site_term)),
    family = tweedie(link = "log"),
    data   = model_data
  )
  # Interaction dropped to give a likelihood ratio test of status-by-year
  additive <- glmmTMB(
    as.formula(paste0(response, " ~ status + year + depth_z", site_term)),
    family = tweedie(link = "log"),
    data   = model_data
  )
  list(full = full, additive = additive, lrt = anova(additive, full))
}

models <- purrr::set_names(names(class_labels)) %>%
  purrr::map(~ fit_catch(paste0("n_", .x)))

# Predicted means and SE for each status-by-year combination --------------------

predictions <- purrr::imap_dfr(models, function(m, class_key) {
  emmeans(m$full, ~ status * year, type = "response") %>%
    as.data.frame() %>%
    dplyr::rename(mean = response, se = SE) %>%
    dplyr::mutate(size_class = class_labels[[class_key]], .before = 1)
})

write.csv(predictions,
          file.path(output_dir, paste0(name, "_lobster-glm-predictions.csv")),
          row.names = FALSE)

# Tidy coefficient table, so the report can show results without refitting
coefficients <- purrr::imap_dfr(models, function(m, class_key) {
  co <- summary(m$full)$coefficients$cond
  tibble::tibble(
    size_class = class_labels[[class_key]],
    term       = rownames(co),
    estimate   = co[, "Estimate"],
    se         = co[, "Std. Error"],
    z          = co[, "z value"],
    p          = co[, "Pr(>|z|)"]
  )
})

write.csv(coefficients,
          file.path(output_dir, paste0(name, "_lobster-glm-coefficients.csv")),
          row.names = FALSE)

# Likelihood ratio test of the status x year interaction, for reporting
interaction_tests <- purrr::imap_dfr(models, function(m, class_key) {
  tibble::tibble(
    size_class = class_labels[[class_key]],
    chisq      = m$lrt$Chisq[2],
    df         = m$lrt$`Chi Df`[2],
    p          = m$lrt$`Pr(>Chisq)`[2]
  )
})

write.csv(interaction_tests,
          file.path(output_dir, paste0(name, "_lobster-glm-interaction-tests.csv")),
          row.names = FALSE)

# Written model summary --------------------------------------------------------

summary_path <- file.path(output_dir, paste0(name, "_lobster-glm-summary.txt"))
sink(summary_path)
cat("Tweedie GLM of western rock lobster catch per pot\n")
cat("Abrolhos Marine Park, Yamatji Shallow Bank, 2025 and 2026\n")
cat(strrep("=", 70), "\n\n")
cat("Formula: <response> ~ status * year + depth_z", site_term, "\n")
cat("Family:  tweedie(link = \"log\")\n")
cat("Responses:", paste(paste0("n_", names(class_labels)), collapse = ", "), "\n")
cat("Pots used:", nrow(model_data), "of", nrow(catch), "\n")
cat("\nNOTE: the legal male, legal female and large male models are subsets of\n")
cat("the legal model, not independent tests of it. The all lobsters model is the\n")
cat("legal and sublegal responses added together, so it is not independent either.\n")
if (!use_site) {
  cat("\nNOTE: the (1 | site) random effect specified by the supervisor is NOT\n")
  cat("included, because string identifiers are not recorded in either survey.\n")
  cat("Standard errors and p-values are therefore anti-conservative.\n")
}
cat("\nNOTE: status is spatially confounded with survey area. The National Park\n")
cat("Zone is a single contiguous block sampled on its own day in both years, so\n")
cat("'inside vs outside' is partly 'middle area vs north and south areas'.\n")

for (class_key in names(models)) {
  cat("\n\n", strrep("-", 70), "\n", toupper(class_labels[[class_key]]), "\n",
      strrep("-", 70), "\n\n", sep = "")
  print(summary(models[[class_key]]$full))
  cat("\nLikelihood ratio test for the status x year interaction:\n")
  print(models[[class_key]]$lrt)
  cat("\nPredicted means (response scale):\n")
  print(dplyr::filter(predictions, .data$size_class == class_labels[[class_key]]))
}

# Residual diagnostics ---------------------------------------------------------
# Inside the sink, so the test results end up in the summary file rather than
# only on the console

# plot.DHARMa sets its own two panel layout, so each model needs its own file
for (class_key in names(models)) {
  res <- simulateResiduals(models[[class_key]]$full, n = 1000, seed = 1)

  png(file.path(output_dir, paste0(name, "_lobster-glm-dharma_", class_key, ".png")),
      height = 5, width = 10, units = "in", res = 150)
  plot(res)
  mtext(paste("Tweedie GLM residual diagnostics -", class_labels[[class_key]]),
        outer = TRUE, line = -1.5, font = 2)
  dev.off()

  cat("\n\nDHARMa diagnostics -", class_labels[[class_key]], "\n")
  print(testDispersion(res, plot = FALSE))
  print(testZeroInflation(res, plot = FALSE))
}
sink()

# Control plot: predicted mean +/- SE through time ------------------------------

plot_data <- predictions %>%
  dplyr::mutate(year = as.numeric(as.character(year)),
                size_class = factor(size_class, levels = class_labels))

# Panels are ordered by the classes given, so the caller controls the reading
# order rather than it falling out of the order the models were fitted in
temporal_plot <- function(classes, ncol) {
  ggplot(plot_data %>%
           dplyr::filter(size_class %in% classes) %>%
           dplyr::mutate(size_class = factor(size_class, levels = classes)),
         aes(x = year, y = mean, fill = status, group = status)) +
    geom_line(aes(colour = status), position = position_dodge(0.3),
              linewidth = 0.4, alpha = 0.7) +
    geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                  width = 0.15, position = position_dodge(0.3)) +
    geom_point(shape = 21, size = 3, stroke = 0.3, colour = "black",
               position = position_dodge(0.3)) +
    facet_wrap(~size_class, ncol = ncol, scales = "free_y") +
    scale_fill_manual(values = status_colours, name = NULL) +
    scale_colour_manual(values = status_colours, guide = "none") +
    scale_x_continuous(breaks = unique(plot_data$year)) +
    coord_cartesian(ylim = c(0, NA)) +
    labs(x = "Year", y = "Predicted lobsters per pot (mean +/- SE)",
         caption = "Tweedie GLM, depth held at its mean") +
    theme_classic() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"))
}

temporal_plot(panel_classes, ncol = 3)

ggsave(file.path(plot_dir, paste0(name, "_lobster-glm-control-plot.png")),
       height = 6, width = 11, dpi = 300, bg = "white")

# All lobsters on its own, at the same scale conventions as the panels above so
# it can be read against them
temporal_plot(class_labels["total"], ncol = 1)

ggsave(file.path(plot_dir, paste0(name, "_lobster-glm-control-plot_total.png")),
       height = 4, width = 5.5, dpi = 300, bg = "white")

# The same plot for the three size classes that make up the size progression,
# in a single row so the panels read sublegal -> legal -> large males
size_progression <- class_labels[c("sublegal", "legal", "male_large")]

temporal_plot(size_progression, ncol = 3)

ggsave(file.path(plot_dir, paste0(name, "_lobster-glm-control-plot_size-progression.png")),
       height = 4, width = 11, dpi = 300, bg = "white")

# Bar plot of the same predictions ---------------------------------------------

ggplot(dplyr::filter(plot_data, size_class %in% panel_classes),
       aes(x = factor(year), y = mean, fill = status)) +
  geom_col(colour = "black", linewidth = 0.3, position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~size_class, nrow = 1, scales = "free_y") +
  scale_fill_manual(values = status_colours, name = NULL) +
  labs(x = "Year", y = "Predicted lobsters per pot (mean +/- SE)",
       caption = "Tweedie GLM, depth held at its mean") +
  theme_classic() +
  theme(legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(plot_dir, paste0(name, "_lobster-glm-barplot.png")),
       height = 4.5, width = 2.8 * length(panel_classes), dpi = 300, bg = "white")

# Box plot of the raw per-pot data ---------------------------------------------

raw_long <- model_data %>%
  tidyr::pivot_longer(paste0("n_", names(panel_classes)),
                      names_to = "size_class", values_to = "count") %>%
  dplyr::mutate(size_class = factor(panel_classes[sub("^n_", "", size_class)],
                                    levels = panel_classes))

ggplot(raw_long, aes(x = factor(year), y = count, fill = status)) +
  geom_boxplot(outlier.size = 0.6, linewidth = 0.3,
               position = position_dodge(0.8)) +
  # Shared y axis, so the four size classes can be compared against each other
  # rather than each being stretched to its own range
  facet_wrap(~size_class, nrow = 1) +
  scale_fill_manual(values = status_colours, name = NULL) +
  labs(x = "Year", y = "Lobsters per pot (observed)",
       caption = "Raw counts per pot, including pots that caught nothing") +
  theme_classic() +
  theme(legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(plot_dir, paste0(name, "_lobster-catch-boxplot.png")),
       height = 4.5, width = 2.8 * length(panel_classes), dpi = 300, bg = "white")

message("Model outputs written to ", output_dir)
message("Model plots written to ", plot_dir)
print(as.data.frame(predictions))
