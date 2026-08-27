###
# Project: NESP 4.20 - Marine Park Dashboard reporting
# Data:    Western rock lobster pots, Abrolhos (Yamatji Shallow Bank)
# Task:    Negative binomial GLMM of legal and sublegal catch, inside vs outside the NPZ
# Author:  Henry Evans
# Date:    August 2026
###

# Model follows the specification provided by the project supervisor:
#   legal    ~ status * year + depth_z + (1 | string)
#   sublegal ~ status * year + depth_z + (1 | string)
# fitted with glmmTMB, giving predicted means and standard errors for each
# status-by-year combination. The family was changed from the specified tweedie
# to negative binomial - see the note above model_family below.
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
# The (1 | string) term is now ON. A string is one line of pots. Strings could
# not be recovered from either survey export - 2025 pot numbers happen to run in
# order along each line but the 2026 numbering is scrambled relative to position
# - so they were assigned by hand and live in data/abrolhos/manual/lobster/.
#
# What the random effect is for: the same strings were fished in both years, but
# the individual pots were not dropped in exactly the same spots. String is the
# spatial pairing that makes the between-year comparison like for like. It is
# not a claim that a string is a biologically coherent unit - a long string
# spans several km, so its two ends are not interchangeable.
#
# What it does to the results: year is compared within strings, so its standard
# error goes DOWN. Status is compared almost entirely between strings, so its
# standard error goes UP - roughly doubling. The second is the honest number:
# 13 of the 14 strings sit inside a single zone, so the real replication for
# status is close to the number of strings rather than the number of pots.
# String 14 is the one exception - it is a single line of pots that crosses the
# boundary, so it is the only within-string zone contrast in the data.
# Dropping the term is not an option; without it the status estimate changes
# sign and AIC is ~80 worse.

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
output_dir <- "output/model-output/abrolhos/lobster"
dir.create(plot_dir,   recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

use_string <- TRUE   # set FALSE to refit without the random effect, for comparison

# The supervisor's specification was tweedie(link = "log"). Tweedie will not fit
# the two sparse responses once (1 | string) is in: legal male and large male
# both end with a non-positive-definite Hessian and no usable standard errors,
# and the large male string SD runs away to 59 on the log scale, predicting 5.8
# lobsters per pot inside the NPZ against a raw mean of 1.0. The cause is the
# extra Tweedie power parameter trading off against the random effect variance
# when the response is mostly zeros - large males are 55 to 88% zeros. Negative
# binomial has no such parameter, fits all six cleanly, and is the conventional
# choice for integer counts. Switch back here to compare.
model_family <- nbinom2(link = "log")
family_label <- "Negative binomial GLMM"

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
                  colClasses = c(pot_number = "character",
                                 string     = "character")) %>%
  dplyr::mutate(
    # "Outside NPZ" first so model coefficients read as the effect of protection
    status = factor(if_else(zone %in% "National Park Zone",
                            "Inside NPZ", "Outside NPZ"),
                    levels = c("Outside NPZ", "Inside NPZ")),
    year = factor(year),
    # Ordered numerically rather than as text, so string 2 does not sort after 10
    string = factor(string, levels = as.character(sort(as.numeric(unique(string))))),
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

if (use_string) {
  if (!"string" %in% names(model_data)) {
    stop("use_string is TRUE but the data has no `string` column. ",
         "Run 03_combine-data.R, which joins the lookup in.")
  }
  if (any(is.na(model_data$string))) {
    stop(sum(is.na(model_data$string)), " pots have no string assigned.")
  }
  message("Strings: ", nlevels(droplevels(model_data$string)),
          " | pots per string: ",
          paste(range(table(droplevels(model_data$string))), collapse = " to "))
}

# Fit --------------------------------------------------------------------------

string_term <- if (use_string) " + (1 | string)" else ""

fit_catch <- function(response) {
  full <- glmmTMB(
    as.formula(paste0(response, " ~ status * year + depth_z", string_term)),
    family = model_family,
    data   = model_data
  )
  # Interaction dropped to give a likelihood ratio test of status-by-year
  additive <- glmmTMB(
    as.formula(paste0(response, " ~ status + year + depth_z", string_term)),
    family = model_family,
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
cat(family_label, "of western rock lobster catch per pot\n")
cat("Abrolhos Marine Park, Yamatji Shallow Bank, 2025 and 2026\n")
cat(strrep("=", 70), "\n\n")
cat("Formula: <response> ~ status * year + depth_z", string_term, "\n")
cat("Family:  nbinom2(link = \"log\")   [specified as tweedie; see script header]\n")
cat("Responses:", paste(paste0("n_", names(class_labels)), collapse = ", "), "\n")
cat("Pots used:", nrow(model_data), "of", nrow(catch), "\n")
cat("\nNOTE: the legal male, legal female and large male models are subsets of\n")
cat("the legal model, not independent tests of it. The all lobsters model is the\n")
cat("legal and sublegal responses added together, so it is not independent either.\n")
if (use_string) {
  cat("\nStrings:", nlevels(droplevels(model_data$string)),
      "- the random effect is the spatial pairing between years, not a\n")
  cat("claim that a string is a biologically coherent unit. Year is compared\n")
  cat("within strings; status is compared almost entirely between them, so the\n")
  cat("status standard errors are roughly double what a model without the term\n")
  cat("would report. All but one string sits inside a single zone - string 14\n")
  cat("crosses the boundary and is the only within-string zone contrast.\n")
} else {
  cat("\nNOTE: the (1 | string) random effect specified by the supervisor is NOT\n")
  cat("included. Pots within a string are not independent, so standard errors\n")
  cat("and p-values are anti-conservative.\n")
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
  mtext(paste(family_label, "residual diagnostics -", class_labels[[class_key]]),
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
         caption = paste(family_label, "with (1 | string), depth held at its mean")) +
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
       caption = paste(family_label, "with (1 | string), depth held at its mean")) +
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
