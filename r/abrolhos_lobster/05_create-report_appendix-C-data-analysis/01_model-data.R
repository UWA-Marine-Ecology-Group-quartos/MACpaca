###
# Project: NESP 4.21 - Australian Marine Parks Natural Values Reporting
# Data:    Western rock lobster pot synthesis, Abrolhos (Yamatji Shallow Bank)
# Task:    Test status and year for each lobster metric
# Author:
# Date:    October 2026
###

# Same model as r/abrolhos/05_lobster/06_model_catch-rates.R:
#   count ~ status * year + depth_z + (1 | string), negative binomial
# Run 02_create-metrics.R in Appendix A first

rm(list = ls())

script_dir <- dirname(
  rstudioapi::getActiveDocumentContext()$path
)

config <- yaml::read_yaml(
  file.path(script_dir, "00_config.yml")
)

name <- config$name
park <- config$park
maturity_mm <- config$maturity_mm
large_mm    <- config$large_mm

library(tidyverse)
library(glmmTMB)
library(emmeans)

out_dir  <- paste0("output/model-output/", park)
plot_dir <- paste0("plots/", park)
dir.create(out_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

status_colours <- c("No-Take" = "#7bbc63",
                    "Fished"  = "#b9e6fb")

metric_labels <- c(all           = "All lobster",
                   sub_maturity  = paste0("Sub-maturity (<", maturity_mm, " mm CL)"),
                   mature_female = paste0("Female ≥", maturity_mm, " mm CL"),
                   mature_male   = paste0("Male ≥", maturity_mm, " mm CL"),
                   large_female  = paste0("Female ≥", large_mm, " mm CL"),
                   large_male    = paste0("Male ≥", large_mm, " mm CL"))

# Data ----
# Fished first so coefficients read as the effect of protection
dat <- readRDS(paste0("data/", park, "/tidy/", name, "_lobster-metrics.rds")) %>%
  dplyr::mutate(status  = factor(status, levels = c("Fished", "No-Take")),
                year    = factor(year),
                string  = factor(string),
                # One pot is logged at 0 m depth, treated as missing
                depth_m = if_else(depth_m <= 0, NA_real_, depth_m)) %>%
  dplyr::filter(!is.na(depth_m), !is.na(string)) %>%
  dplyr::mutate(depth_z = as.numeric(scale(depth_m))) %>%
  glimpse()

# Fit models ----
fit_model <- function(resp) {
  d <- dplyr::filter(dat, response %in% resp)
  full <- glmmTMB(count ~ status * year + depth_z + (1 | string),
                  family = nbinom2(link = "log"), data = d)
  # Interaction dropped for a likelihood ratio test of status x year
  additive <- glmmTMB(count ~ status + year + depth_z + (1 | string),
                      family = nbinom2(link = "log"), data = d)
  list(full = full, lrt = anova(additive, full), n = nrow(d))
}

models <- purrr::map(purrr::set_names(names(metric_labels)), fit_model)

# Interaction tests ----
interaction_tests <- purrr::imap_dfr(models, function(m, resp) {
  tibble::tibble(response = metric_labels[[resp]],
                 n_pots   = m$n,
                 chisq    = m$lrt$Chisq[2],
                 df       = m$lrt$`Chi Df`[2],
                 p        = m$lrt$`Pr(>Chisq)`[2])
})

write.csv(interaction_tests, paste0(out_dir, "/", name, "_interaction-tests.csv"),
          row.names = FALSE)

# Coefficients ----
coefficients <- purrr::imap_dfr(models, function(m, resp) {
  co <- summary(m$full)$coefficients$cond
  tibble::tibble(response = metric_labels[[resp]],
                 term     = rownames(co),
                 estimate = co[, "Estimate"],
                 se       = co[, "Std. Error"],
                 z        = co[, "z value"],
                 p        = co[, "Pr(>|z|)"])
})

write.csv(coefficients, paste0(out_dir, "/", name, "_coefficients.csv"),
          row.names = FALSE)

# Status test within each year ----
# Ratio of No-Take to Fished
status_tests <- purrr::imap_dfr(models, function(m, resp) {
  emmeans(m$full, ~ status | year, type = "response") %>%
    pairs(reverse = TRUE) %>%
    as.data.frame() %>%
    dplyr::mutate(response = metric_labels[[resp]], .before = 1)
})

write.csv(status_tests, paste0(out_dir, "/", name, "_status-tests.csv"),
          row.names = FALSE)

# Predictions ----
predictions <- purrr::imap_dfr(models, function(m, resp) {
  emmeans(m$full, ~ status * year, type = "response") %>%
    as.data.frame() %>%
    dplyr::rename(mean = response, se = SE) %>%
    dplyr::mutate(response = metric_labels[[resp]], .before = 1)
})

write.csv(predictions, paste0(out_dir, "/", name, "_predictions.csv"),
          row.names = FALSE)

# Predicted plot ----
p_pred <- predictions %>%
  dplyr::mutate(response = factor(response, levels = metric_labels),
                status   = factor(status, levels = names(status_colours))) %>%
  ggplot(aes(x = year, y = mean, fill = status)) +
  geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                width = 0.3, position = position_dodge(0.5)) +
  geom_point(shape = 21, size = 3, stroke = 0.2, colour = "black", alpha = 0.8,
             position = position_dodge(0.5)) +
  scale_fill_manual(values = status_colours, name = "Status") +
  coord_cartesian(ylim = c(0, NA)) +
  facet_wrap(~response, ncol = 2, scales = "free_y") +
  labs(x = "Year", y = "Predicted lobster per pot (mean ± SE)") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text       = element_text(face = "bold"))
p_pred

saveRDS(p_pred, paste0(plot_dir, "/", name, "_predicted-metric-plot.rds"))

print(interaction_tests)
print(status_tests)
