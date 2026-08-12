controlplot_fish <- function(data, metric, amp_abbrv,
                             metric_label = NULL,
                             survey_years = NULL,
                             x_range = c(2015, 2025),
                             zoning_year = 2018,
                             sst_file = NULL,
                             depth_levels = c("Shallow (0 - 30 m)",
                                              "Mesophotic (30 - 70 m)",
                                              "Rariphotic (70 - 200 m)")) {

  mean_col <- metric
  se_col   <- paste0(metric, "_se")

  if (is.null(metric_label)) {
    metric_label <- dplyr::case_when(
      metric == "richness"  ~ "Species richness (per BRUV)",
      metric == "cti"       ~ "Community Thermal Index (\u00B0C)",
      metric == "b20"       ~ "Large reef fish index* (biomass g per BRUV)",
      metric == "abundance" ~ "Total abundance (per BRUV)",
      TRUE ~ stringr::str_to_title(metric)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  zone_levels <- c(
    paste(amp_abbrv, "NPZ (IUCN II)"),
    paste(amp_abbrv, "other zones")
  )

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(year),
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(zone_new, levels = zone_levels)
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", metric_label)
    return(NULL)
  }

  # x axis breaks default to the survey years present in the data
  if (is.null(survey_years)) {
    survey_years <- sort(unique(plot_dat$year))
  }
  survey_years <- as.numeric(survey_years)

  fill_vals  <- setNames(c("#7bbc63", "#b9e6fb"), zone_levels)
  shape_vals <- setNames(c(21, 21), zone_levels)

  # layers shared by both branches
  common_layers <- list(
    geom_vline(
      xintercept = zoning_year,
      linetype = "dotted",
      color = "black",
      linewidth = 0.5,
      alpha = 0.5
    ),
    facet_wrap(~depth_class, ncol = 1, scales = "free_y"),
    theme_classic(),
    scale_x_continuous(breaks = survey_years),
    scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = FALSE),
    scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = FALSE),
    labs(x = "Year", y = metric_label, title = NULL),
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
  )

  if (metric == "cti") {

    if (is.null(sst_file)) {
      sst_file <- paste0("data/", park, "/spatial/oceanography/",
                         name, "_SST_time-series-recent.rds")
    }

    sst <- readRDS(sst_file) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        sst = mean(sst, na.rm = TRUE),
        sd  = mean(sd,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(year >= x_range[1], year <= x_range[2])

    p <- ggplot() +
      geom_line(
        data = sst,
        aes(x = year, y = sst)
      ) +
      geom_ribbon(
        data = sst,
        aes(x = year, ymin = sst - sd, ymax = sst + sd),
        alpha = 0.2
      ) +
      geom_errorbar(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          ymin = .data[[mean_col]] - .data[[se_col]],
          ymax = .data[[mean_col]] + .data[[se_col]],
          fill = zone_new,
          shape = zone_new
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        data = plot_dat,
        aes(
          x = year,
          y = .data[[mean_col]],
          fill = zone_new,
          shape = zone_new
        ),
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      common_layers +
      coord_cartesian(xlim = x_range)

  } else {

    p <- ggplot(
      data = plot_dat,
      aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
    ) +
      geom_errorbar(
        aes(
          ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
          ymax = .data[[mean_col]] + .data[[se_col]]
        ),
        width = 0.8,
        position = position_dodge(width = 0.6)
      ) +
      geom_point(
        size = 3,
        position = position_dodge(width = 0.6),
        stroke = 0.2,
        color = "black",
        alpha = 0.8
      ) +
      common_layers +
      coord_cartesian(xlim = x_range, ylim = c(0, NA))
  }

  return(p)
}
