controlplot_fish <- function(data, metric, amp_abbrv, state_abbrv = NULL,
                             metric_label = NULL,
                             survey_years = c(2022, 2025),
                             x_start = 2015,
                             zoning_year = 2018,
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

  # ---- Build the full palette, keyed by zone label ----
  # State categories are only added when a state park abbreviation is supplied,
  # so paste() never produces a stray leading space from a NULL/empty value.
  all_levels <- c(
    paste(amp_abbrv, "HPZ"),
    paste(amp_abbrv, "NPZ (IUCN II)"),
    paste(amp_abbrv, "other zones")
  )
  all_fills  <- c("#fff8a3", "#7bbc63", "#b9e6fb")
  all_shapes <- c(21, 21, 21)

  if (!is.null(state_abbrv) && nzchar(state_abbrv)) {
    all_levels <- c(all_levels,
                    paste(state_abbrv, "SZ (IUCN II)"),
                    paste(state_abbrv, "other zones"))
    all_fills  <- c(all_fills,  "#bfd054", "#bddde1")
    all_shapes <- c(all_shapes, 25, 25)
  }

  names(all_fills)  <- all_levels
  names(all_shapes) <- all_levels

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(year),
      depth_class = factor(depth_class, levels = depth_levels)
    )

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", metric_label)
    return(NULL)
  }

  # ---- Keep only zones that actually appear in the data ----
  present <- all_levels[all_levels %in% unique(plot_dat$zone_new)]

  if (length(present) == 0) {
    message("No recognised zone_new values to plot for ", metric_label)
    return(NULL)
  }

  plot_dat$zone_new <- factor(plot_dat$zone_new, levels = present)

  fill_vals  <- all_fills[present]
  shape_vals <- all_shapes[present]

  # ---- Shared x-axis settings ----
  # Axis starts at x_start so the zoning reference line stays in view, even
  # though sampling did not begin until the first survey year.
  x_breaks <- sort(unique(survey_years))
  x_limits <- c(x_start, max(x_breaks) + 1)

  if (metric == "cti") {

    sst <- readRDS(
      paste0("data/", park, "/spatial/oceanography/",
             name, "_SST_time-series-recent.rds")
    ) %>%
      dplyr::mutate(year = as.numeric(year)) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(
        sst = mean(sst, na.rm = TRUE),
        sd  = mean(sd,  na.rm = TRUE),
        .groups = "drop"
      )

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
      geom_vline(
        xintercept = zoning_year,
        linetype = "dashed",
        color = "black",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = x_breaks) +
      coord_cartesian(xlim = x_limits) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = TRUE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = TRUE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )

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
      geom_vline(
        xintercept = zoning_year,
        linetype = "dashed",
        color = "black",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = x_breaks) +
      coord_cartesian(xlim = x_limits, ylim = c(0, NA)) +
      scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = TRUE) +
      scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = TRUE) +
      labs(
        x = "Year",
        y = metric_label,
        title = NULL
      ) +
      theme(
        legend.position = "right",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )
  }

  return(p)
}
