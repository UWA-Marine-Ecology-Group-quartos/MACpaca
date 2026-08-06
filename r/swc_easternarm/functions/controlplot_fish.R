controlplot_fish <- function(data, metric, amp_abbrv, state_abbrv = NULL,
                             metric_label = NULL,
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

  # Commonwealth zones only, unless a state park abbreviation is supplied
  zone_levels <- c(
    paste(amp_abbrv, "HPZ"),
    paste(amp_abbrv, "NPZ (IUCN II)"),
    paste(amp_abbrv, "other zones")
  )
  zone_fills  <- c("#fff8a3", "#7bbc63", "#b9e6fb")
  zone_shapes <- c(21, 21, 21)

  if (!is.null(state_abbrv)) {
    zone_levels <- c(zone_levels,
                     paste(state_abbrv, "SZ (IUCN II)"),
                     paste(state_abbrv, "other zones"))
    zone_fills  <- c(zone_fills, "#bfd054", "#bddde1")
    zone_shapes <- c(zone_shapes, 25, 25)
  }

  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(year),
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(zone_new, levels = zone_levels)
    )

  # Warn if any zone label was not matched, rather than plotting it as NA
  if (any(is.na(plot_dat$zone_new))) {
    message("Dropping ", sum(is.na(plot_dat$zone_new)),
            " rows with zone labels not in: ", paste(zone_levels, collapse = ", "))
    plot_dat <- dplyr::filter(plot_dat, !is.na(zone_new))
  }

  # Only keep the zones actually present, so the legend matches the plot
  plot_dat <- dplyr::mutate(plot_dat, zone_new = droplevels(zone_new))

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", metric_label)
    return(NULL)
  }

  fill_vals  <- setNames(zone_fills,  zone_levels)
  shape_vals <- setNames(zone_shapes, zone_levels)

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
        xintercept = 2018, # TODO set to your zoning/reference year
        linetype = "dashed",
        color = "black",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2014, 2024)) + # TODO set to your survey years
      coord_cartesian(xlim = c(2013, 2025)) + # TODO set to your survey years
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
        xintercept = 2018, # TODO set to your zoning/reference year
        linetype = "dashed",
        color = "black",
        linewidth = 0.5,
        alpha = 0.5
      ) +
      facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
      theme_classic() +
      scale_x_continuous(breaks = c(2022, 2024)) + # TODO set to your survey years
      coord_cartesian(xlim = c(2016, 2025), ylim = c(0, NA)) + # TODO set to your survey years
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
