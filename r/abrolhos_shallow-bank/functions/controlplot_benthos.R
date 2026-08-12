controlplot_benthos <- function(data, taxa, amp_abbrv, state_abbrv = NULL,
                                taxa_label = NULL,
                                reference_year = 2018,
                                x_limits = c(2016, 2025),
                                drop_empty_depths = TRUE,
                                depth_levels = c("Shallow (0 - 30 m)",
                                                 "Mesophotic (30 - 70 m)",
                                                 "Rariphotic (70 - 200 m)")) {

  mean_col <- taxa
  se_col   <- paste0(taxa, "_se")

  if (is.null(taxa_label)) {
    taxa_label <- dplyr::case_when(
      taxa == "seagrass"   ~ "Seagrass",
      taxa == "macroalgae" ~ "Macroalgae",
      taxa == "rock"       ~ "Rock",
      taxa == "sand"       ~ "Sand",
      taxa == "inverts"    ~ "Sessile invertebrates",
      TRUE ~ stringr::str_to_title(taxa)
    )
  }

  req_cols <- c("year", "zone_new", "depth_class", mean_col, se_col)
  if (!all(req_cols %in% names(data))) {
    stop("Data is missing one or more required columns: ",
         paste(setdiff(req_cols, names(data)), collapse = ", "))
  }

  # Zone levels. state_abbrv is NULL for Commonwealth-only parks, in which case
  # the state entries are dropped rather than appearing as empty legend keys.
  # HPZ is not included - Abrolhos has no Habitat Protection Zone
  amp_levels <- c(paste(amp_abbrv, "NPZ (IUCN II)"),
                  paste(amp_abbrv, "other zones"))

  state_levels <- if (is.null(state_abbrv)) character(0) else {
    c(paste(state_abbrv, "SZ (IUCN II)"),
      paste(state_abbrv, "other zones"))
  }

  zone_levels <- c(amp_levels, state_levels)

  fill_vals  <- setNames(c("#7bbc63", "#b9e6fb",
                           "#bfd054", "#bddde1")[seq_along(zone_levels)],
                         zone_levels)
  shape_vals <- setNames(c(21, 21, 25, 25)[seq_along(zone_levels)],
                         zone_levels)

  # year may arrive as a factor or character (config$years is quoted in YAML),
  # but the x axis is continuous - coerce via as.character() so factor levels
  # are not silently converted to their integer codes
  plot_dat <- data %>%
    dplyr::filter(!is.na(.data[[mean_col]])) %>%
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      depth_class = factor(depth_class, levels = depth_levels),
      zone_new = factor(zone_new, levels = zone_levels)
    ) %>%
    dplyr::filter(!is.na(year))

  if (nrow(plot_dat) == 0) {
    message("No data available to plot for ", taxa_label)
    return(NULL)
  }

  # Depth classes with no data would otherwise render as blank facets
  if (drop_empty_depths) {
    plot_dat <- plot_dat %>% dplyr::mutate(depth_class = droplevels(depth_class))
  }

  survey_years <- sort(unique(plot_dat$year))

  p <- ggplot(
    data = plot_dat,
    aes(x = year, y = .data[[mean_col]], fill = zone_new, shape = zone_new)
  ) +
    geom_errorbar(
      aes(
        ymin = pmax(.data[[mean_col]] - .data[[se_col]], 0),
        ymax = .data[[mean_col]] + .data[[se_col]]
      ),
      width = 0.4,
      position = position_dodge(width = 0.6)
    ) +
    geom_point(
      size = 3,
      position = position_dodge(width = 0.6),
      stroke = 0.2,
      colour = "black",
      alpha = 0.8
    ) +
    geom_vline(
      xintercept = reference_year,
      linetype = "dashed",
      colour = "black",
      linewidth = 0.5,
      alpha = 0.5
    ) +
    facet_wrap(~depth_class, ncol = 1, scales = "free_y") +
    theme_classic() +
    scale_x_continuous(breaks = survey_years) +
    coord_cartesian(xlim = x_limits, ylim = c(0, NA)) +
    scale_fill_manual(values = fill_vals, name = "Marine Parks", drop = FALSE) +
    scale_shape_manual(values = shape_vals, name = "Marine Parks", drop = FALSE) +
    labs(x = "Year", y = "Mean predicted probability") +
    theme(
      legend.position = "right",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )

  return(p)
}
