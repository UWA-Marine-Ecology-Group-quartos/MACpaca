dominantbenthos_plot_multi <- function(dat_list, prediction_limits, habitat_lookup) {

  yrs <- names(dat_list)

  if (is.null(yrs) || any(yrs == "")) {
    stop("dat_list must be a named list")
  }

  # Gradient high colours for each habitat
  grad_high <- c(
    "Sand"                  = "wheat",
    "Macroalgae"            = "darkorange4",
    "Seagrass"              = "forestgreen",
    "Rock"                  = "grey40",
    "Sessile invertebrates" = "deeppink3"
  )

  # Legend label (line break for long names)
  legend_names <- c(
    "Sand"                  = "Sand",
    "Macroalgae"            = "Macroalgae",
    "Seagrass"              = "Seagrass",
    "Rock"                  = "Rock",
    "Sessile invertebrates" = "Sessile\ninvertebrates"
  )

  # Canonical rendering order — filter to modelled taxa only
  hab_order <- c("Sand", "Rock", "Macroalgae", "Seagrass", "Sessile invertebrates")
  modelled  <- hab_order[hab_order %in% names(habitat_lookup)]

  multi_year <- length(dat_list) > 1

  # ------------------------------------------------------------
  # Extract dominant benthos data + combined SE rasters by year
  # ------------------------------------------------------------
  dom_plot_list <- vector("list", length(dat_list))
  se_list       <- vector("list", length(dat_list))

  for (i in seq_along(dat_list)) {
    dat <- dat_list[[i]]

    pred_class <- as.data.frame(dat, xy = TRUE) %>%
      dplyr::mutate(year = yrs[i])

    dom_plot_list[[i]] <- normalise_se(data = pred_class)
    se_list[[i]]       <- dat[["mean_se"]]
  }

  # Shared SE limits across years
  se_vals   <- unlist(lapply(se_list, terra::values))
  se_limits <- range(se_vals, na.rm = TRUE)

  # ------------------------------------------------------------
  # Theme variants
  # ------------------------------------------------------------
  theme_left <- theme(
    axis.title        = element_blank(),
    axis.text         = element_text(size = 9),
    axis.ticks        = element_line(linewidth = 0.2),
    panel.grid.major  = element_line(linewidth = 0.2, colour = "grey85"),
    panel.grid.minor  = element_blank(),
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width  = unit(0.45, "cm"),
    plot.margin       = margin(2, 2, 2, 2, unit = "mm")
  )

  theme_inner <- theme_left +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank()
    )

  theme_no_x <- theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

  ngari_colours <- wasanc %>%
    st_drop_geometry() %>%
    distinct(zone, colour) %>%
    arrange(zone) %>%
    pull(colour)

  build_base <- function(col, show_x = TRUE, show_park_legend = TRUE) {

    # col 1 = predicted probability (keeps the y axis), col 2 = SE (drops it)
    y_theme <- if (col == 1) theme_left else theme_inner
    x_theme <- if (show_x) theme() else theme_no_x

    list(
      geom_contour(
        data = bathy,
        aes(x = x, y = y, z = Depth),
        colour    = "black",
        breaks    = c(-30, -70, -200),
        linewidth = 0.1
      ),
      geom_sf(data = ausc, fill = "seashell2", colour = "black", linewidth = 0.2),
      geom_sf(
        data        = wasanc,
        aes(colour  = zone),
        fill        = NA,
        linewidth   = 0.8,
        show.legend = show_park_legend
      ),
      scale_colour_manual(
        name   = "State Marine Park",
        guide  = "legend",
        values = with(wasanc, setNames(colour, zone))
      ),
      guides(colour = guide_legend(
        order        = 2,
        ncol         = 1,
        title.position = "top",
        override.aes = list(colour = ngari_colours, fill = NA, linewidth = 1),
        title.theme  = element_text(size = 9, face = "bold")
      )),
      ggnewscale::new_scale_color(),
      geom_sf(
        data        = marine_parks_amp,
        aes(colour  = zone),
        fill        = NA,
        show.legend = show_park_legend,
        linewidth   = 0.6
      ),
      geom_sf(data = cwatr, colour = "firebrick", linewidth = 0.6),
      scale_colour_manual(
        name   = "Australian Marine Parks",
        guide  = "legend",
        values = with(marine_parks_amp, setNames(colour, zone))
      ),
      guides(colour = guide_legend(
        order        = 1,
        ncol         = 2,
        title.position = "top",
        override.aes = list(fill = NA, linewidth = 1),
        title.theme  = element_text(size = 9, face = "bold")
      )),
      scale_x_continuous(breaks = scales::breaks_width(0.3)),
      scale_y_continuous(breaks = scales::breaks_width(0.04)),
      coord_sf(
        xlim   = c(prediction_limits[1], prediction_limits[2]),
        ylim   = c(prediction_limits[3], prediction_limits[4]),
        crs    = 4326,
        expand = FALSE
      ),
      labs(x = NULL, y = NULL, colour = NULL),
      theme_minimal(),
      y_theme,
      x_theme,
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11)
      )
    )
  }

  # ------------------------------------------------------------
  # Top row: dominant benthos panels
  # ------------------------------------------------------------
  p_dom <- lapply(seq_along(yrs), function(i) {

    pred_plot <- dom_plot_list[[i]]

    p <- ggplot()

    for (j in seq_along(modelled)) {
      hab       <- modelled[j]
      stub      <- habitat_lookup[[hab]]
      fit_col   <- paste0("p_", stub, ".fit")
      alpha_col <- paste0("p_", stub, ".alpha")

      if (j > 1) p <- p + new_scale_fill() + new_scale("alpha")

      p <- p +
        geom_tile(data = pred_plot,
                  aes(x = x, y = y,
                      fill  = .data[[alpha_col]],
                      alpha = .data[[fit_col]])) +
        scale_alpha_continuous(range = c(0, 1), guide = "none", name = hab) +
        scale_fill_gradient(
          low      = "white",
          high     = grad_high[[hab]],
          name     = legend_names[[hab]],
          na.value = "transparent",
          breaks   = c(0, 0.5, 1),
          labels   = c("0", "0.5", "1"),
          guide    = guide_colorbar(title.hjust = 0, title.vjust = 0.5, label.hjust = 0)

        )
    }

    # Column header on the first row only - years are labelled down the side
    if (i == 1) p <- p + ggtitle("Predicted Habitat Probability")

    p + build_base(col = 1, show_x = i == length(yrs), show_park_legend = FALSE)
  })

  # ------------------------------------------------------------
  # Bottom row: combined SE panels
  # ------------------------------------------------------------
  p_se <- lapply(seq_along(yrs), function(i) {
    ggplot() +
      geom_spatraster(data = se_list[[i]], maxcell = Inf) +
      scale_fill_viridis_c(
        option   = "A",
        na.value = "transparent",
        name     = "Normalised\ncombined SE",
        limits   = se_limits,
        oob      = scales::squish

      ) +
      ggtitle(if (i == 1) "Standard Error" else NULL) +
      build_base(col = 2, show_x = i == length(yrs), show_park_legend = FALSE)
  })
  # ------------------------------------------------------------
  # Row labels
  # ------------------------------------------------------------
  row_label_plot <- function(label) {
    ggplot() +
      theme_void() +
      annotate(
        "text", x = 0.5, y = 0.5,
        label = label, angle = 90,
        fontface = "bold", size = 3.5
      )
  }

  # ------------------------------------------------------------
  # Legend blocks
  # ------------------------------------------------------------
  # The habitat colourbars and the SE colourbar are drawn from stand-alone
  # legend-only plots so they can be arranged as two columns: the habitats
  # across two rows on the left, the SE bar on its own to the right.
  legend_theme <- theme_void() +
    theme(
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.box           = "horizontal",
      legend.box.just      = "centre",
      legend.justification = "centre",
      legend.title         = element_text(size = 9, margin = margin(b = 2, r = 3)),
      legend.text          = element_text(size = 8),
      legend.key.height    = unit(0.3, "cm"),
      legend.key.width     = unit(0.42, "cm"),
      legend.spacing.x     = unit(6, "mm"),
      legend.spacing.y     = unit(0.5, "mm"),
      legend.spacing       = unit(0.5, "mm"),
      legend.box.margin    = margin(0, 0, 0, 0)
    )

  # Two-row stand-in data so every gradient spans the full 0-1 range without
  # re-rendering the prediction rasters.
  legend_dat <- data.frame(x = c(1, 2), y = c(1, 1))

  for (hab in modelled) {
    stub <- habitat_lookup[[hab]]
    legend_dat[[paste0("p_", stub, ".fit")]]   <- c(0, 1)
    legend_dat[[paste0("p_", stub, ".alpha")]] <- c(0, 1)
  }

  hab_legend_plot <- function(habs) {

    p <- ggplot()

    for (j in seq_along(habs)) {
      hab       <- habs[j]
      stub      <- habitat_lookup[[hab]]
      fit_col   <- paste0("p_", stub, ".fit")
      alpha_col <- paste0("p_", stub, ".alpha")

      if (j > 1) p <- p + new_scale_fill() + new_scale("alpha")

      p <- p +
        geom_tile(data = legend_dat,
                  aes(x = x, y = y,
                      fill  = .data[[alpha_col]],
                      alpha = .data[[fit_col]])) +
        scale_alpha_continuous(range = c(0, 1), guide = "none", name = hab) +
        scale_fill_gradient(
          low      = "white",
          high     = grad_high[[hab]],
          name     = legend_names[[hab]],
          na.value = "transparent",
          breaks   = c(0, 0.5, 1),
          labels   = c("0", "0.5", "1"),
          guide    = guide_colorbar(title.position = "top", title.hjust = 0,
                                    title.vjust = 0.5, label.hjust = 0)
        )
    }

    p + legend_theme +
      theme(
        legend.justification = "centre",
        legend.box.just      = "bottom"
      )
  }

  se_legend_plot <- function() {
    ggplot(data.frame(x = c(1, 2), y = c(1, 1), se = se_limits)) +
      geom_tile(aes(x = x, y = y, fill = se)) +
      scale_fill_viridis_c(
        option   = "A",
        na.value = "transparent",
        name     = "Normalised\ncombined SE",
        limits   = se_limits,
        oob      = scales::squish,
        guide    = guide_colorbar(title.position = "top", title.hjust = 0.5,
                                  title.vjust = 0.5, label.hjust = 0)
      ) +
      legend_theme +
      theme(legend.key.width = unit(0.45, "cm"))
  }

  extract_legend <- function(p) {
    cowplot::get_plot_component(p, "guide-box-bottom")
  }

  # plot_grid() centres whatever it is handed, so the extracted legend gtables
  # are wrapped in a viewport that pins them to the left and bottom of the cell.
  justify_grob <- function(g, hjust = "left", vjust = "bottom") {
    x <- switch(hjust, left = 0, right = 1, 0.5)
    y <- switch(vjust, bottom = 0, top = 1, 0.5)

    grid::editGrob(
      g,
      vp = grid::viewport(
        x      = x,
        y      = y,
        just   = c(hjust, vjust),
        width  = grid::grobWidth(g),
        height = grid::grobHeight(g)
      )
    )
  }

  # Habitats split across two rows, SE alongside as the second column
  split_at <- ceiling(length(modelled) / 2)
  hab_rows <- list(modelled[seq_len(split_at)])

  if (length(modelled) > split_at) {
    hab_rows <- c(hab_rows, list(modelled[(split_at + 1):length(modelled)]))
  }

  hab_legends <- lapply(hab_rows, function(habs) {
    justify_grob(extract_legend(hab_legend_plot(habs)),
                 hjust = "centre", vjust = "bottom")
  })

  hab_block <- if (length(hab_legends) == 1) {
    hab_legends[[1]]
  } else {
    cowplot::plot_grid(plotlist = hab_legends, ncol = 1)
  }

  legend_top <- cowplot::plot_grid(
    hab_block,
    justify_grob(extract_legend(se_legend_plot()),
                 hjust = "centre", vjust = "centre"),
    ncol       = 2,
    rel_widths = c(1, 0.35)
  )

  # ------------------------------------------------------------
  # Combine
  # ------------------------------------------------------------
  # One row per year: predicted probability on the left, SE on the right.
  # Panel legends are switched off - the block assembled above is placed
  # underneath the grid instead.
  year_rows <- lapply(seq_along(yrs), function(i) {
    if (multi_year) {
      row_label_plot(yrs[i]) + p_dom[[i]] + p_se[[i]] +
        plot_layout(widths = c(0.06, 1, 1))
    } else {
      p_dom[[i]] + p_se[[i]] +
        plot_layout(widths = c(1, 1))
    }
  })

  p_out <- wrap_plots(year_rows, ncol = 1)

  p_out <- p_out &
    theme(
      legend.position = "none",
      panel.spacing   = unit(0.5, "mm"),
      plot.margin     = margin(2, 2, 2, 2, unit = "mm")
    )

  p_out <- cowplot::plot_grid(
    p_out,
    legend_top,
    NULL,
    marine_park_legend(),
    ncol        = 1,
    rel_heights = c(0.98, 0.24, 0.06, 0.145)
  )

  return(p_out)
}
