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

  theme_top <- theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

  ngari_colours <- wasanc %>%
    st_drop_geometry() %>%
    distinct(zone, colour) %>%
    arrange(zone) %>%
    pull(colour)

  build_base <- function(i, show_x = TRUE, show_park_legend = TRUE) {

    y_theme <- if (i == 1) theme_left else theme_inner
    x_theme <- if (show_x) theme() else theme_top

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
      # Axis labels every 0.4 degrees
      scale_x_continuous(breaks = scales::breaks_width(0.4)),
      scale_y_continuous(breaks = scales::breaks_width(0.4)),
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
          guide    = guide_colorbar(
            direction      = "horizontal",
            title.position = "top",
            title.hjust    = 0.5,
            label.hjust    = 0.5,
            barwidth       = unit(1.6, "cm"),
            barheight      = unit(0.3, "cm")
          )
        )
    }

    # Only add year title when there are multiple years
    if (multi_year) p <- p + ggtitle(yrs[i])

    p + build_base(i, show_x = FALSE, show_park_legend = FALSE)
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
      build_base(i, show_x = TRUE, show_park_legend = FALSE)
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

  dom_label <- row_label_plot("Predicted Habitat Probability")
  se_label  <- row_label_plot("Standard Error")

  # ------------------------------------------------------------
  # Combine
  # ------------------------------------------------------------
  dom_row <- dom_label + wrap_plots(p_dom, nrow = 1, guides = "collect") +
    plot_layout(widths = c(0.06, 1))

  se_row <- se_label + wrap_plots(p_se, nrow = 1, guides = "collect") +
    plot_layout(widths = c(0.06, 1))

  p_out <- (dom_row / se_row) +
    plot_layout(heights = c(1, 1), guides = "collect") &
    theme(
      legend.position      = "bottom",
      legend.direction     = "horizontal",
      legend.box           = "horizontal",
      legend.box.just      = "centre",
      legend.justification = "centre",
      legend.title         = element_text(size = 9, margin = margin(b = 2, r = 3)),
      legend.text          = element_text(size = 8),
      legend.key.height    = unit(0.3, "cm"),
      legend.key.width     = unit(0.35, "cm"),
      legend.spacing.x     = unit(1, "mm"),
      legend.spacing.y     = unit(0.5, "mm"),
      legend.spacing       = unit(0.5, "mm"),
      legend.box.margin    = margin(0, 0, 0, 0),
      panel.spacing        = unit(0.5, "mm"),
      plot.margin          = margin(2, 2, 2, 2, unit = "mm")
    )

  p_out <- cowplot::plot_grid(p_out, marine_park_legend(), ncol = 1, rel_heights = c(1, 0.145))

  return(p_out)
}
